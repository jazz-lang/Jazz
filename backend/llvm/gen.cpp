
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/IR/Verifier.h>
#include <iostream>
#include "ast/module.h"
#include "llvm.h"

using namespace jazz;

void ComptimeScope::onScopeEnd() {}

void ComptimeScope::clear() {}

void Scope::onScopeEnd() {
    for (const Expr* expr : llvm::reverse(deferredExprs)) {
        irGenerator->codegenExpr(*expr);
    }

    for (auto& p : llvm::reverse(deinitsToCall)) {
        if (p.decl && p.decl->hasBeenMoved()) continue;
        irGenerator->createDeinitCall(p.function, p.value, p.type, p.decl);
    }
}

void Scope::clear() {
    deferredExprs.clear();
    deinitsToCall.clear();
}

Codegen::Codegen() : builder(ctx) {
    scopes.push_back(Scope(*this));
    functions = llvm::StringMap<const FunctionDecl&>();
}

void Codegen::setLocalValue(Type type, std::string name, llvm::Value* value,
                            const Decl* decl) {
    scopes.back().addLocalValue(std::move(name), value);

    if (type) {
        deferDeinitCall(value, type, decl);
    }
}

void Codegen::setComptimeLocalValue(Type type, std::string name, Value value) {
    comptimeScopes.back().addLocalValue(std::move(name), value);
}

Value* Codegen::findComptimeLocal(llvm::StringRef name, const Decl* decl) {
    Value* value = new Value();
    bool found = false;

    for (auto& scope : llvm::reverse(comptimeScopes)) {
        auto it = scope.getLocalValues().find(name);

        if (it == scope.getLocalValues().end()) continue;
        value = const_cast<Value*>(&it->second);
        found = true;
        break;
    }
    if (found) {
        return value;
    }
    jazz_assert(decl);
    llvm_unreachable("UNIMPLEMENTED");
}

llvm::Value* Codegen::findValue(llvm::StringRef name, const Decl* decl) {
    llvm::Value* value = nullptr;

    for (const auto& scope : llvm::reverse(scopes)) {
        auto it = scope.getLocalValues().find(name);
        if (it == scope.getLocalValues().end()) continue;
        value = it->second;
        break;
    }

    if (value) {
        return value;
    }

    for (auto& scope : llvm::reverse(comptimeScopes)) {
        auto it = scope.getLocalValues().find(name);
        if (it == scope.getLocalValues().end()) continue;
        if (it->second.type.isString() ||
            it->second.type.getName() == "StringRef") {
            value = builder.CreateGlobalStringPtr(*it->second.string);
        } else {
            value =
                llvm::dyn_cast<llvm::Value>(comptimeValueToLLVMVal(it->second));
        }
        break;
    }
    if (value) {
        return value;
    }

    jazz_assert(decl);

    switch (decl->getKind()) {
        case DeclKind::VarDecl:
            return codegenVarDecl(*llvm::cast<VarDecl>(decl));

        case DeclKind::FieldDecl:
            return codegenMemberAccess(findValue("this", nullptr),
                                       llvm::cast<FieldDecl>(decl)->getType(),
                                       llvm::cast<FieldDecl>(decl)->getName());

        case DeclKind::FunctionDecl:
            return getFunctionProto(*llvm::cast<FunctionDecl>(decl));

        default:
            llvm_unreachable("all cases handled");
    }
}

llvm::Type* Codegen::getBuiltinType(llvm::StringRef name) {
    return llvm::StringSwitch<llvm::Type*>(name)
        .Case("void", llvm::Type::getVoidTy(ctx))
        .Case("bool", llvm::Type::getInt1Ty(ctx))
        .Case("char", llvm::Type::getInt8Ty(ctx))
        .Case("int", llvm::Type::getInt32Ty(ctx))
        .Case("int8", llvm::Type::getInt8Ty(ctx))
        .Case("int16", llvm::Type::getInt16Ty(ctx))
        .Case("int32", llvm::Type::getInt32Ty(ctx))
        .Case("int64", llvm::Type::getInt64Ty(ctx))
        .Case("uint", llvm::Type::getInt32Ty(ctx))
        .Case("uint8", llvm::Type::getInt8Ty(ctx))
        .Case("uint16", llvm::Type::getInt16Ty(ctx))
        .Case("uint32", llvm::Type::getInt32Ty(ctx))
        .Case("uint64", llvm::Type::getInt64Ty(ctx))
        .Case("float", llvm::Type::getFloatTy(ctx))
        .Case("float32", llvm::Type::getFloatTy(ctx))
        .Case("float64", llvm::Type::getDoubleTy(ctx))
        .Case("float80", llvm::Type::getX86_FP80Ty(ctx))
        .Default(nullptr);
}

llvm::Type* Codegen::toIR(Type type, Position location) {
    switch (type.getKind()) {
        case TypeKind::BasicType: {
            if (auto* builtinType = getBuiltinType(type.getName()))
                return builtinType;

            auto it = structs.find(type.getRefinedTypeName());
            if (it != structs.end()) return it->second.first;

            auto& basicType = llvm::cast<BasicType>(*type);
            if (auto* enumDecl =
                    llvm::dyn_cast<EnumDecl>(basicType.getDecl())) {
                return toIR(enumDecl->getUnderlyingType());
            }

            return codegenTypeDecl(*basicType.getDecl());
        }
        case TypeKind::ArrayType:

            jazz_assert(type.isArrayWithConstantSize(), "unimplemented array");
            return llvm::ArrayType::get(toIR(type.getElementType(), location),
                                        type.getArraySize());
        case TypeKind::TupleType: {
            auto elementTypes =
                map(type.getTupleElements(), [&](const TupleElement& element) {
                    return toIR(element.type);
                });
            return llvm::StructType::get(ctx, elementTypes);
        }
        case TypeKind::FunctionType: {
            auto paramTypes = map(type.getParamTypes(),
                                  [&](Type type) { return toIR(type); });
            auto* returnType = toIR(type.getReturnType());
            return llvm::FunctionType::get(returnType, paramTypes, false)
                ->getPointerTo();
        }
        case TypeKind::PointerType: {
            if (type.getPointee().isArrayWithRuntimeSize()) {
                return toIR(BasicType::get("ArrayRef",
                                           type.getPointee().getElementType()),
                            location);
            } else if (type.getPointee().isArrayWithUnknownSize()) {
                return llvm::PointerType::get(
                    toIR(type.getPointee().getElementType(), location), 0);
            }

            auto* pointeeType = toIR(type.getPointee(), location);
            return llvm::PointerType::get(pointeeType->isVoidTy()
                                              ? llvm::Type::getInt8Ty(ctx)
                                              : pointeeType,
                                          0);
        }
        case TypeKind::OptionalType:
            if (type.getWrappedType().isPointerType() ||
                type.getWrappedType().isFunctionType()) {
                return toIR(type.getWrappedType());
            }
            llvm_unreachable(
                "IRGen doesn't support non-pointer optional types yet");
    }
    llvm_unreachable("all cases handled");
}

void Codegen::beginScope() {
    scopes.push_back(Scope(*this));
    comptimeScopes.push_back(ComptimeScope(*this));
}

void Codegen::endScope() {
    scopes.back().onScopeEnd();
    comptimeScopes.back().onScopeEnd();
    scopes.pop_back();
    comptimeScopes.pop_back();
}

void Codegen::deferEvaluationOf(const Expr& expr) {
    scopes.back().addDeferredExpr(expr);
}

DeinitDecl* Codegen::getDefaultDeinitializer(const TypeDecl& typeDecl) {
    jazz_assert(typeDecl.getDeinitializer() == nullptr);

    for (auto& field : typeDecl.getFields()) {
        if (field.getType().getDeinitializer() != nullptr) {
            auto deinitializer = llvm::make_unique<DeinitDecl>(
                const_cast<TypeDecl&>(typeDecl), typeDecl.getLocation());
            deinitializer->setBody({});
            helperDecls.push_back(std::move(deinitializer));
            return llvm::cast<DeinitDecl>(helperDecls.back().get());
        }
    }

    return nullptr;
}

void Codegen::deferDeinitCall(llvm::Value* valueToDeinit, Type type,
                              const Decl* decl) {
    llvm::Function* proto = nullptr;

    if (auto* deinitializer = type.getDeinitializer()) {
        proto = getFunctionProto(*deinitializer);
    } else if (auto* typeDecl = type.getDecl()) {
        if (auto deinitializer = getDefaultDeinitializer(*typeDecl)) {
            proto = getFunctionProto(*deinitializer);
        }
    }

    if (proto) {
        scopes.back().addDeinitToCall(proto, valueToDeinit, type, decl);
    }
}

void Codegen::codegenDeferredExprsAndDeinitCallsForReturn() {
    for (auto& scope : llvm::reverse(scopes)) {
        scope.onScopeEnd();
    }
    scopes.back().clear();
}

llvm::AllocaInst* Codegen::createEntryBlockAlloca(Type type, const Decl* decl,
                                                  llvm::Value* arraySize,
                                                  const llvm::Twine& name) {
    auto* insertBlock = builder.GetInsertBlock();
    auto* entryBlock = &insertBlock->getParent()->getEntryBlock();

    if (lastAlloca == llvm::BasicBlock::iterator() ||
        lastAlloca->getParent() != entryBlock) {
        if (entryBlock->empty()) {
            builder.SetInsertPoint(entryBlock);
        } else {
            builder.SetInsertPoint(&entryBlock->front());
        }
    } else {
        builder.SetInsertPoint(entryBlock, std::next(lastAlloca));
    }

    auto* llvmType = toIR(type, decl ? decl->getLocation() : Position());
    auto* alloca = builder.CreateAlloca(llvmType, arraySize, name);
    lastAlloca = alloca->getIterator();
    auto nameString = name.str();
    if (!nameString.empty()) {
        setLocalValue(type, std::move(nameString), alloca, decl);
    }
    builder.SetInsertPoint(insertBlock);
    return alloca;
}

llvm::Value* Codegen::createLoad(llvm::Value* value) {
    return builder.CreateLoad(value, value->getName() + "_load");
}

llvm::Value* Codegen::codegenAssignmentLHS(const Expr* lhs, const Expr* rhs) {
    if (auto* initDecl = llvm::dyn_cast<InitDecl>(currentDecl)) {
        if (auto* varExpr = llvm::dyn_cast<VarExpr>(lhs)) {
            if (auto* fieldDecl =
                    llvm::dyn_cast<FieldDecl>(varExpr->getDecl())) {
                if (fieldDecl->getParent() == initDecl->getTypeDecl()) {
                    return rhs->isUndefinedLiteralExpr()
                               ? nullptr
                               : codegenLvalueExpr(*lhs);
                }
            }
        }
    }

    if (auto* basicType = llvm::dyn_cast<BasicType>(lhs->getType().getBase())) {
        if (auto* typeDecl = basicType->getDecl()) {
            if (auto* deinit = typeDecl->getDeinitializer()) {
                llvm::Value* value = codegenLvalueExpr(*lhs);
                createDeinitCall(getFunctionProto(*deinit), value,
                                 lhs->getType(), typeDecl);
                return rhs->isUndefinedLiteralExpr() ? nullptr : value;
            }
        }
    }

    return rhs->isUndefinedLiteralExpr() ? nullptr : codegenLvalueExpr(*lhs);
}

void Codegen::createDeinitCall(llvm::Function* deinit,
                               llvm::Value* valueToDeinit, Type type,
                               const Decl* decl) {
    if (!valueToDeinit->getType()->isPointerTy()) {
        auto* alloca = createEntryBlockAlloca(type, decl);
        builder.CreateStore(valueToDeinit, alloca);
        valueToDeinit = alloca;
    }

    builder.CreateCall(deinit, valueToDeinit);
}

llvm::Type* Codegen::getLLVMTypeForPassing(const TypeDecl& typeDecl,
                                           bool isMutating) {
    auto* structType = toIR(typeDecl.getType());

    if (!isMutating && typeDecl.passByValue()) {
        return structType;
    } else {
        return llvm::PointerType::get(structType, 0);
    }
}

llvm::Value* Codegen::comptimeValueToLLVMVal(const Value& value) {
    if (value.type.isString() || value.type.getName() == "StringRef") {
        return builder.CreateGlobalStringPtr(*value.string,
                                             "compile_time_string");
    } else if (value.reference) {
        return comptimeValueToLLVMVal(*value.ref);
    } else {
        return comptimeValueToLLVMValue(value);
    }
}

llvm::Constant* Codegen::comptimeValueToLLVMValue(const Value& value) {
    auto irtype = toIR(value.type);
    if (value.type.isInteger() ||
        (value.type.isPointerType() && !value.allocated)) {
        return llvm::ConstantInt::get(irtype, value.i64);
    } else if (value.type.isArrayType() || value.type) {
        std::vector<llvm::Constant*> constants;
        for (auto& val : *value.array) {
            constants.push_back(comptimeValueToLLVMValue(val));
        }
        return llvm::ConstantArray::get(
            llvm::ArrayType::get(toIR(value.type), value.array->size()),
            constants);
    } else if (value.type.isArrayType()) {
        llvm_unreachable("unimplemented");
    } else if (value.type.isBool()) {
        auto boolean = llvm::APInt(1, (int)value.boolean);
        return llvm::ConstantInt::get(irtype, boolean);
    } else if (value.type.isFloat32() || value.type.isFloat()) {
        return llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx), value.f32);
    } else if (value.type.isFloat64()) {
        return llvm::ConstantFP::get(irtype, value.f64);
    } else if (value.allocated && value.type.isPointerType()) {
        printf("%s\n", value.type.getName().str().c_str());
        errorExit(
            "can't convert compile-time allocated pointer to llvm constant");
    }

    return llvm::ConstantFP::get(irtype, value.f64);
}

llvm::Value* Codegen::getFunctionForCall(const CallExpr& call) {
    if (!call.callsNamedFunction()) {
        error(call.getLocation(),
              "anonymous function calls not implemented yet");
    }

    const Decl* decl = call.getCalleeDecl();
    if (!decl) return nullptr;

    switch (decl->getKind()) {
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
        case DeclKind::InitDecl:
        case DeclKind::DeinitDecl:
            return getFunctionProto(*llvm::cast<FunctionDecl>(decl));
        case DeclKind::VarDecl:
            return findValue(llvm::cast<VarDecl>(decl)->getName(), decl);
        case DeclKind::ParamDecl:
            return findValue(llvm::cast<ParamDecl>(decl)->getName(), decl);
        case DeclKind::FieldDecl:
            if (call.getReceiver()) {
                return codegenMemberAccess(
                    codegenLvalueExpr(*call.getReceiver()),
                    llvm::cast<FieldDecl>(decl)->getType(),
                    llvm::cast<FieldDecl>(decl)->getName());
            } else {
                return findValue(llvm::cast<FieldDecl>(decl)->getName(), decl);
            }
        default:
            llvm_unreachable("invalid callee decl");
    }
}

llvm::Module& Codegen::compile(const Module& sourceModule) {
    jazz_assert(!module);

    module = llvm::make_unique<llvm::Module>(sourceModule.getName(), ctx);

    for (const auto& sourceFile : sourceModule.getSourceFiles()) {
        for (const auto& decl : sourceFile.getTopLevelDecls()) {
            codegenDecl(*decl);
        }
    }

    while (true) {
        auto currentFunctionInstantiations = functionInstantiations;

        for (auto& p : currentFunctionInstantiations) {
            if (p.second.getDecl().isExtern() ||
                !p.second.getFunction()->empty())
                continue;

            currentDecl = &p.second.getDecl();
            codegenFunctionBody(p.second.getDecl(), *p.second.getFunction());
            jazz_assert(
                !llvm::verifyFunction(*p.second.getFunction(), &llvm::errs()));
        }

        if (functionInstantiations.size() ==
            currentFunctionInstantiations.size())
            break;
    }

    jazz_assert(!llvm::verifyModule(*module, &llvm::errs()));
    generatedModules.push_back(std::move(module));
    return *generatedModules.back();
}
