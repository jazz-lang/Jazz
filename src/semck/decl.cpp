#include "typecheck.h"
#include <llvm/ADT/SmallPtrSet.h>
#include <iostream>
#include "ast/module.h"
#include "import_c.h"

using namespace jazz;

void Typechecker::inferType(Type type, AccessLevel userAccessLevel) {
    switch (type.getKind()) {
        case TypeKind::BasicType: {
            if (type.isBuiltinType()) {
                validateGenericArgCount(0, type.getGenericArgs(), type.getName(), type.getLocation());
                break;
            }

            auto* basicType = llvm::cast<BasicType>(type.getBase());

            for (auto genericArg : basicType->getGenericArgs()) {
                inferType(genericArg, userAccessLevel);
            }

            auto decls = findDecls(basicType->getRefinedName());
            Decl* decl;

            if (decls.empty()) {
                auto decls = findDecls(basicType->getName());

                if (decls.empty()) {
                    error(type.getLocation(), "unknown type '", type, "'");
                }

                decl = decls[0];
                auto typeTemplate = llvm::cast<TypeTemplate>(decl)->instantiate(basicType->getGenericArgs());
                getCurrentModule()->addToSymbolTable(*typeTemplate);
                inferTypedef(*typeTemplate);
            } else {
                decl = decls[0];

                switch (decl->getKind()) {
                    case DeclKind::TypeDecl:
                        if (auto* deinitDecl = llvm::cast<TypeDecl>(decl)->getDeinitializer()) {
                            typecheckFunction(*deinitDecl);
                        }
                        break;
                    case DeclKind::TypeTemplate:
                        validateGenericArgCount(llvm::cast<TypeTemplate>(decl)->getGenericParams().size(), basicType->getGenericArgs(),
                                                basicType->getName(), type.getLocation());
                        break;
                    default:
                        break;
                }
            }

            checkHasAccess(*decl, type.getLocation(), userAccessLevel);
            break;
        }
        case TypeKind::ArrayType:
            inferType(type.getElementType(), userAccessLevel);
            break;
        case TypeKind::TupleType:
            for (auto& element : type.getTupleElements()) {
                inferType(element.type, userAccessLevel);
            }
            break;
        case TypeKind::FunctionType:
            for (auto paramType : type.getParamTypes()) {
                inferType(paramType, userAccessLevel);
            }
            inferType(type.getReturnType(), userAccessLevel);
            break;
        case TypeKind::PointerType: {
            if (type.getPointee().isArrayWithRuntimeSize()) {
                auto qualifiedTypeName = getRefinedTypeName("ArrayRef", type.getPointee().getElementType());
                if (findDecls(qualifiedTypeName).empty()) {
                    auto& arrayRef = llvm::cast<TypeTemplate>(findDecl("ArrayRef", Position()));
                    auto* instantiation = arrayRef.instantiate({ type.getPointee().getElementType() });
                    getCurrentModule()->addToSymbolTable(*instantiation);
                    declsToTypecheck.push_back(instantiation);
                }
            } else {
                inferType(type.getPointee(), userAccessLevel);
            }
            break;
        }
        case TypeKind::OptionalType:
            inferType(type.getWrappedType(), userAccessLevel);
            break;
    }
}

void Typechecker::typecheckParameter(ParamDecl& decl, AccessLevel userAccessLevel) {
    if (getCurrentModule()->getSymbolTable().containsInCurrentScope(decl.getName())) {
        
        if (decl.isMethodDecl() || decl.isFunctionDecl()) {
            if (llvm::cast<MethodDecl>(decl).isOverriding()) {

            } else {
                error(decl.getLocation(), "redefinition of '", decl.getName(), "'");
            }
        } else {
            
            error(decl.getLocation(), "redefinition of '", decl.getName(), "'");
        }
    }

    if (decl.getType().isMutable()) {
        error(decl.getLocation(), "parameter types cannot be 'mutable'");
    }

    inferType(decl.getType(), userAccessLevel);
    getCurrentModule()->getSymbolTable().add(decl.getName(), &decl);
}

static bool returns(llvm::ArrayRef<std::unique_ptr<Stmt>> block) {
    if (block.empty()) return false;

    switch (block.back()->getKind()) {
        case StmtKind::ReturnStmt:
            return true;
        case StmtKind::IfStmt: {
            auto& ifStmt = llvm::cast<IfStmt>(*block.back());
            return returns(ifStmt.getThenBody()) && returns(ifStmt.getElseBody());
        }
        case StmtKind::SwitchStmt: {
            auto& switchStmt = llvm::cast<SwitchStmt>(*block.back());
            return llvm::all_of(switchStmt.getCases(), [](auto& c) { return returns(c.getStmts()); }) &&
                   returns(switchStmt.getDefaultStmts());
        }
        default:
            return false;
    }
}

void Typechecker::typecheckGenericParamDecls(llvm::ArrayRef<GenericParamDecl> genericParams, AccessLevel userAccessLevel) {
    for (auto& genericParam : genericParams) {
        if (getCurrentModule()->getSymbolTable().contains(genericParam.getName())) {
            error(genericParam.getLocation(), "redefinition of '", genericParam.getName(), "'");
        }

        for (Type constraint : genericParam.getConstraints()) {
            inferType(constraint, userAccessLevel);
        }
    }
}

void Typechecker::typecheckParameters(llvm::MutableArrayRef<ParamDecl> params, AccessLevel userAccessLevel) {
    for (auto& param : params) {
        typecheckParameter(param, userAccessLevel);
    }
}

void Typechecker::typecheckFunction(FunctionDecl& decl) {
    if (decl.isTypechecked()) return;
    if (decl.isExtern()) return;
    TypeDecl* receiverTypeDecl = decl.getTypeDecl();

    getCurrentModule()->getSymbolTable().pushScope();
    _SAVE_(currentFunction);
    currentFunction = &decl;

    typecheckParameters(decl.getParams(), decl.getAccessLevel());

    if (!decl.isInitDecl() && !decl.isDeinitDecl()) {
        inferType(decl.getReturnType(), decl.getAccessLevel());
        if (decl.getReturnType().isMutable()) error(decl.getLocation(), "return types cannot be 'mutable'");
    }

    if (!decl.isExtern()) {
        _SAVE_(functionReturnType);
        functionReturnType = decl.getReturnType();

        llvm::SmallPtrSet<FieldDecl*, 32> initializedFields;
        _SAVE_(onAssign);
        onAssign = [&](Expr& lhs) { initializedFields.insert(lhs.getFieldDecl()); };

        if (receiverTypeDecl) {
            Type thisType = receiverTypeDecl->getTypeForPassing(decl.isMutating());
            getCurrentModule()->addToSymbolTable(VarDecl(thisType, "this", nullptr, &decl, AccessLevel::None, *getCurrentModule(), decl.getLocation()));
        }

        bool delegatedInit = false;

        if (decl.hasBody()) {
            for (auto& stmt : decl.getBody()) {
                typecheckStmt(stmt);

                if (!decl.isInitDecl()) continue;

                if (auto* exprStmt = llvm::dyn_cast<ExprStmt>(stmt.get())) {
                    if (auto* callExpr = llvm::dyn_cast<CallExpr>(&exprStmt->getExpr())) {
                        if (auto* initDecl = llvm::dyn_cast_or_null<InitDecl>(callExpr->getCalleeDecl())) {
                            if (initDecl->getTypeDecl() == receiverTypeDecl) {
                                delegatedInit = true;
                            }
                        }
                    }
                }
            }
        }

        if (decl.isInitDecl() && !delegatedInit) {
            for (auto& field : decl.getTypeDecl()->getFields()) {
                if (initializedFields.count(&field) == 0) {
                    warning(decl.getLocation(), "initializer doesn't initialize member variable '", field.getName(), "'");
                }
            }
        }
    }

    getCurrentModule()->getSymbolTable().popScope();

    if ((!receiverTypeDecl || !receiverTypeDecl->isInterface()) && !decl.getReturnType().isVoid() && !returns(decl.getBody())) {
        error(decl.getLocation(), "'", decl.getName(), "' is missing a return statement");
    }

    decl.setTypechecked(true);
}

void Typechecker::typecheckFunctionProto(FunctionTemplate& decl) {
    typecheckGenericParamDecls(decl.getGenericParams(), decl.getAccessLevel());
}

void Typechecker::inferTypedef(TypeDecl& decl) {
    for (Type interface : decl.getInterfaces()) {
        if (interface.isBasicType()) {
            if (auto* interfaceDecl = getTypeDecl(llvm::cast<BasicType>(*interface))) {
                if (interfaceDecl->isInterface()) {
                    for (const auto& interfaceOfInterface : interfaceDecl->getInterfaces()) {
                        if (decl.hasInterface(*interfaceOfInterface.getDecl())) {
                            continue;
                        } else {
                            error(decl.getLocation(),"'",decl.getName(),"' doesn't implements '",interfaceOfInterface.getName(),"' required by '",interface.getName(),"'");
                        }
                    }
                    std::string errorReason;
                    if (!providesInterfaceRequirements(decl, *interfaceDecl, &errorReason)) {
                        error(decl.getLocation(), "'", decl.getName(), "' ", errorReason, " required by interface '",
                              interfaceDecl->getName(), "'");
                    }

                    for (auto& method : interfaceDecl->getMethods()) {
                        auto& methodDecl = llvm::cast<MethodDecl>(*method);
                        
                        if (methodDecl.hasBody()) {
                            auto copy = methodDecl.instantiate({ { "This", decl.getType() } }, decl);
                            getCurrentModule()->addToSymbolTable(*copy);
                            decl.addMethod(move(copy));
                        }
                    }

                    continue;
                }
            }
        }
        error(decl.getLocation(), "'", interface, "' is not an interface");
    }

    TypeDecl* realDecl;
    std::unique_ptr<TypeDecl> thisTypeResolved;

    if (decl.isInterface()) {
        thisTypeResolved = llvm::cast<TypeDecl>(decl.instantiate({ { "This", decl.getType() } }, {}));
        realDecl = thisTypeResolved.get();
    } else {
        realDecl = &decl;
    }

    for (auto& fieldDecl : realDecl->getFields()) {
        typecheckField(fieldDecl);
    }

    for (auto& memberDecl : realDecl->getMemberDecls()) {
        if (memberDecl->isMethodDecl()) {
            if (llvm::cast<MethodDecl>(memberDecl).isOverriding()) {
                
            }
        }
        typecheckMemberDecl(*memberDecl);
    }
}

void Typechecker::inferTypeTemplate(TypeTemplate& decl) {
    typecheckGenericParamDecls(decl.getGenericParams(), decl.getAccessLevel());
}

void Typechecker::typecheckEnum(EnumDecl& decl) {
    std::vector<const EnumCase*> cases = map(decl.getCases(), [](const EnumCase& c) { return &c; });
    std::sort(cases.begin(), cases.end(), [](auto* a, auto* b) { return a->getName() < b->getName(); });
    auto it = std::adjacent_find(cases.begin(), cases.end(), [](auto* a, auto* b) { return a->getName() == b->getName(); });

    if (it != cases.end()) {
        error((*it)->getLocation(), "duplicate enum case '", (*it)->getName(), "'");
    }

    for (auto& enumCase : decl.getCases()) {
        typecheckExpr(*enumCase.getValue());
    }
}

void Typechecker::typecheckVar(VarDecl& decl, bool isGlobal) {
    if (!isGlobal && getCurrentModule()->getSymbolTable().contains(decl.getName())) {
        error(decl.getLocation(), "redefinition of '", decl.getName(), "'");
    }
    if (isGlobal && decl.getInitializer()->isUndefinedLiteralExpr()) {
        error(decl.getLocation(), "global variables cannot be uninitialized");
    }

    Type declaredType = decl.getType();
    Type initType = typecheckExpr(*decl.getInitializer());

    if (declaredType) {
        bool isLocalVariable = decl.getParent() && decl.getParent()->isFunctionDecl();
        inferType(declaredType, isLocalVariable ? AccessLevel::None : decl.getAccessLevel());

        if (initType) {
            Type convertedType;

            if (isImplicitlyConvertible(decl.getInitializer(), initType, declaredType, &convertedType)) {
                decl.getInitializer()->setType(convertedType ? convertedType : initType);
            } else {
                const char* hint;

                if (initType.isNull()) {
                    jazz_assert(!declaredType.isOptionalType());
                    hint = " (add '?' to the type to make it nullable)";
                } else {
                    hint = "";
                }

                error(decl.getInitializer()->getLocation(), "cannot initialize variable of type '", declaredType, "' with '", initType, "'", hint);
            }
        }
    } else {
        if (initType.isNull()) {
            error(decl.getLocation(), "couldn't infer type of '", decl.getName(), "', add a type annotation");
        }

        initType.setMutable(decl.getType().isMutable());
        decl.setType(initType);
    }

    if (!isGlobal) {
        getCurrentModule()->addToSymbolTable(decl, false);
    }

    if (!decl.getType().isImplicitlyCopy()) {
        decl.getInitializer()->setMoved(true,decl.getLocation());
    }
}

void Typechecker::typecheckField(FieldDecl& decl) {
    inferType(decl.getType(), std::min(decl.getAccessLevel(), decl.getParent()->getAccessLevel()));
}

void Typechecker::typecheckImport(ImportDecl& decl, const PackageManifest* manifest, llvm::ArrayRef<std::string> importPaths,
                                      llvm::ArrayRef<std::string> frameworkSearchPaths) {
    if (importJazzModule(currentSourceFile, manifest, importPaths, frameworkSearchPaths, decl.getTarget())) {
        return;
    }

    if (!includeC(*currentSourceFile,decl.getTarget(),importPaths,frameworkSearchPaths)) {
        error(decl.getLocation(),"couldn't find module or C/C++ header '",decl.getTarget(),"'");
    }
}

void Typechecker::typecheckDecl(Decl& decl, const PackageManifest* manifest, llvm::ArrayRef<std::string> importPaths,
                                        llvm::ArrayRef<std::string> frameworkSearchPaths) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl:
            llvm_unreachable("no top-level parameter declarations");
        case DeclKind::FunctionDecl:
            typecheckFunction(llvm::cast<FunctionDecl>(decl));
            break;
        case DeclKind::MethodDecl:
            llvm_unreachable("no top-level method declarations");
        case DeclKind::GenericParamDecl:
            llvm_unreachable("no top-level parameter declarations");
        case DeclKind::InitDecl:
            llvm_unreachable("no top-level initializer declarations");
        case DeclKind::DeinitDecl:
            llvm_unreachable("no top-level deinitializer declarations");
        case DeclKind::FunctionTemplate:
            typecheckFunctionProto(llvm::cast<FunctionTemplate>(decl));
            break;
        case DeclKind::TypeDecl:
            inferTypedef(llvm::cast<TypeDecl>(decl));
            break;
        case DeclKind::TypeTemplate:
            inferTypeTemplate(llvm::cast<TypeTemplate>(decl));
            break;
        case DeclKind::EnumDecl:
            typecheckEnum(llvm::cast<EnumDecl>(decl));
            break;
        case DeclKind::VarDecl:
            typecheckVar(llvm::cast<VarDecl>(decl), true);
            break;
        case DeclKind::FieldDecl:
            llvm_unreachable("no top-level field declarations");
        case DeclKind::ImportDecl:
            typecheckImport(llvm::cast<ImportDecl>(decl), manifest, importPaths, frameworkSearchPaths);
            break;
        case DeclKind::LinkDecl: 
            break;
    }
}

void Typechecker::typecheckMemberDecl(Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::MethodDecl:
        case DeclKind::InitDecl:
        case DeclKind::DeinitDecl:
            typecheckFunction(llvm::cast<MethodDecl>(decl));
            break;
        case DeclKind::FieldDecl:
            typecheckField(llvm::cast<FieldDecl>(decl));
            break;
        case DeclKind::FunctionTemplate:
            typecheckFunctionProto(llvm::cast<FunctionTemplate>(decl));
            break;
        default:
            llvm_unreachable("invalid member declaration kind");
    }
}
