#include "import_c.h"
#include <cstdio>
#include <cstdlib>
#include <memory>
#include <string>
#include <vector>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclGroup.h>
#include <clang/AST/PrettyPrinter.h>
#include <clang/AST/Type.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Lex/HeaderSearch.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Parse/ParseAST.h>
#include <clang/Sema/Sema.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/Program.h>
#include "typecheck.h"
#include "ast/decl.h"
#include "ast/module.h"
#include "ast/type.h"
#include "utils.h"

using namespace jazz;

namespace {

clang::TargetInfo* targetInfo;

Type getIntTypeByWidth(int widthInBits, bool asSigned) {
    switch (widthInBits) {
        case 8:
            return asSigned ? Type::getInt8() : Type::getUInt8();
        case 16:
            return asSigned ? Type::getInt16() : Type::getUInt16();
        case 32:
            return asSigned ? Type::getInt32() : Type::getUInt32();
        case 64:
            return asSigned ? Type::getInt64() : Type::getUInt64();
    }
    llvm_unreachable("unsupported integer width");
}

Type toJazz(const clang::BuiltinType& type) {
    switch (type.getKind()) {
        case clang::BuiltinType::Void:
            return Type::getVoid();
        case clang::BuiltinType::Bool:
            return Type::getBool();
        case clang::BuiltinType::Char_S:
        case clang::BuiltinType::Char_U:
            return Type::getChar();
        case clang::BuiltinType::SChar:
            return getIntTypeByWidth(targetInfo->getCharWidth(), true);
        case clang::BuiltinType::UChar:
            return getIntTypeByWidth(targetInfo->getCharWidth(), false);
        case clang::BuiltinType::Short:
            return getIntTypeByWidth(targetInfo->getShortWidth(), true);
        case clang::BuiltinType::UShort:
            return getIntTypeByWidth(targetInfo->getShortWidth(), false);
        case clang::BuiltinType::Int:
            return Type::getInt();
        case clang::BuiltinType::UInt:
            return Type::getUInt();
        case clang::BuiltinType::Long:
            return getIntTypeByWidth(targetInfo->getLongWidth(), true);
        case clang::BuiltinType::ULong:
            return getIntTypeByWidth(targetInfo->getLongWidth(), false);
        case clang::BuiltinType::LongLong:
            return getIntTypeByWidth(targetInfo->getLongLongWidth(), true);
        case clang::BuiltinType::ULongLong:
            return getIntTypeByWidth(targetInfo->getLongLongWidth(), false);
        case clang::BuiltinType::Float:
            return Type::getFloat32();
        case clang::BuiltinType::Double:
            return Type::getFloat64();
        case clang::BuiltinType::LongDouble:
            return Type::getFloat80();
        default:
            break;
    }
    llvm_unreachable("unsupported builtin type");
}

llvm::StringRef getRecordName(const clang::RecordDecl& recordDecl) {
    if (!recordDecl.getName().empty()) {
        return recordDecl.getName();
    } else if (auto* typedefNameDecl = recordDecl.getTypedefNameForAnonDecl()) {
        return typedefNameDecl->getName();
    }
    return "";
}

Type toJazz(clang::QualType qualtype) {
    const bool isMutable = !qualtype.isConstQualified();
    auto& type = *qualtype.getTypePtr();
    switch (type.getTypeClass()) {
        case clang::Type::Pointer: {
            auto pointeeType = llvm::cast<clang::PointerType>(type).getPointeeType();
            if (pointeeType->isFunctionType()) {
                return OptionalType::get(toJazz(pointeeType), isMutable);
            }
            return OptionalType::get(PointerType::get(toJazz(pointeeType), false), isMutable);
        }
        case clang::Type::Builtin: {
            Type jazzType = toJazz(llvm::cast<clang::BuiltinType>(type));
            jazzType.setMutable(isMutable);
            return jazzType;
        }
        case clang::Type::Typedef: {
            auto desugared = llvm::cast<clang::TypedefType>(type).desugar();
            if (!isMutable) desugared.addConst();
            return toJazz(desugared);
        }
        case clang::Type::Elaborated:
            return toJazz(llvm::cast<clang::ElaboratedType>(type).getNamedType());
        case clang::Type::Record: {
            auto* recordDecl = llvm::cast<clang::RecordType>(type).getDecl();
            return BasicType::get(getRecordName(*recordDecl), {}, isMutable);
        }
        case clang::Type::Paren:
            return toJazz(llvm::cast<clang::ParenType>(type).getInnerType());
        case clang::Type::FunctionProto: {
            auto& functionProtoType = llvm::cast<clang::FunctionProtoType>(type);
            auto paramTypes = map(functionProtoType.getParamTypes(), [](clang::QualType qualType) { return toJazz(qualType).asImmutable(); });
            return FunctionType::get(toJazz(functionProtoType.getReturnType()).asImmutable(), std::move(paramTypes), isMutable);
        }
        case clang::Type::FunctionNoProto: {
            auto& functionNoProtoType = llvm::cast<clang::FunctionNoProtoType>(type);
            return FunctionType::get(toJazz(functionNoProtoType.getReturnType()).asImmutable(), {}, isMutable);
        }
        case clang::Type::ConstantArray: {
            auto& constantArrayType = llvm::cast<clang::ConstantArrayType>(type);
            if (!constantArrayType.getSize().isIntN(64)) {
                error(Position(), "array is too large");
            }
            return ArrayType::get(toJazz(constantArrayType.getElementType()), constantArrayType.getSize().getLimitedValue(), isMutable);
        }
        case clang::Type::IncompleteArray:
            return ArrayType::get(toJazz(llvm::cast<clang::IncompleteArrayType>(type).getElementType()), ArrayType::unknownSize);
        case clang::Type::Attributed:
            return toJazz(llvm::cast<clang::AttributedType>(type).getEquivalentType());
        case clang::Type::Decayed:
            return toJazz(llvm::cast<clang::DecayedType>(type).getDecayedType());
        case clang::Type::Enum:
            return toJazz(llvm::cast<clang::EnumType>(type).getDecl()->getIntegerType());
        case clang::Type::Vector:
            return Type::getInt();
        default:
            error(Position(), "unhandled type class '", type.getTypeClassName(), "' (importing type '", qualtype.getAsString(), "')");
    }
}

FunctionDecl toJazz(const clang::FunctionDecl& decl, Module* currentModule) {
    auto params = map(decl.parameters(), [](clang::ParmVarDecl* param) {
        return ParamDecl(toJazz(param->getType()), param->getNameAsString(), Position());
    });
    
    
    FunctionProto proto(decl.getNameAsString(), std::move(params), toJazz(decl.getReturnType()), decl.isVariadic(), true);
    auto fdecl =  FunctionDecl(std::move(proto), {}, AccessLevel::Default, *currentModule, Position());
    fdecl.should_inline = decl.isInlined();
    return fdecl;
}

llvm::Optional<FieldDecl> toJazz(const clang::FieldDecl& decl, TypeDecl& typeDecl) {
    if (decl.getName().empty()) return llvm::None;
    return FieldDecl(toJazz(decl.getType()), decl.getNameAsString(), typeDecl, AccessLevel::Default, Position());
}

llvm::Optional<TypeDecl> toJazz(const clang::RecordDecl& decl, Module* currentModule) {
    auto tag = decl.isUnion() ? TypeTag::Union : TypeTag::Struct;
    TypeDecl typeDecl(tag, getRecordName(decl), {}, {}, AccessLevel::Default, *currentModule, Position());

    for (auto* field : decl.fields()) {
        if (auto fieldDecl = toJazz(*field, typeDecl)) {
            typeDecl.getFields().emplace_back(std::move(*fieldDecl));
        } else {
            return llvm::None;
        }
    }
    return std::move(typeDecl);
}

VarDecl toJazz(const clang::VarDecl& decl, Module* currentModule) {
    return VarDecl(toJazz(decl.getType()), decl.getName(), nullptr, nullptr, AccessLevel::Default, *currentModule, Position());
}

void addIntegerConstantToSymbolTable(llvm::StringRef name, llvm::APSInt value, clang::QualType qualType, Module& module) {
    auto initializer = llvm::make_unique<IntLiteralExpr>(std::move(value), Position());
    auto type = toJazz(qualType).asImmutable();
    initializer->setType(type);
    module.addToSymbolTable(VarDecl(type, name, std::move(initializer), nullptr, AccessLevel::Default, module, Position()));
}

void addFloatConstantToSymbolTable(llvm::StringRef name, long double value, Module& module) {
    auto initializer = llvm::make_unique<FloatLiteralExpr>(value, Position());
    auto type = Type::getFloat64();
    initializer->setType(type);
    module.addToSymbolTable(VarDecl(type, name, std::move(initializer), nullptr, AccessLevel::Default, module, Position()));
}

class CToJazzConverter : public clang::ASTConsumer {
public:
    CToJazzConverter(Module& module) : module(module) {}

    bool HandleTopLevelDecl(clang::DeclGroupRef declGroup) final override {
        for (clang::Decl* decl : declGroup) {
            switch (decl->getKind()) {
                case clang::Decl::Function:
                    module.addToSymbolTable(toJazz(llvm::cast<clang::FunctionDecl>(*decl), &module));
                    break;
                case clang::Decl::Record: {
                    if (!decl->isFirstDecl()) break;
                    auto typeDecl = toJazz(llvm::cast<clang::RecordDecl>(*decl), &module);
                    if (typeDecl) {
                        if (module.getSymbolTable().find(typeDecl->getName()).empty()) {
                            module.addToSymbolTable(std::move(*typeDecl));
                        }
                    }
                    break;
                }
                case clang::Decl::Enum: {
                    auto& enumDecl = llvm::cast<clang::EnumDecl>(*decl);
                    for (auto* enumerator : enumDecl.enumerators()) {
                        auto& value = enumerator->getInitVal();
                        addIntegerConstantToSymbolTable(enumerator->getName(), value, enumDecl.getIntegerType(), module);
                    }
                    break;
                }
                case clang::Decl::Var:
                    module.addToSymbolTable(toJazz(llvm::cast<clang::VarDecl>(*decl), &module));
                    break;
                case clang::Decl::Typedef: {
                    auto& typedefDecl = llvm::cast<clang::TypedefDecl>(*decl);
                    if (auto* baseTypeId = typedefDecl.getUnderlyingType().getBaseTypeIdentifier()) {
                        module.addIdentifierReplacement(typedefDecl.getName(), baseTypeId->getName());
                    }
                    break;
                }

                default:
                    break;
            }
        }
        return true; 
    }

private:
    Module& module;
};

class MacroImporter : public clang::PPCallbacks {
public:
    MacroImporter(Module& module, clang::Sema& clangSema) : module(module), clangSema(clangSema) {}

    void MacroDefined(const clang::Token& name, const clang::MacroDirective* macro) final override {
        if (macro->getMacroInfo()->getNumTokens() != 1) return;
        auto& token = macro->getMacroInfo()->getReplacementToken(0);

        switch (token.getKind()) {
            case clang::tok::identifier:
                module.addIdentifierReplacement(name.getIdentifierInfo()->getName(), token.getIdentifierInfo()->getName());
                return;

            case clang::tok::numeric_constant:
                importNumericConstant(name.getIdentifierInfo()->getName(), token);
                return;

            default:
                return;
        }
    }

private:
    void importNumericConstant(llvm::StringRef name, const clang::Token& token) {
        auto result = clangSema.ActOnNumericConstant(token);
        if (!result.isUsable()) return;
        clang::Expr* parsed = result.get();

        if (auto* intLiteral = llvm::dyn_cast<clang::IntegerLiteral>(parsed)) {
            llvm::APSInt value(intLiteral->getValue(), parsed->getType()->isUnsignedIntegerType());
            addIntegerConstantToSymbolTable(name, std::move(value), parsed->getType(), module);
        } else if (auto* floatLiteral = llvm::dyn_cast<clang::FloatingLiteral>(parsed)) {
            auto value = floatLiteral->getValueAsApproximateDouble();
            addFloatConstantToSymbolTable(name, value, module);
        }
    }

private:
    Module& module;
    clang::Sema& clangSema;
};

} // namespace

bool jazz::includeC(SourceFile& importer, llvm::StringRef headerName, llvm::ArrayRef<std::string> importPaths,
                          llvm::ArrayRef<std::string> frameworkSearchPaths) {
    auto it = Module::getAllImportedModulesMap().find(headerName);
    if (it != Module::getAllImportedModulesMap().end()) {
        importer.addImportedModule(it->second);
        return true;
    }

    auto module = std::make_shared<Module>(headerName);

    clang::CompilerInstance instance;
    clang::DiagnosticOptions diagnosticOptions;
    instance.createDiagnostics();

    std::shared_ptr<clang::TargetOptions> pto = std::make_shared<clang::TargetOptions>();
    pto->Triple = llvm::sys::getDefaultTargetTriple();
    targetInfo = clang::TargetInfo::CreateTargetInfo(instance.getDiagnostics(), pto);
    instance.setTarget(targetInfo);

    instance.createFileManager();
    instance.createSourceManager(instance.getFileManager());

    for (llvm::StringRef includePath : importPaths) {
        instance.getHeaderSearchOpts().AddPath(includePath, clang::frontend::System, false, false);
    }
    for (llvm::StringRef frameworkPath : frameworkSearchPaths) {
        instance.getHeaderSearchOpts().AddPath(frameworkPath, clang::frontend::System, true, false);
    }

    instance.createPreprocessor(clang::TU_Complete);
    auto& pp = instance.getPreprocessor();
    pp.getBuiltinInfo().initializeBuiltins(pp.getIdentifierTable(), pp.getLangOpts());

    instance.setASTConsumer(llvm::make_unique<CToJazzConverter>(*module));
    instance.createASTContext();
    instance.createSema(clang::TU_Complete, nullptr);
    pp.addPPCallbacks(llvm::make_unique<MacroImporter>(*module, instance.getSema()));

    const clang::DirectoryLookup* curDir = nullptr;
    const clang::FileEntry* fileEntry = instance.getPreprocessor().getHeaderSearchInfo().LookupFile(headerName, {}, false, nullptr, curDir, {},
                                                                                              nullptr, nullptr, nullptr, nullptr, nullptr);
    if (!fileEntry) return false;

    auto fileID = instance.getSourceManager().createFileID(fileEntry, clang::SourceLocation(), clang::SrcMgr::C_System);
    instance.getSourceManager().setMainFileID(fileID);
    instance.getDiagnosticClient().BeginSourceFile(instance.getLangOpts(), &instance.getPreprocessor());
    clang::ParseAST(instance.getPreprocessor(), &instance.getASTConsumer(), instance.getASTContext());
    instance.getDiagnosticClient().EndSourceFile();

    importer.addImportedModule(module);
    Module::getAllImportedModulesMap()[module->getName()] = module;
    return true;
}
