#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/Support/ErrorOr.h>

#include "ast/decl.h"
#include "ast/expr.h"
#include "ast/stmt.h"

namespace llvm {
class StringRef;
template<typename T>
class ArrayRef;
template<typename T, unsigned N>
class SmallVector;
} // namespace llvm

namespace jazz {

class Module;
class PackageManifest;
class SourceFile;
struct Position;
struct Type;

class Typechecker {
public:
    Typechecker(std::vector<std::string>&& disabledWarnings,bool no_std = false)
    : currentModule(nullptr), currentSourceFile(nullptr), currentFunction(nullptr), isPostProcessing(false),
      disabledWarnings(std::move(disabledWarnings)),no_std(no_std) {}

    Module* getCurrentModule() const { return NOTNULL(currentModule); }
    void setCurrentModule(Module* module) { currentModule = module; }
    const SourceFile* getCurrentSourceFile() const { return currentSourceFile; }

    void typecheckModule(Module& module, const PackageManifest* manifest, llvm::ArrayRef<std::string> importPaths,
                         llvm::ArrayRef<std::string> frameworkSearchPaths);
    Type typecheckExpr(Expr& expr, bool useIsWriteOnly = false);
    void typecheckVar(VarDecl& decl, bool isGlobal);
    void typecheckField(FieldDecl& decl);
    void typecheckDecl(Decl& decl, const PackageManifest* manifest, llvm::ArrayRef<std::string> importPaths,
                               llvm::ArrayRef<std::string> frameworkSearchPaths);
    void postProcess();

private:
    void typecheckParameters(llvm::MutableArrayRef<ParamDecl> params, AccessLevel userAccessLevel);
    void typecheckFunction(FunctionDecl& decl);
    void typecheckFunctionProto(FunctionTemplate& decl);
    void typecheckMemberDecl(Decl& decl);

    void typecheckStmt(std::unique_ptr<Stmt>& stmt);
    void typecheckCompoundStmt(CompoundStmt& stmt);
    void typecheckReturnStmt(ReturnStmt& stmt);
    void typecheckVarStmt(VarStmt& stmt);
    void typecheckIfStmt(IfStmt& ifStmt);
    void typecheckSwitchStmt(SwitchStmt& stmt);
    void typecheckWhileStmt(WhileStmt& whileStmt);
    void typecheckBreakStmt(BreakStmt& breakStmt);
    void typecheckContinueStmt(ContinueStmt& continueStmt);
    void inferType(Type type, AccessLevel userAccessLevel);
    void typecheckParameter(ParamDecl& decl, AccessLevel userAccessLevel);
    void typecheckGenericParamDecls(llvm::ArrayRef<GenericParamDecl> genericParams, AccessLevel userAccessLevel);
    void inferTypedef(TypeDecl& decl);
    void inferTypeTemplate(TypeTemplate& decl);
    void typecheckEnum(EnumDecl& decl);
    void typecheckImport(ImportDecl& decl, const PackageManifest* manifest, llvm::ArrayRef<std::string> importPaths,
                             llvm::ArrayRef<std::string> frameworkSearchPaths);

    Type typecheckVarExpr(VarExpr& expr, bool useIsWriteOnly);
    Type typecheckArrayLiteralExpr(ArrayLiteralExpr& expr);
    Type typecheckTupleExpr(TupleExpr& expr);
    Type typecheckUnaryExpr(UnaryExpr& expr);
    Type typecheckBinaryExpr(BinaryExpr& expr);
    Type typecheckBinaryExpr(BinaryExpr& expr, Token::Kind op);
    void typecheckAssignment(Expr& lhs, Expr* rhs, Type rightType, Position location);
    Type typecheckCallExpr(CallExpr& expr);
    Type typecheckConversion(CallExpr& expr);
    Type typecheckCast(CallExpr& expr);
    Type typecheckSizeofExpr(SizeofExpr& expr);
    Type typecheckAddressofExpr(AddressofExpr& expr);
    Type typecheckMemberExpr(MemberExpr& expr);
    Type getField(Type& type,std::string name,Type& baseType,MemberExpr& expr);
    Type typecheckSubscriptExpr(SubscriptExpr& expr);
    Type typecheckUnwrapExpr(UnwrapExpr& expr);
    Type typecheckLambdaExpr(LambdaExpr& expr);
    Type typecheckIfExpr(IfExpr& expr);

    bool isInterface(Type type);
    bool hasMethod(TypeDecl& type, FunctionDecl& functionDecl) const;
    bool providesInterfaceRequirements(TypeDecl& type, TypeDecl& interface, std::string* errorReason) const;
    bool isImplicitlyConvertible(const Expr* expr, Type source, Type target, Type* convertedType) const;
    llvm::StringMap<Type> getGenericArgsForCall(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call,
                                                llvm::ArrayRef<ParamDecl> params, bool returnOnError);
    Decl& findDecl(llvm::StringRef name, Position location) const;
    std::vector<Decl*> findDecls(llvm::StringRef name, TypeDecl* receiverTypeDecl = nullptr, bool inAllImportedModules = false) const;
    std::vector<Decl*> findCalleeCandidates(const CallExpr& expr, llvm::StringRef callee);
    Decl* resolveOverload(llvm::ArrayRef<Decl*> decls, CallExpr& expr, llvm::StringRef callee, bool returnNullOnError = false);
    std::vector<Type> inferGenericArgs(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call, llvm::ArrayRef<ParamDecl> params, bool returnOnError);
    bool argumentsMatch(const CallExpr& expr, const FunctionDecl* functionDecl, llvm::ArrayRef<ParamDecl> params = {}) const;
    void validateArgs(CallExpr& expr, const Decl& calleeDecl, llvm::StringRef functionName = "", Position location = Position()) const;
    void validateArgs(CallExpr& expr, bool isMutating, llvm::ArrayRef<ParamDecl> params, bool isVariadic, llvm::StringRef functionName = "",
                      Position location = Position()) const;
    TypeDecl* getTypeDecl(const BasicType& type);
    void checkReturnPointerToLocal(const ReturnStmt& stmt) const;
    static void checkHasAccess(const Decl& decl, Position location, AccessLevel userAccessLevel);

    llvm::ErrorOr<const Module&> importJazzModule(SourceFile* importer, const PackageManifest* manifest, llvm::ArrayRef<std::string> importPaths,
                                                  llvm::ArrayRef<std::string> frameworkSearchPaths, llvm::StringRef moduleName);


    bool isGuaranteedNonNull(const Expr& expr) const;
    bool isGuaranteedNonNull(const Expr& expr, const Stmt& currentControlStmt) const;

    llvm::Optional<bool> maySetToNullBeforeEvaluating(const Expr& var, const Stmt& stmt) const;
    llvm::Optional<bool> subExprMaySetToNullBeforeEvaluating(const Expr& var, const Expr& expr) const;
    llvm::Optional<bool> maySetToNullBeforeEvaluating(const Expr& var, const Expr& expr) const;
    llvm::Optional<bool> maySetToNullBeforeEvaluating(const Expr& var, llvm::ArrayRef<std::unique_ptr<Stmt>> block) const;
    void scopeEnd();
    bool isWarningEnabled(llvm::StringRef warning) const;

private:
    std::function<void(Expr&)> onAssign;
    Module* currentModule;
    SourceFile* currentSourceFile;
    FunctionDecl* currentFunction;
    std::vector<Stmt*> currentControlStmts;
    Type functionReturnType;
    bool isPostProcessing;
    std::vector<Expr*> movedValues;
    std::vector<Decl*> declsToTypecheck;
    std::vector<std::string> disabledWarnings;
    bool no_std;
};

void validateGenericArgCount(size_t genericParamCount, llvm::ArrayRef<Type> genericArgs, llvm::StringRef name, Position location);

} // namespace jazz
