#pragma once

#include <memory>
#include <ostream>

namespace llvm {
template<typename T>
class ArrayRef;
}

namespace jazz {

class Module;
class VarExpr;
class StringLiteralExpr;
class CharacterLiteralExpr;
class IntLiteralExpr;
class FloatLiteralExpr;
class BoolLiteralExpr;
class NullLiteralExpr;
class UndefinedLiteralExpr;
class ArrayLiteralExpr;
class TupleExpr;
class UnaryExpr;
class BinaryExpr;
class CallExpr;
class SizeofExpr;
class AddressofExpr;
class MemberExpr;
class SubscriptExpr;
class UnwrapExpr;
class LambdaExpr;
class IfExpr;
class Expr;
class stream;
class ReturnStmt;
class VarStmt;
class DeferStmt;
class IfStmt;
class SwitchStmt;
class WhileStmt;
class ForStmt;
class BreakStmt;
class ContinueStmt;
class CompoundStmt;
class Stmt;
class ParamDecl;
class GenericParamDecl;
class FunctionDecl;
class InitDecl;
class DeinitDecl;
class FunctionTemplate;
class FieldDecl;
class TypeDecl;
class TypeTemplate;
class VarDecl;
class ImportDecl;
class Decl;

class ASTDumper {
public:
    ASTDumper(std::ostream& out) : out(out), indentLevel(0) {}
    void dumpModule(const Module& module);
    void dumpVarExpr(const VarExpr& expr);
    void dumpStringLiteralExpr(const StringLiteralExpr& expr);
    void dumpCharacterLiteralExpr(const CharacterLiteralExpr& expr);
    void dumpIntLiteralExpr(const IntLiteralExpr& expr);
    void dumpFloatLiteralExpr(const FloatLiteralExpr& expr);
    void dumpBoolLiteralExpr(const BoolLiteralExpr& expr);
    void dumpNullLiteralExpr(const NullLiteralExpr&);
    void dumpUndefinedLiteralExpr(const UndefinedLiteralExpr&);
    void dumpArrayLiteralExpr(const ArrayLiteralExpr& expr);
    void dumpTupleExpr(const TupleExpr& expr);
    void dumpUnaryExpr(const UnaryExpr& expr);
    void dumpBinaryExpr(const BinaryExpr& expr);
    void dumpCallExpr(const CallExpr& expr);
    void dumpSizeofExpr(const SizeofExpr& expr);
    void dumpAddressofExpr(const AddressofExpr& expr);
    void dumpMemberExpr(const MemberExpr& expr);
    void dumpSubscriptExpr(const SubscriptExpr& expr);
    void dumpUnwrapExpr(const UnwrapExpr& expr);
    void dumpLambdaExpr(const LambdaExpr& expr);
    void dumpIfExpr(const IfExpr& expr);
    void dumpExpr(const Expr& expr);
    void dumpReturnStmt(const ReturnStmt& stmt);
    void dumpVarStmt(const VarStmt& stmt);
    void dumpDeferStmt(const DeferStmt& stmt);
    void dumpIfStmt(const IfStmt& stmt);
    void dumpSwitchStmt(const SwitchStmt& stmt);
    void dumpWhileStmt(const WhileStmt& stmt);
    void dumpForStmt(const ForStmt& stmt);
    void dumpBreakStmt(const BreakStmt&);
    void dumpContinueStmt(const ContinueStmt&);
    void dumpCompoundStmt(const CompoundStmt& stmt);
    void dumpStmt(const Stmt& stmt);
    void dumpParamDecl(const ParamDecl& decl);
    void dumpFunctionDecl(const FunctionDecl& decl);
    void dumpInitDecl(const InitDecl& decl);
    void dumpDeinitDecl(const DeinitDecl& decl);
    void dumpFunctionTemplate(const FunctionTemplate& decl);
    void dumpFieldDecl(const FieldDecl& decl);
    void dumpTypeDecl(const TypeDecl& decl);
    void dumpTypeTemplate(const TypeTemplate& decl);
    void dumpVarDecl(const VarDecl& decl);
    void dumpImportDecl(const ImportDecl& decl);
    void dumpDecl(const Decl& decl);

private:
    void breakLine();
    void dumpParams(llvm::ArrayRef<ParamDecl> params);
    void dumpGenericParams(llvm::ArrayRef<GenericParamDecl> genericParams);
    void dumpBlock(llvm::ArrayRef<std::unique_ptr<Stmt>> block);

private:
    std::ostream& out;
    int indentLevel;
};

} // namespace jazz
