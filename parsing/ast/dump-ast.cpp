#include "dump-ast.h"
#include <iostream>
#include <string>

#include <llvm/Support/ErrorHandling.h>

#include "ast/decl.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "utils.h"

using namespace jazz;

void ASTDumper::breakLine() {
    out << '\n';
    for (int i = 0; i < indentLevel; ++i) {
        out << "    ";
    }
}

void ASTDumper::dumpParams(llvm::ArrayRef<ParamDecl> params) {
    out << "(";
    for (const ParamDecl& param : params) {
        dumpParamDecl(param);
        if (&param != &params.back()) out << " ";
    }
    out << ")";
}

void ASTDumper::dumpGenericParams(llvm::ArrayRef<GenericParamDecl> genericParams) {
    out << "(";
    for (const GenericParamDecl& genericParam : genericParams) {
        out << genericParam.getName();
        if (&genericParam != &genericParams.back()) out << " ";
    }
    out << ")";
}

void ASTDumper::dumpVarExpr(const VarExpr& expr) {
    out << expr.getIdentifier();
}

void ASTDumper::dumpStringLiteralExpr(const StringLiteralExpr& expr) {
    out << '"' << expr.getValue() << '"';
}

void ASTDumper::dumpCharacterLiteralExpr(const CharacterLiteralExpr& expr) {
    out << '\'' << expr.getValue() << '\'';
}

void ASTDumper::dumpIntLiteralExpr(const IntLiteralExpr& expr) {
    out << expr.getValue().toString(10, false);
}

void ASTDumper::dumpFloatLiteralExpr(const FloatLiteralExpr& expr) {
    out << expr.getValue();
}

void ASTDumper::dumpBoolLiteralExpr(const BoolLiteralExpr& expr) {
    out << (expr.getValue() ? "true" : "false");
}

void ASTDumper::dumpNullLiteralExpr(const NullLiteralExpr&) {
    out << "null";
}

void ASTDumper::dumpUndefinedLiteralExpr(const UndefinedLiteralExpr&) {
    out << "undefined";
}

void ASTDumper::dumpArrayLiteralExpr(const ArrayLiteralExpr& expr) {
    out << "(array-literal";
    for (auto& e : expr.getElements()) {
        out << " ";
        dumpExpr(*e);
    }
    out << ")";
}

void ASTDumper::dumpTupleExpr(const TupleExpr& expr) {
    out << "(tuple-literal";
    for (auto& e : expr.getElements()) {
        out << " (";
        out << e.getName();
        out << " ";
        dumpExpr(*e.getValue());
        out << ")";
    }
    out << ")";
}

void ASTDumper::dumpUnaryExpr(const UnaryExpr& expr) {
    out << "(" << expr.getOperator();
    dumpExpr(expr.getOperand());
    out << ")";
}

void ASTDumper::dumpBinaryExpr(const BinaryExpr& expr) {
    out << "(";
    dumpExpr(expr.getLHS());
    out << " " << expr.getOperator() << " ";
    dumpExpr(expr.getRHS());
    out << ")";
}

void ASTDumper::dumpCallExpr(const CallExpr& expr) {
    out << "(call ";
    dumpExpr(expr.getCallee());
    for (auto& arg : expr.getArgs()) {
        out << " ";
        dumpExpr(*arg.getValue());
    }
    out << ")";
}

void ASTDumper::dumpSizeofExpr(const SizeofExpr& expr) {
    out << "(sizeof " << expr.getType() << ")";
}

void ASTDumper::dumpAddressofExpr(const AddressofExpr& expr) {
    out << "(addressof ";
    dumpExpr(expr.getOperand());
    out << ")";
}

void ASTDumper::dumpMemberExpr(const MemberExpr& expr) {
    out << "(member-expr ";
    dumpExpr(*expr.getBaseExpr());
    out << " " << expr.getMemberName() << ")";
}

void ASTDumper::dumpSubscriptExpr(const SubscriptExpr& expr) {
    out << "(subscript ";
    dumpExpr(*expr.getBaseExpr());
    out << " ";
    dumpExpr(*expr.getIndexExpr());
    out << ")";
}

void ASTDumper::dumpUnwrapExpr(const UnwrapExpr& expr) {
    out << "(unwrap ";
    dumpExpr(expr.getOperand());
    out << ")";
}

void ASTDumper::dumpLambdaExpr(const LambdaExpr& expr) {
    out << "(lambda ";
    dumpParams(expr.getParams());
    out << " ";
    dumpExpr(*expr.getBody());
    out << ")";
}

void ASTDumper::dumpIfExpr(const IfExpr& expr) {
    out << "(if-expr ";
    dumpExpr(*expr.getCondition());
    out << " ";
    dumpExpr(*expr.getThenExpr());
    out << " ";
    dumpExpr(*expr.getElseExpr());
    out << ")";
}

void ASTDumper::dumpExpr(const Expr& expr) {
    switch (expr.getKind()) {
        case ExprKind::VarExpr:
            dumpVarExpr(llvm::cast<VarExpr>(expr));
            break;
        case ExprKind::StringLiteralExpr:
            dumpStringLiteralExpr(llvm::cast<StringLiteralExpr>(expr));
            break;
        case ExprKind::CharacterLiteralExpr:
            dumpCharacterLiteralExpr(llvm::cast<CharacterLiteralExpr>(expr));
            break;
        case ExprKind::IntLiteralExpr:
            dumpIntLiteralExpr(llvm::cast<IntLiteralExpr>(expr));
            break;
        case ExprKind::FloatLiteralExpr:
            dumpFloatLiteralExpr(llvm::cast<FloatLiteralExpr>(expr));
            break;
        case ExprKind::BoolLiteralExpr:
            dumpBoolLiteralExpr(llvm::cast<BoolLiteralExpr>(expr));
            break;
        case ExprKind::NullLiteralExpr:
            dumpNullLiteralExpr(llvm::cast<NullLiteralExpr>(expr));
            break;
        case ExprKind::UndefinedLiteralExpr:
            dumpUndefinedLiteralExpr(llvm::cast<UndefinedLiteralExpr>(expr));
            break;
        case ExprKind::ArrayLiteralExpr:
            dumpArrayLiteralExpr(llvm::cast<ArrayLiteralExpr>(expr));
            break;
        case ExprKind::TupleExpr:
            dumpTupleExpr(llvm::cast<TupleExpr>(expr));
            break;
        case ExprKind::UnaryExpr:
            dumpUnaryExpr(llvm::cast<UnaryExpr>(expr));
            break;
        case ExprKind::BinaryExpr:
            dumpBinaryExpr(llvm::cast<BinaryExpr>(expr));
            break;
        case ExprKind::CallExpr:
            dumpCallExpr(llvm::cast<CallExpr>(expr));
            break;
        case ExprKind::SizeofExpr:
            dumpSizeofExpr(llvm::cast<SizeofExpr>(expr));
            break;
        case ExprKind::AddressofExpr:
            dumpAddressofExpr(llvm::cast<AddressofExpr>(expr));
            break;
        case ExprKind::MemberExpr:
            dumpMemberExpr(llvm::cast<MemberExpr>(expr));
            break;
        case ExprKind::SubscriptExpr:
            dumpSubscriptExpr(llvm::cast<SubscriptExpr>(expr));
            break;
        case ExprKind::UnwrapExpr:
            dumpUnwrapExpr(llvm::cast<UnwrapExpr>(expr));
            break;
        case ExprKind::LambdaExpr:
            dumpLambdaExpr(llvm::cast<LambdaExpr>(expr));
            break;
        case ExprKind::IfExpr:
            dumpIfExpr(llvm::cast<IfExpr>(expr));
            break;
    }
}

void ASTDumper::dumpBlock(llvm::ArrayRef<std::unique_ptr<Stmt>> block) {
    indentLevel++;
    for (const auto& stmt : block) {
        dumpStmt(*stmt);
    }
    indentLevel--;
}

void ASTDumper::dumpReturnStmt(const ReturnStmt& stmt) {
    breakLine();
    out << "(return-stmt";

    if (stmt.getReturnValue()) {
        out << " ";
        dumpExpr(*stmt.getReturnValue());
    }

    out << ")";
}

void ASTDumper::dumpVarStmt(const VarStmt& stmt) {
    breakLine();
    out << "(var-stmt " << stmt.getDecl().getName() << " ";
    dumpExpr(*stmt.getDecl().getInitializer());
    out << ")";
}

void ASTDumper::dumpDeferStmt(const DeferStmt& stmt) {
    breakLine();
    out << "(defer-stmt ";
    dumpExpr(stmt.getExpr());
    out << ")";
}

void ASTDumper::dumpIfStmt(const IfStmt& stmt) {
    breakLine();
    out << "(if-stmt ";
    dumpExpr(stmt.getCondition());
    indentLevel++;
    breakLine();
    out << "(then";
    dumpBlock(stmt.getThenBody());
    out << ")";
    if (!stmt.getElseBody().empty()) {
        breakLine();
        out << "(else";
        dumpBlock(stmt.getElseBody());
        out << ")";
    }
    indentLevel--;
    out << ")";
}

void ASTDumper::dumpSwitchStmt(const SwitchStmt& stmt) {
    breakLine();
    out << "(switch-stmt ";
    dumpExpr(stmt.getCondition());
    indentLevel++;
    for (const SwitchCase& switchCase : stmt.getCases()) {
        breakLine();
        out << "(case ";
        dumpExpr(*switchCase.getValue());
        dumpBlock(switchCase.getStmts());
        out << ")";
    }
    indentLevel--;
    out << ")";
}

void ASTDumper::dumpWhileStmt(const WhileStmt& stmt) {
    breakLine();
    out << "(while-stmt ";
    dumpExpr(stmt.getCondition());
    dumpBlock(stmt.getBody());
    out << ")";
}

void ASTDumper::dumpForStmt(const ForStmt& stmt) {
    breakLine();
    out << "(for-stmt ";
    dumpVarDecl(*stmt.getVariable());
    out << " ";
    dumpExpr(stmt.getRangeExpr());
    dumpBlock(stmt.getBody());
    out << ")";
}

void ASTDumper::dumpBreakStmt(const BreakStmt&) {
    breakLine();
    out << "(break-stmt)";
}

void ASTDumper::dumpContinueStmt(const ContinueStmt&) {
    breakLine();
    out << "(continue-stmt)";
}

void ASTDumper::dumpCompoundStmt(const CompoundStmt& stmt) {
    breakLine();
    out << "(compound-stmt ";
    dumpBlock(stmt.getBody());
    out << ")";
}

void ASTDumper::dumpStmt(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt:
            dumpReturnStmt(llvm::cast<ReturnStmt>(stmt));
            break;
        case StmtKind::VarStmt:
            dumpVarStmt(llvm::cast<VarStmt>(stmt));
            break;
        case StmtKind::ExprStmt:
            breakLine();
            dumpExpr(llvm::cast<ExprStmt>(stmt).getExpr());
            break;
        case StmtKind::DeferStmt:
            dumpDeferStmt(llvm::cast<DeferStmt>(stmt));
            break;
        case StmtKind::IfStmt:
            dumpIfStmt(llvm::cast<IfStmt>(stmt));
            break;
        case StmtKind::SwitchStmt:
            dumpSwitchStmt(llvm::cast<SwitchStmt>(stmt));
            break;
        case StmtKind::WhileStmt:
            dumpWhileStmt(llvm::cast<WhileStmt>(stmt));
            break;
        case StmtKind::ForStmt:
            dumpForStmt(llvm::cast<ForStmt>(stmt));
            break;
        case StmtKind::BreakStmt:
            dumpBreakStmt(llvm::cast<BreakStmt>(stmt));
            break;
        case StmtKind::ContinueStmt:
            dumpContinueStmt(llvm::cast<ContinueStmt>(stmt));
            break;
        case StmtKind::CompoundStmt:
            dumpCompoundStmt(llvm::cast<CompoundStmt>(stmt));
            break;
    }
}

void ASTDumper::dumpParamDecl(const ParamDecl& decl) {
    out << "(" << decl.getType() << " " << decl.getName() << ")";
}

void ASTDumper::dumpFunctionDecl(const FunctionDecl& decl) {
    breakLine();
    out << (decl.isExtern() ? "(extern-function-decl " : "(function-decl ");
    jazz::operator<<(out, decl.getName());
    out << " ";
    dumpParams(decl.getParams());
    out << " " << decl.getReturnType();
    if (!decl.isExtern()) {
        dumpBlock(decl.getBody());
    }
    out << ")";
}

void ASTDumper::dumpInitDecl(const InitDecl& decl) {
    breakLine();
    out << "(init-decl " << decl.getTypeDecl()->getName() << " (";
    for (const ParamDecl& param : decl.getParams()) {
        dumpParamDecl(param);
        if (&param != &decl.getParams().back()) out << " ";
    }
    out << ")";
    dumpBlock(decl.getBody());
    out << ")";
}

void ASTDumper::dumpDeinitDecl(const DeinitDecl& decl) {
    breakLine();
    out << "(deinit-decl " << decl.getTypeDecl()->getName();
    dumpBlock(decl.getBody());
    out << ")";
}

void ASTDumper::dumpFunctionTemplate(const FunctionTemplate& decl) {
    breakLine();
    out << "(function-template ";
    dumpGenericParams(decl.getGenericParams());
    out << decl.getFunctionDecl() << ")";
}

void ASTDumper::dumpFieldDecl(const FieldDecl& decl) {
    breakLine();
    out << "(field-decl " << decl.getType() << " " << decl.getName() << ")";
}

void ASTDumper::dumpTypeDecl(const TypeDecl& decl) {
    breakLine();
    out << "(type-decl ";
    switch (decl.getTag()) {
        case TypeTag::Struct:
            out << "struct ";
            break;
        case TypeTag::Interface:
            out << "interface ";
            break;
        case TypeTag::Union:
            out << "union ";
            break;
        case TypeTag::Enum:
            out << "enum ";
            break;
    }
    out << decl.getName();
    indentLevel++;

    if (auto* enumDecl = llvm::dyn_cast<EnumDecl>(&decl)) {
        for (auto& enumCase : enumDecl->getCases()) {
            breakLine();
            out << "(enum-case " << enumCase.getName() << " ";
            dumpExpr(*enumCase.getValue());
            out << ")";
        }
    }

    for (const FieldDecl& field : decl.getFields()) {
        dumpFieldDecl(field);
    }

    indentLevel--;
    out << ")";
}

void ASTDumper::dumpTypeTemplate(const TypeTemplate& decl) {
    breakLine();
    out << "(type-template ";
    dumpGenericParams(decl.getGenericParams());
    out << " ";
    dumpTypeDecl(*decl.getTypeDecl());
    out << ")";
}

void ASTDumper::dumpVarDecl(const VarDecl& decl) {
    breakLine();
    out << "(var-decl " << decl.getName();
    if (decl.getInitializer()) {
        out << " ";
        dumpExpr(*decl.getInitializer());
    }
    out << ")";
}

void ASTDumper::dumpImportDecl(const ImportDecl& decl) {
    breakLine();
    out << "(import-decl \"" << decl.getTarget() << "\")";
}

void ASTDumper::dumpDecl(const Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl:
            dumpParamDecl(llvm::cast<ParamDecl>(decl));
            break;
        case DeclKind::GenericParamDecl:
            llvm_unreachable("handled via FunctionTemplate");
            break;
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
            dumpFunctionDecl(llvm::cast<FunctionDecl>(decl));
            break;
        case DeclKind::InitDecl:
            dumpInitDecl(llvm::cast<InitDecl>(decl));
            break;
        case DeclKind::DeinitDecl:
            dumpDeinitDecl(llvm::cast<DeinitDecl>(decl));
            break;
        case DeclKind::FunctionTemplate:
            dumpFunctionTemplate(llvm::cast<FunctionTemplate>(decl));
            break;
        case DeclKind::TypeDecl:
            dumpTypeDecl(llvm::cast<TypeDecl>(decl));
            break;
        case DeclKind::TypeTemplate:
            dumpTypeTemplate(llvm::cast<TypeTemplate>(decl));
            break;
        case DeclKind::EnumDecl:
            dumpTypeDecl(llvm::cast<EnumDecl>(decl));
            break;
        case DeclKind::VarDecl:
            dumpVarDecl(llvm::cast<VarDecl>(decl));
            break;
        case DeclKind::FieldDecl:
            dumpFieldDecl(llvm::cast<FieldDecl>(decl));
            break;
        case DeclKind::ImportDecl:
            dumpImportDecl(llvm::cast<ImportDecl>(decl));
            break;
    }
}

void ASTDumper::dumpModule(const Module& module) {
    for (const auto& sourceFile : module.getSourceFiles()) {
        out << "(source-file " << sourceFile.getFilePath();
        indentLevel++;
        for (const auto& decl : sourceFile.getTopLevelDecls()) {
            dumpDecl(*decl);
        }
        indentLevel--;
        out << ")\n";
    }
}
