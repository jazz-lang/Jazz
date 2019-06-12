#include "llvm.h"
#include <iostream>
using namespace jazz;

void Codegen::codegenReturnStmt(const ReturnStmt& stmt) {
    codegenDeferredExprsAndDeinitCallsForReturn();

    if (auto* returnValue = stmt.getReturnValue()) {
        builder.CreateRet(codegenExprPass(*returnValue, builder.getCurrentFunctionReturnType()));
    } else {
        if (!currentDecl->isMain()) {
            builder.CreateRetVoid();
        } else {
            builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0));
        }
    }
}

void Codegen::codegenVarStmt(const VarStmt& stmt) {
    auto* alloca = createEntryBlockAlloca(stmt.getDecl().getType(), &stmt.getDecl(), nullptr, stmt.getDecl().getName());
    auto* initializer = stmt.getDecl().getInitializer();

    if (auto* callExpr = llvm::dyn_cast<CallExpr>(initializer)) {
        if (callExpr->getCalleeDecl()) {
            if (auto* initDecl = llvm::dyn_cast<InitDecl>(callExpr->getCalleeDecl())) {
                if (initDecl->getTypeDecl()->getType() == stmt.getDecl().getType().asImmutable()) {
                    codegenCallExpr(*callExpr, alloca);
                    return;
                }
            }
        }
    }

    if (!initializer->isUndefinedLiteralExpr()) {
        builder.CreateStore(codegenExprPass(*initializer, alloca->getAllocatedType()), alloca);
    }
}

void Codegen::codegenBlock(llvm::ArrayRef<std::unique_ptr<Stmt>> stmts, llvm::BasicBlock* destination, llvm::BasicBlock* continuation) {
    builder.SetInsertPoint(destination);

    beginScope();
    for (const auto& stmt : stmts) {
        codegenStmt(*stmt);
        if (stmt->isReturnStmt() || stmt->isBreakStmt()) break;
    }
    endScope();

    llvm::BasicBlock* insertBlock = builder.GetInsertBlock();
    if (insertBlock->empty() || (!llvm::isa<llvm::ReturnInst>(insertBlock->back()) && !llvm::isa<llvm::BranchInst>(insertBlock->back()))) {
        builder.CreateBr(continuation);
    }
}

void Codegen::codegenIfStmt(const IfStmt& ifStmt) {
    auto* condition = codegenExpr(ifStmt.getCondition());
    if (ifStmt.getCondition().getKind() == ExprKind::ComptimeExpr) {
        auto val = evalExpr(ifStmt.getCondition());
        
        if (val.boolean) {
            beginScope();
            for (auto& stmt : ifStmt.getThenBody()) {
                codegenStmt(*stmt);
            }
            endScope();
        } else {
            beginScope();
            for (auto& stmt : ifStmt.getElseBody()) {
                codegenStmt(*stmt);
            }
            endScope();
        }
        
        return;
    }
    if (condition->getType()->isPointerTy()) {
        condition = codegenImplicitNullComparison(condition);
    }
    auto* function = builder.GetInsertBlock()->getParent();
    auto* thenBlock = llvm::BasicBlock::Create(ctx, "if.then", function);
    auto* elseBlock = llvm::BasicBlock::Create(ctx, "if.else", function);
    auto* endIfBlock = llvm::BasicBlock::Create(ctx, "if.end", function);
    builder.CreateCondBr(condition, thenBlock, elseBlock);
    codegenBlock(ifStmt.getThenBody(), thenBlock, endIfBlock);
    codegenBlock(ifStmt.getElseBody(), elseBlock, endIfBlock);
    builder.SetInsertPoint(endIfBlock);
}

void Codegen::codegenSwitchStmt(const SwitchStmt& switchStmt) {
    auto* condition = codegenExpr(switchStmt.getCondition());
    auto* function = builder.GetInsertBlock()->getParent();
    auto* insertBlockBackup = builder.GetInsertBlock();
    auto caseIndex = 0;

    auto cases = map(switchStmt.getCases(), [&](const SwitchCase& switchCase) {
        auto* value = codegenExpr(*switchCase.getValue());
        auto* block = llvm::BasicBlock::Create(ctx, llvm::Twine("switch.case.", std::to_string(caseIndex++)), function);
        return std::make_pair(llvm::cast<llvm::ConstantInt>(value), block);
    });

    builder.SetInsertPoint(insertBlockBackup);
    auto* defaultBlock = llvm::BasicBlock::Create(ctx, "switch.default", function);
    auto* end = llvm::BasicBlock::Create(ctx, "switch.end", function);
    breakTargets.push_back(end);
    auto* switchInst = builder.CreateSwitch(condition, defaultBlock);

    auto casesIterator = cases.begin();
    for (auto& switchCase : switchStmt.getCases()) {
        auto* value = casesIterator->first;
        auto* block = casesIterator->second;
        codegenBlock(switchCase.getStmts(), block, end);
        switchInst->addCase(value, block);
        ++casesIterator;
    }

    codegenBlock(switchStmt.getDefaultStmts(), defaultBlock, end);
    breakTargets.pop_back();
    builder.SetInsertPoint(end);
}

void Codegen::codegenWhileStmt(const WhileStmt& whileStmt) {
    auto* increment = whileStmt.getIncrement();
    if (whileStmt.getCondition().getKind() == ExprKind::ComptimeExpr || whileStmt.shouldInline()) {
        auto boolean = evalExpr(whileStmt.getCondition());
        auto body = whileStmt.getBody();
        inline_while = true;
        while (boolean.boolean && !inline_break) {
            beginScope();
            for (auto& statement : body) {
                codegenStmt(*statement);
                if (statement->isReturnStmt()) {
                    break;
                } 

                if (inline_while_continue) {
                    continue;
                }

                if (statement->isBreakStmt() || inline_break) {
                    break;
                }
            }
            boolean = evalExpr(whileStmt.getCondition());
            endScope();
        }
        inline_while = false;
        return;
    }
    auto* function = builder.GetInsertBlock()->getParent();
    auto* condition = llvm::BasicBlock::Create(ctx, "loop.condition", function);
    auto* body = llvm::BasicBlock::Create(ctx, "loop.body", function);
    auto* afterBody = increment ? llvm::BasicBlock::Create(ctx, "loop.increment", function) : condition;
    auto* end = llvm::BasicBlock::Create(ctx, "loop.end", function);

    breakTargets.push_back(end);
    continueTargets.push_back(afterBody);
    builder.CreateBr(condition);

    builder.SetInsertPoint(condition);
    auto* conditionValue = codegenExpr(whileStmt.getCondition());
    if (conditionValue->getType()->isPointerTy()) {
        conditionValue = codegenImplicitNullComparison(conditionValue);
    }
    builder.CreateCondBr(conditionValue, body, end);
    codegenBlock(whileStmt.getBody(), body, afterBody);

    if (increment) {
        builder.SetInsertPoint(afterBody);
        codegenExpr(*increment);
        builder.CreateBr(condition);
    }

    breakTargets.pop_back();
    continueTargets.pop_back();
    builder.SetInsertPoint(end);
}

void Codegen::codegenBreakStmt(const BreakStmt&) {
    if (!inline_while) {
    jazz_assert(!breakTargets.empty());
    builder.CreateBr(breakTargets.back());
    } else {
        inline_break = true;
    }
}

void Codegen::codegenContinueStmt(const ContinueStmt&) {
    if (!inline_while) {
        jazz_assert(!continueTargets.empty());
        builder.CreateBr(continueTargets.back());
    } else {
        inline_while_continue = true;
    }
}

void Codegen::codegenCompoundStmt(const CompoundStmt& compoundStmt) {
    beginScope();

    for (auto& stmt : compoundStmt.getBody()) {
        codegenStmt(*stmt);
    }

    endScope();
}

void Codegen::codegenStmt(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ComptimeStmt: {
            auto& comptimeStmt = llvm::cast<ComptimeStmt>(stmt);
            evalStmt(*comptimeStmt.stmt);
            break;
        }
        case StmtKind::ReturnStmt:
            codegenReturnStmt(llvm::cast<ReturnStmt>(stmt));
            break;
        case StmtKind::VarStmt:
            codegenVarStmt(llvm::cast<VarStmt>(stmt));
            break;
        case StmtKind::ExprStmt:
            codegenExpr(llvm::cast<ExprStmt>(stmt).getExpr());
            break;
        case StmtKind::DeferStmt:
            deferEvaluationOf(llvm::cast<DeferStmt>(stmt).getExpr());
            break;
        case StmtKind::IfStmt:
            codegenIfStmt(llvm::cast<IfStmt>(stmt));
            break;
        case StmtKind::SwitchStmt:
            codegenSwitchStmt(llvm::cast<SwitchStmt>(stmt));
            break;
        case StmtKind::WhileStmt:
            codegenWhileStmt(llvm::cast<WhileStmt>(stmt));
            break;
        case StmtKind::ForStmt: {
            auto& forStmt = llvm::cast<ForStmt>(stmt);

            auto &range = forStmt.getRangeExpr();
            auto start = evalExpr(*range.getSubExprs()[1]);
            auto end = evalExpr(*range.getSubExprs()[2]);
            auto var = forStmt.getVariable();
            beginScope();
            setComptimeLocalValue(var->getType(),var->getName(),start);
            inline_while = true;
            auto should_break = false;
            while (start.i64 < end.i64 && !should_break) {
                beginScope();
                
                for (auto& statement : forStmt.getBody()) {
                    codegenStmt(*statement);
                    if (inline_while_continue) {
                        continue;
                    }
                    if (statement->isBreakStmt() || inline_break) {
                        should_break = true;
                        break;
                    }
                }
                endScope();
                start.i64++;
                findComptimeLocal(var->getName(),var)->i64 = start.i64;
            }
            inline_while = false;
            inline_break = false;
            inline_while_continue = false;
            endScope();
            break;
        }
        case StmtKind::BreakStmt:
            codegenBreakStmt(llvm::cast<BreakStmt>(stmt));
            break;
        case StmtKind::ContinueStmt:
            codegenContinueStmt(llvm::cast<ContinueStmt>(stmt));
            break;
        case StmtKind::CompoundStmt:
            codegenCompoundStmt(llvm::cast<CompoundStmt>(stmt));
            break;
    }
}
