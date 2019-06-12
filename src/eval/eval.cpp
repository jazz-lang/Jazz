#include "eval/eval.h"
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/IR/Verifier.h>

EValue* Evaluator::findLocal(llvm::StringRef name,const Decl* decl) {
    EValue *value = new EValue();
    bool found = false;
    
    for (auto& scope : llvm::reverse(scopes)) {
        auto it = scope.getLocalValues().find(name);
        
        if (it == scope.getLocalValues().end()) continue;
        value = const_cast<EValue*>(&it->second);
        found = true;
        break;
    }
    if (found) {
        return value;
    }
    jazz_assert(decl);
    llvm_unreachable("UNIMPLEMENTED");
}

EValue* Evaluator::evalLValueExpr(const Expr& expr) {
    switch (expr.getKind()) {
        case ExprKind::VarExpr: {
            auto& varExpr = llvm::cast<VarExpr>(expr);
        
            return findLocal(varExpr.getIdentifier(),varExpr.getDecl());
        }
        default: {
            llvm_unreachable("unimplemented");
        }
    }
}

EValue Evaluator::evalExpr(const Expr& expr) {
    switch (expr.getKind()) {
        case ExprKind::IntLiteralExpr: {
            auto& lit = llvm::cast<IntLiteralExpr>(expr);
            auto val = EValue(expr.getType());
            int64_t integer = lit.getValue().getExtValue();
            val.val = reinterpret_cast<uint8_t*>(integer);

            return val;
        }

        case ExprKind::FloatLiteralExpr: {
            auto& lit = llvm::cast<FloatLiteralExpr>(expr);
            double floating = lit.getValue();
            auto val = EValue(expr.getType());
            val.val = reinterpret_cast<uint8_t*>(*reinterpret_cast<int64_t*>(&floating));

            return val;
        }

        case ExprKind::IfExpr: {
            auto& ifExpr = llvm::cast<IfExpr>(expr);
            
            auto cond = evalExpr(*ifExpr.getCondition());
            
            if (*cond.val) {
                return evalExpr(*ifExpr.getThenExpr());
            } else {
                return evalExpr(*ifExpr.getElseExpr());
            }
        }

        case ExprKind::BoolLiteralExpr: {
            auto& lit = llvm::cast<BoolLiteralExpr>(expr);
            auto val = EValue(expr.getType());
            val.val = reinterpret_cast<uint8_t*>(lit.getValue());
            return val;
        }

        case ExprKind::CharacterLiteralExpr: {
            auto& lit = llvm::cast<CharacterLiteralExpr>(expr);
            auto val = EValue(expr.getType());
            val.val = reinterpret_cast<uint8_t*>(lit.getValue());
            return val;
        }


        case ExprKind::VarExpr: {
            auto& varExpr = llvm::cast<VarExpr>(expr);
            
            return *findLocal(varExpr.getIdentifier(),varExpr.getDecl());
        }
        case ExprKind::ComptimeExpr: {
            auto& comptimeExpr = llvm::cast<ComptimeExpr>(expr);
            return evalExpr(comptimeExpr.getExpr());
        }

        case ExprKind::UndefinedLiteralExpr: {
            return EValue(); // TODO: allocate zeroed memory with type of this expression
        };

        case ExprKind::BinaryExpr: {
            auto& binaryExpr = llvm::cast<BinaryExpr>(expr);
            
            auto rhs = evalExpr(binaryExpr.getRHS());
            if (binaryExpr.isAssignment()) {
                auto lval = evalLValueExpr(binaryExpr.getLHS());
                *lval = rhs;
                return rhs;
            } else {
                auto lhs = evalExpr(binaryExpr.getLHS());
                switch (binaryExpr.getOperator().getKind()) {
                    case Token::Plus: 
                        return lhs + rhs;
                    case Token::Minus: 
                        return lhs - rhs;
                    case Token::Star: 
                        return lhs * rhs;
                    case Token::Slash: 
                        return lhs / rhs;
                    case Token::Equal: 
                        return lhs == rhs;
                    case Token::NotEqual: 
                        return lhs != rhs;
                    case Token::Greater: 
                        return lhs > rhs;
                    case Token::GreaterOrEqual: 
                        return lhs >= rhs;
                }
            }
        };



        default: {}
    }
}

void Evaluator::evalStmt(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ComptimeStmt: {
            auto &statement = llvm::cast<ComptimeStmt>(stmt);
            evalStmt(statement.getStmt());
            break;
        }
        case StmtKind::IfStmt: { 
            auto &ifStmt = llvm::cast<IfStmt>(stmt);
            auto condition = evalExpr(ifStmt.getCondition());
            if (*condition.val) {
                auto body = ifStmt.getThenBody();
                for (auto& statement : body) {
                    evalStmt(*statement);
                    if (eval_return) {
                        break;
                    }
                }
            } else {
                auto body = ifStmt.getElseBody();
                for (auto& statement : body) {
                    evalStmt(*statement);
                    if (eval_return) {
                        break;
                    }
                }
            }
            break;
        }

        case StmtKind::ReturnStmt: {
            auto& returnStmt = llvm::cast<ReturnStmt>(stmt);
            if (auto value = returnStmt.getReturnValue()) {
                return_value = new EValue(value->getType());
                auto val = evalExpr(*value);
                return_value->val = val.val;
            } else {
                return_value = new EValue(Type::getVoid());
            }
            eval_return = true;
            break;
        }

        case StmtKind::BreakStmt: {
            eval_break = true;
            break;
        }
        case StmtKind::ContinueStmt: {
            eval_continue = true;
            break;
        }

        case StmtKind::ExprStmt: {
            auto& exprStmt = llvm::cast<ExprStmt>(stmt);
            evalExpr(exprStmt.getExpr());
            break;
        }
        case StmtKind::WhileStmt: {
            auto& whileStmt = llvm::cast<WhileStmt>(stmt);
            auto condition = evalExpr(whileStmt.getCondition());
            
            while(*condition.val && !eval_break && !eval_return) {
                if (eval_continue) {
                    continue;
                }
                for (auto& statement : whileStmt.getBody()) {
                    evalStmt(*statement);
                }
                condition = evalExpr(whileStmt.getCondition());
            }
            break;
        }



        default: 
            llvm_unreachable("all cases handled");
     
    }
}



EValue EValue::operator+(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) + reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) + reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(type);
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.f32 = f32 + rhs.f32;
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.f64 = f64 + rhs.f64;
        } else {
            val.f80 = f80 + rhs.f80;
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}

EValue EValue::operator-(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) - reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) - reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(type);
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.f32 = f32 - rhs.f32;
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.f64 = f64 - rhs.f64;
        } else {
            val.f80 = f80 - rhs.f80;
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}

EValue EValue::operator*(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) * reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) * reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(type);
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.f32 = f32 - rhs.f32;
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.f64 = f64 - rhs.f64;
        } else {
            val.f80 = f80 - rhs.f80;
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}

EValue EValue::operator/(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) / reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(type);
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) / reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(type);
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.f32 = f32 / rhs.f32;
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.f64 = f64 / rhs.f64;
        } else {
            val.f80 = f80 / rhs.f80;
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}



EValue EValue::operator==(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(Type::getBool());
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) == reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(Type::getBool());
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) == reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(Type::getBool());
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.val = reinterpret_cast<uint8_t*>(f32 == rhs.f32);
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.val = reinterpret_cast<uint8_t*>(f64 == rhs.f64);
        } else {
            val.val = reinterpret_cast<uint8_t*>(f80 == rhs.f80);
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}

EValue EValue::operator>(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(Type::getBool());
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) > reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(Type::getBool());
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) > reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(Type::getBool());
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.val = reinterpret_cast<uint8_t*>(f32 > rhs.f32);
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.val = reinterpret_cast<uint8_t*>(f64 > rhs.f64);
        } else {
            val.val = reinterpret_cast<uint8_t*>(f80 > rhs.f80);
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}

EValue EValue::operator>=(const EValue& rhs) {
    auto val = EValue();
    if (*(*this > rhs).val || *(*this == rhs).val) {
        val.val =reinterpret_cast<uint8_t*>(true);
    }  else {
        val.val =reinterpret_cast<uint8_t*>(false);
    }
}

EValue EValue::operator<(const EValue& rhs) {
    if (type.isInteger() && rhs.type.isInteger()) {
        if (type.isUnsigned()) {
            auto val = EValue(Type::getBool());
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<uint64_t>(this->val) < reinterpret_cast<uint64_t>(rhs.val)
                );
            return val;
        } else {
            auto val = EValue(Type::getBool());
            val.val = 
                reinterpret_cast<uint8_t*>(
                    reinterpret_cast<int64_t>(this->val) < reinterpret_cast<int64_t>(rhs.val)
                );
            return val;
        }
    } else if (type.isFloatingPoint() && rhs.type.isFloatingPoint()) {
        auto val = EValue(Type::getBool());
        if ((type.isFloat32() || type.isFloat())&& (rhs.type.isFloat32() || rhs.type.isFloat())) {
            val.val = reinterpret_cast<uint8_t*>(f32 < rhs.f32);
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.val = reinterpret_cast<uint8_t*>(f64 < rhs.f64);
        } else {
            val.val = reinterpret_cast<uint8_t*>(f80 < rhs.f80);
        }
        return val;
    } else if (type.isPointerType() && rhs.type.isInteger()) {
        llvm_unreachable("Pointer arithmetics not implemented yet");
    } else {
        llvm_unreachable("Unimplemented");
    }
}

EValue EValue::operator!=(const EValue& rhs) {
    auto val = EValue(Type::getBool());
    val.val = reinterpret_cast<uint8_t*>(!(static_cast<bool>(*(*this == rhs).val)));
    return val;
}