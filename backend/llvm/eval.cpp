#include <iostream>
#include "ast/stmt.h"
#include "llvm.h"
#include "llvm/ADT/StringRef.h"
#include "utils.h"
using namespace jazz;

int64_t getTypeSize(const Type& type) {
    if (type.isInt8() || type.isUInt8() || type.isBool() || type.isChar()) {
        return sizeof(uint8_t);
    } else if (type.isInt16() || type.isUInt16()) {
        return sizeof(int16_t);
    } else if (type.isInt32() || type.isUInt32()) {
        return sizeof(int32_t);
    } else if (type.isInt64() || type.isUInt64()) {
        return sizeof(int64_t);
    } else if (type.isInt()) {
        return sizeof(int);
    } else if (type.isUInt()) {
        return sizeof(unsigned);
    } else if (type.isFloat()) {
        return sizeof(float);
    } else if (type.isPointerType()) {
        return sizeof(size_t);
    } else if (type.isFunctionType()) {
        return sizeof(size_t);
    } else if (type.isEnumType()) {
        return sizeof(int);
    } else if (type.isArrayWithUnknownSize()) {
        return sizeof(size_t);
    } else if (type.isArrayWithConstantSize()) {
        return getTypeSize(llvm::cast<ArrayType>(*type).getElementType()) *
               llvm::cast<ArrayType>(*type).getSize();
    } else if (type.isFloat() || type.isFloat32()) {
        return sizeof(float);
    } else if (type.isFloat64()) {
        return sizeof(double);
    } else if (type.isOptionalType()) {
        auto& optional_type = llvm::cast<OptionalType>(*type);
        return getTypeSize(optional_type.getWrappedType());
    } else if (type.isTupleType()) {
        auto& tuple_type = llvm::cast<TupleType>(*type);
        auto total_size = 0;
        for (auto& val : tuple_type.getElements()) {
            total_size += getTypeSize(val.type);
        }
        return total_size;

    } else {
        auto totalSize = 0;
        for (auto field : type.getDecl()->getFields()) {
            totalSize += getTypeSize(field.getType());
        }
        return totalSize;
    }
}

static Value builtinConversion(Value& value, Type type) {
    auto val = Value();
    val.type = type;
    if (value.type.isUnsigned() && type.isSigned()) {
        val.i64 = (int64_t)value.u64;
    } else if (value.type.isSigned() && type.isUnsigned()) {
        val.u64 = (uint64_t)value.i64;
    } else if ((value.type.isInteger() || value.type.isChar() ||
                value.type.isBool()) &&
               (type.isInteger() || type.isChar())) {
        val.i64 = value.i64;
    } else if (value.type.isFloatingPoint()) {
        if (type.isSigned() && (value.type.isFloat32() || value.type.isFloat()))
            val.i32 = (int32_t)value.f32;
        else if (type.isUnsigned() &&
                 (value.type.isFloat32() || value.type.isFloat()))
            val.u32 = (uint32_t)value.f32;
        else if (type.isSigned() && value.type.isFloat64())
            val.i64 = (int64_t)value.f64;
        else if (type.isUnsigned() && value.type.isFloat64())
            val.u64 = (uint64_t)value.f64;
    } else if (value.type.isInteger()) {
        if ((type.isFloat() || type.isFloat32()) && value.type.isSigned())
            val.f32 = (float)value.i32;
        else if ((type.isFloat() || type.isFloat32()) &&
                 value.type.isUnsigned())
            val.f32 = (float)value.u32;
        else if (type.isFloat64() && value.type.isUnsigned())
            val.f64 = (long double)value.u64;
        else if (type.isFloat64() && value.type.isSigned())
            val.f64 = (long double)value.i64;
    } else {
        error(type.getLocation(), "can't handle conversion at compile time");
    }

    return val;
}

Value jazz::Codegen::evalBinaryOp(Token::Kind op, Value& lhs, Value& rhs,
                                  Type lhsType, Position location) {
    switch (op) {
        case Token::Plus: {
            if (lhs.type.isPointerType() && rhs.type.isInteger()) {
                auto val = Value();

                if (lhs.reference) {
                    val.ptr = lhs.ref->ptr +
                              rhs.i32 * getTypeSize(lhs.type.getPointee());
                } else {
                    val.ptr =
                        lhs.ptr + rhs.i32 * getTypeSize(lhs.type.getPointee());
                }
                val.type = lhs.type;

                return val;
            }
            auto val = lhs + rhs;

            return val;
        }
        case Token::Minus:
            return lhs - rhs;
        case Token::Star:
            return lhs * rhs;
        case Token::Slash:
            return lhs / rhs;
        case Token::Greater:
            return lhs > rhs;
        case Token::Less:
            return lhs < rhs;
        case Token::LessOrEqual:
            return lhs <= rhs;
        case Token::GreaterOrEqual:
            return lhs >= rhs;
        case Token::Equal: {
            auto value = lhs == rhs;
            value.type = Type::getBool();
            return value;
        }
        case Token::NotEqual:
            return lhs != rhs;
        case Token::LeftShift:
            return lhs << rhs;
        case Token::RightShift:
            return lhs >> rhs;
        case Token::And:
            return lhs & rhs;
        case Token::Or:
            return lhs | rhs;
        default: {
            printf("Some shit\n");
            jazz::error(location, llvm::StringRef("unimplemented"));
        }
    }
}

Value* jazz::Codegen::evalLvalueExpr(const Expr& expr) {
    auto* val = new Value();
    switch (expr.getKind()) {
        case ExprKind::VarExpr: {
            auto& varExpr = llvm::cast<VarExpr>(expr);
            auto value =
                findComptimeLocal(varExpr.getIdentifier(), varExpr.getDecl());

            val = value;
            break;
        }
        case ExprKind::MemberExpr: {
            auto& memberExpr = llvm::cast<MemberExpr>(expr);

            auto value = evalExpr(*memberExpr.getBaseExpr());
            auto fieldName = memberExpr.getMemberName();

            return &(*value.tuple_value)[fieldName];
        }

        case ExprKind::SubscriptExpr: {
            auto& subscriptExpr = llvm::cast<SubscriptExpr>(expr);

            if (subscriptExpr.getBaseExpr()->getType().isArrayType()) {
                auto value = evalExpr(*subscriptExpr.getBaseExpr());

                auto idx = evalExpr(*subscriptExpr.getIndexExpr());

                if (idx.i32 >= value.array->size()) {
                    error(expr.getLocation(), "Array index ", idx.i32,
                          " out of bouds");
                } else {
                    return &(*value.array)[idx.i32];
                }
            }
        }

        /*case ExprKind::ArrayLiteralExpr: {
            auto& arrayExpr = llvm::cast<ArrayLiteralExpr>(expr);
            val->array = new std::vector<Value>();
            for (auto& element : arrayExpr.getElements()) {
                auto value = evalExpr(*element);
                val->array->push_back(value);
            }

            val->type = arrayExpr.getType();
            return val;
            break;
        }*/
        case ExprKind::UnaryExpr: {
            auto& unaryExpr = llvm::cast<UnaryExpr>(expr);
            auto value = evalExpr(unaryExpr.getOperand());
            switch (unaryExpr.getOperator().getKind()) {
                case Token::Star: {
                    if (value.ref) {
                        val->ref = value.ref;
                        val->type = unaryExpr.getType();
                        val->reference = true;
                        return val;
                    } else {
                        if (value.type.isPointerType()) {
                            auto type = value.type.getPointee();
                            if (type.isInteger())
                                val->i64 = *(int64_t*)value.ptr;
                            if (type.isFloat32() || type.isFloat())
                                val->f32 = *(float*)value.ptr;
                            if (type.isFloat64())
                                val->f64 = *(long double*)value.ptr;
                            if (type.isString())
                                val->string = *(std::string**)value.ptr;
                            val->type = unaryExpr.getType();

                            return val;
                        }
                    }
                    break;
                }

                default: {
                    llvm_unreachable("unimplemented");
                }
            }
            break;
        }
        default: {
            printf("%i\n", expr.getKind());
            llvm_unreachable("unimplemented");
        }
    }
    return val;
}

Value jazz::Codegen::evalExpr(const Expr& expr) {
    auto val = Value();

    switch (expr.getKind()) {
        case ExprKind::TupleExpr: {
            const TupleExpr& tupleExpr = llvm::cast<TupleExpr>(expr);
            val.tuple_value = new llvm::StringMap<Value>();
            val.type = expr.getType();
            for (auto& element : tupleExpr.getElements()) {
                auto element_val = evalExpr(*element.getValue());
                (*val.tuple_value)[element.getName()] = element_val;
            }

            break;
        }
        case ExprKind::TypenameExpr: {
            val.string = new std::string(
                llvm::cast<TypenameExpr>(expr).getType().getName());
            val.type = Type::getChar().makePointer();
            break;
        }
        case ExprKind::IntLiteralExpr: {
            auto int_val = expr.getConstInt();

            val.i64 = int_val.getExtValue();
            val.type = expr.getType();
            break;
        }
        case ExprKind::FloatLiteralExpr: {
            auto float_literal = llvm::cast<FloatLiteralExpr>(expr);
            auto float_val = float_literal.getValue();

            if (expr.getType().isFloat32()) {
                val.f32 = static_cast<float>(float_val);
            } else {
                val.f64 = static_cast<long double>(float_val);
            }
            val.type = expr.getType();
            break;
        }

        case ExprKind::SubscriptExpr: {
            auto& subscriptExpr = llvm::cast<SubscriptExpr>(expr);

            if (subscriptExpr.getBaseExpr()->getType().isArrayType()) {
                auto value = evalExpr(*subscriptExpr.getBaseExpr());

                auto index = evalExpr(*subscriptExpr.getIndexExpr());

                if (index.i32 > static_cast<int>(value.array->size())) {
                    error(expr.getLocation(), "Array index ", index.i32,
                          " out of bounds ", value.array->size());
                }

                return (*value.array)[index.i32];
            } else {
                llvm_unreachable("unimplemented");
            }
        }

        case ExprKind::ArrayLiteralExpr: {
            auto& arrayExpr = llvm::cast<ArrayLiteralExpr>(expr);
            val.array = new std::vector<Value>();
            auto elems = arrayExpr.getElements();
            for (auto& element : elems) {
                auto value = evalExpr(*element);
                val.array->push_back(value);
            }

            val.type = arrayExpr.getType();
            return val;
        }
        case ExprKind::BinaryExpr: {
            auto& binaryExpr = llvm::cast<BinaryExpr>(expr);
            auto lhs_type = binaryExpr.getLHS().getType();
            auto rhs_type = binaryExpr.getLHS().getType();

            auto rhs = evalExpr(binaryExpr.getRHS());
            rhs.type = rhs_type;
            if (binaryExpr.isAssignment()) {
                auto* lval = evalLvalueExpr(binaryExpr.getLHS());
                if (lval->type.isString()) {
                    lval->string = rhs.string;
                } else if (lval->type.isFloat32()) {
                    lval->f32 = rhs.f32;
                } else if (lval->type.isFloat64()) {
                    lval->f64 = rhs.f64;
                } else if (lval->type.isInteger()) {
                    lval->i64 = rhs.i64;
                } else if (lval->reference) {
                    lval->ref->i64 = rhs.i64;
                }

                return rhs;
            }

            auto lhs = evalExpr(binaryExpr.getLHS());
            lhs.type = lhs_type;
            if (lhs.type.isPointerType() && rhs.type.isInteger()) {
                auto type = lhs.type.getPointee();

                if (type.isInt() || type.isInt32() || type.isUInt() ||
                    type.isUInt32()) {
                    val.ptr = (uint8_t*)(int32_t*)(lhs.ptr + rhs.i32);
                } else if (type.isInt8() || type.isUInt8() || type.isChar()) {
                    val.ptr = (uint8_t*)(lhs.ptr + rhs.i32);
                } else if (type.isInt16() || type.isUInt16()) {
                    val.ptr = (uint8_t*)(int16_t*)(lhs.ptr + rhs.i32);
                } else if (type.isInt64() || type.isUInt64()) {
                    val.ptr = (uint8_t*)(int64_t*)(lhs.ptr + rhs.i32);
                } else {
                    val.ref = lhs.ref + rhs.i32;
                }
            }

            auto result = evalBinaryOp(binaryExpr.getOperator().getKind(), lhs,
                                       rhs, binaryExpr.getLHS().getType());

            val = result;
            val.type = binaryExpr.getLHS().getType();
            break;
        }
        case ExprKind::NullLiteralExpr: {
            val.ptr = NULL;
            val.type = expr.getType();
            break;
        }
        case ExprKind::StringLiteralExpr: {
            auto& stringExpr = llvm::cast<StringLiteralExpr>(expr);
            val.string = new std::string(stringExpr.getValue().str());
            val.type = expr.getType();
            break;
        }
        case ExprKind::SizeofExpr: {
            auto& sizeofExpr = llvm::cast<SizeofExpr>(expr);
            auto elementType = std::move(sizeofExpr.getType());
            val.i64 = getTypeSize(elementType);
            val.type = Type::getUInt64();
            break;
        }

        case ExprKind::VarExpr: {
            auto& varExpr = llvm::cast<VarExpr>(expr);
            auto value =
                findComptimeLocal(varExpr.getIdentifier(), varExpr.getDecl());

            val = *value;
            break;
        }

        case ExprKind::MemberExpr: {
            auto& memberExpr = llvm::cast<MemberExpr>(expr);
            if (memberExpr.getBaseExpr()->getType().isEnumType()) {
                auto type =
                    llvm::cast<EnumDecl>(memberExpr.getType().getDecl());

                auto case_ = type->getCaseByName(memberExpr.getMemberName());
                val.i32 =
                    evalExpr(*llvm::cast<IntLiteralExpr>(case_->getValue()))
                        .i32;
                val.type = Type::getInt();
                break;
            } else if (memberExpr.getBaseExpr()->getType().isTupleType()) {
                auto value = evalExpr(*memberExpr.getBaseExpr());
                value.type = memberExpr.getType();
                auto field_name = memberExpr.getMemberName();

                return (*value.tuple_value)[field_name];

            } else {
                llvm_unreachable("unimplemented");
            }
            break;
        }

        case ExprKind::BoolLiteralExpr: {
            auto& boolExpr = llvm::cast<BoolLiteralExpr>(expr);
            val.boolean = boolExpr.getValue();
            val.type = Type::getBool();
            break;
        }
        case ExprKind::ComptimeExpr: {
            auto& comptimeExpr = llvm::cast<ComptimeExpr>(expr);
            val = evalExpr(*comptimeExpr.expr);
            break;
        }
        case ExprKind::IfExpr: {
            auto& ifExpr = llvm::cast<IfExpr>(expr);
            auto cond = evalExpr(*ifExpr.getCondition());
            if (cond.boolean) {
                val = evalExpr(*ifExpr.getThenExpr());

            } else {
                val = evalExpr(*ifExpr.getElseExpr());
            }
            break;
        }

        case ExprKind::UnaryExpr: {
            auto& unaryExpr = llvm::cast<UnaryExpr>(expr);
            switch (unaryExpr.getOperator().getKind()) {
                case Token::Star: {
                    auto value = evalExpr(unaryExpr.getOperand());
                    val.type = unaryExpr.getType();

                    if (value.reference) {
                        val.ptr = value.ref->ptr;
                    } else if (value.type.isPointerType() &&
                               value.type.getPointee().isBuiltinType()) {
                        auto type = value.type.getPointee();
                        if (type.isInt())
                            val.i32 = *(int*)(value.ptr);
                        else if (type.isInt16())
                            val.i16 = *(int16_t*)(value.ptr);
                        else if (type.isInt32())
                            val.i32 = *(int32_t*)(value.ptr);
                        else if (type.isInt64())
                            val.i64 = *(int64_t*)(value.ptr);
                        else if (type.isFloat32())
                            val.f32 = *(float*)(value.ptr);
                        else if (type.isFloat64())
                            val.f64 = *(long double*)(value.ptr);
                        else if (type.isChar() || type.isInt8() ||
                                 type.isUInt8())
                            val.i8 = *(int8_t*)(value.ptr);
                        else if (type.isPointerType())
                            val.ptr = *(uint8_t**)(value.ptr);
                        else if (type.isString() ||
                                 type.getName() == "StringRef")
                            val.string = *(std::string**)(value.ptr);
                    }
                    break;
                }
                case Token::And: {
                    auto lval = evalLvalueExpr(unaryExpr.getOperand());
                    val.ref = lval;
                    val.type = unaryExpr.getType();

                    val.reference = true;

                    return val;
                }

                case Token::Minus: {
                    auto value = evalExpr(unaryExpr.getOperand());
                    if (value.type.isSigned()) {
                        val.i64 = -value.i64;
                    } else if (value.type.isUnsigned()) {
                        val.u64 = -value.u64;
                    } else if (value.type.isFloat() || value.type.isFloat32()) {
                        val.f32 = -value.f32;
                    } else if (value.type.isFloat64()) {
                        val.f64 = -value.f64;
                    } else {
                        llvm_unreachable("Unimplemented");
                    }
                    break;
                }
                case Token::Not: {
                    auto value = evalExpr(unaryExpr.getOperand());
                    if (value.type.isSigned()) {
                        if (value.type.isInt8())
                            val.i8 = ~value.i8;
                        else if (value.type.isInt16())
                            val.i16 = ~value.i16;
                        else if (value.type.isInt() || value.type.isInt32())
                            val.i32 = ~value.i32;
                        else if (value.type.isInt64())
                            val.i64 = ~value.i64;
                    } else if (value.type.isUnsigned()) {
                        if (value.type.isUInt8())
                            val.i8 = ~value.i8;
                        else if (value.type.isUInt16())
                            val.i16 = ~value.i16;
                        else if (value.type.isUInt() || value.type.isInt32())
                            val.i32 = ~value.i32;
                        else if (value.type.isUInt64())
                            val.i64 = ~value.i64;
                    } else if (value.type.isBool()) {
                        val.boolean = !value.boolean;
                    }
                    break;
                }

                default:
                    llvm_unreachable("unimplemented");
            }
            break;
        }

        case ExprKind::CallExpr: {
            auto& callExpr = llvm::cast<CallExpr>(expr);

            if (callExpr.isBuiltinConversion()) {
                auto value = evalExpr(*callExpr.getArgs().front().getValue());
                val = builtinConversion(value, expr.getType());
                break;
            }

            if (callExpr.getFunctionName() == "alloca") {
                auto size = evalExpr(*callExpr.getArgs().front().getValue());
                val.ptr = static_cast<uint8_t*>(malloc(size.i64));
                val.allocated = true;
                val.type = Type::getVoid(true).makePointer();
                break;
            }

            if (callExpr.getFunctionName() == "cast") {
                auto type = callExpr.getGenericArgs().front();
                auto cast = evalExpr(*callExpr.getArgs().front().getValue());
                val.type = type;
                if (type.isInteger() && cast.type.isFloat32()) {
                    val.i64 = static_cast<int64_t>(cast.f32);
                } else if (type.isInteger() && cast.type.isFloat64()) {
                    val.i64 = static_cast<int64_t>(cast.f64);
                } else if (type.isFloat32() && cast.type.isInteger()) {
                    val.f32 = static_cast<float>(cast.i64);
                } else if (type.isFloat64() && cast.type.isInteger()) {
                    val.f64 = static_cast<long double>(cast.i64);
                } else if ((type.isFloat32() || type.isFloat()) &&
                           cast.type.isFloat64()) {
                    val.f32 = static_cast<float>(cast.f64);
                } else if (type.isFloat64() || type.isFloat32() ||
                           type.isFloat()) {
                    val.f64 = static_cast<long double>(cast.f32);
                } else if ((type.isFloat() && type.isFloat32()) &&
                           (cast.type.isFloat() || cast.type.isFloat32())) {
                    val.f32 = cast.f32;
                } else if (type.isFloat64() && cast.type.isFloat64()) {
                    val.f64 = cast.f64;
                } else if (type.isInteger() && cast.type.isInteger()) {
                    val.u64 = cast.u64;
                } else if (type.isPointerType() && !cast.reference) {
                    val.ptr = cast.ptr;
                } else if (type.isPointerType() && cast.reference) {
                    val.ref = cast.ref;
                } else {
                    val.ptr = cast.ptr;
                }

                return val;
            }

            /*if (!callExpr.getGenericArgs().empty()) {
                jazz::error(callExpr.getLocation(),"Generic arguments not yet
            supported in compile-time context");
            }*/

            auto args = callExpr.getArgs();

            if (callExpr.getFunctionName() == "assert") {
                if (evalExpr(*args.front().getValue()).boolean == false) {
                    error(expr.getLocation(), "Assertion failed");
                } else {
                    val.type = Type::getInt();
                    val.i64 = 0;

                    break;
                }
            }

            if (callExpr.getFunctionName().startswith("__builtin")) {
                auto name = callExpr.getFunctionName();

                if (name == "__builtin_pop") {
                    auto array = evalExpr(*args[0].getValue());
                    auto value = array.array->back();
                    array.array->pop_back();
                    // auto& arrayTy = llvm::cast<ArrayType>(&array.type);

                    return val;
                } else if (name == "__builtin_push") {
                    auto array = evalExpr(*args[0].getValue());
                    auto value = evalExpr(*args[1].getValue());
                    array.array->push_back(value);
                    return val;
                } else {
                    error(expr.getLocation(), "Unknown builtin '", name, "'");
                }
            }

            auto decl = callExpr.getCalleeDecl();
            auto& fdecl = llvm::cast<FunctionDecl>(*decl);

            const auto& body = fdecl.getBody();
            beginScope();
            for (size_t i = 0; i < args.size(); i++) {
                auto value = evalExpr(*args[i].getValue());

                setComptimeLocalValue(fdecl.getParams()[i].getType(),
                                      fdecl.getParams()[i].getName(), value);
            }
            auto returned = false;
            for (const auto& statement : body) {
                evalStmt(*statement);
                if (eval_return) {
                    val.type = return_value->type;
                    if (val.type.isFloat32()) {
                        val.f32 = return_value->f32;
                    } else if (val.type.isFloat64()) {
                        val.f64 = return_value->f64;
                    } else {
                        val.i64 = return_value->i64;
                    }
                    eval_return = false;
                    returned = true;
                    break;
                }
            }

            if (!returned) {
                val.type = Type::getInt();
                val.i64 = 0;
            }
            endScope();
            val.type = fdecl.getReturnType();

            break;
        }
        default:
            auto location = expr.getLocation();
            jazz::error(location, "Can't evaluate expression at compile-time");
            break;
    }

    return val;
}

void Codegen::evalStmt(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ComptimeStmt: {
            evalStmt(stmt);
            break;
        }
        case StmtKind::SwitchStmt: {
            auto& switchStmt = llvm::cast<SwitchStmt>(stmt);
            auto value = evalExpr(switchStmt.getCondition());
            auto defaults = switchStmt.getDefaultStmts();
            auto cases = switchStmt.getCases();
            for (auto& case_ : cases) {
                auto case_value = evalExpr(*case_.getValue());
                if ((case_value == value).boolean) {
                    inline_while = true;
                    for (auto& statement : case_.getStmts()) {
                        evalStmt(*statement);
                        if (inline_break) {
                            break;
                        }
                    }
                    inline_while = false;
                    inline_break = false;
                    return;
                }
            }

            for (auto& statement : defaults) {
                evalStmt(*statement);
            }
            break;
        }

        case StmtKind::VarStmt: {
            auto& varStmt = llvm::cast<VarStmt>(stmt);
            auto initializer = varStmt.getDecl().getInitializer();
            if (!initializer->isUndefinedLiteralExpr()) {
                auto val = evalExpr(*initializer);
                val.type = initializer->getType();
                setComptimeLocalValue(initializer->getType(),
                                      varStmt.getDecl().getName(), val);
            }
            break;
        }

        case StmtKind::IfStmt: {
            auto& ifStmt = llvm::cast<IfStmt>(stmt);
            auto cond = evalExpr(ifStmt.getCondition());
            if (cond.boolean) {
                beginScope();
                for (auto& statement : ifStmt.getThenBody()) {
                    evalStmt(*statement);
                    if (statement->getKind() == StmtKind::BreakStmt ||
                        statement->getKind() == StmtKind::ReturnStmt) {
                        break;
                    }
                }
                endScope();
            } else {
                beginScope();
                for (auto& statement : ifStmt.getElseBody()) {
                    evalStmt(*statement);
                    if (statement->getKind() == StmtKind::BreakStmt ||
                        statement->getKind() == StmtKind::ReturnStmt) {
                        break;
                    }
                }
                endScope();
            }
            break;
        }

        case StmtKind::ExprStmt: {
            auto& exprStmt = llvm::cast<ExprStmt>(stmt);
            auto val = evalExpr(exprStmt.getExpr());
            val.type = exprStmt.getExpr().getType();
            break;
        }

        case StmtKind::ForStmt: {
            auto& forStmt = llvm::cast<ForStmt>(stmt);
            auto& range = forStmt.getRangeExpr();
            auto start = evalExpr(*range.getSubExprs()[1]);
            auto end = evalExpr(*range.getSubExprs()[2]);
            auto var = forStmt.getVariable();
            beginScope();
            setComptimeLocalValue(var->getType(), var->getName(), start);
            inline_while = true;
            auto should_break = false;
            while (start.i64 < end.i64 && !should_break) {
                beginScope();

                for (auto& statement : forStmt.getBody()) {
                    evalStmt(*statement);
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
                findComptimeLocal(var->getName(), var)->i64 = start.i64;
            }
            inline_while = false;
            inline_break = false;
            inline_while_continue = false;
            endScope();
            break;
        }

        case StmtKind::WhileStmt: {
            auto& whileStmt = llvm::cast<WhileStmt>(stmt);
            auto cond = evalExpr(whileStmt.getCondition());
            auto should_break = false;
            auto should_continue = false;
            while (cond.boolean) {
                if (should_break) {
                    break;
                }
                if (should_continue) {
                    continue;
                }
                beginScope();
                for (auto& statement : whileStmt.getBody()) {
                    if (statement->isContinueStmt()) {
                        continue;
                    }
                    if (statement->getKind() == StmtKind::BreakStmt ||
                        statement->getKind() == StmtKind::ReturnStmt) {
                        should_break = true;
                        std::cout << "BREAK!";
                        break;
                    }
                    evalStmt(*statement);
                    if (eval_break) {
                        should_break = true;
                        eval_break = false;
                        break;
                    }
                    if (eval_continue) {
                        eval_continue = false;
                        should_continue = true;
                        break;
                    }
                }

                endScope();
                cond = evalExpr(whileStmt.getCondition());
            }
            break;
        }
        case StmtKind::ReturnStmt: {
            auto& returnStmt = llvm::cast<ReturnStmt>(stmt);
            auto valueExpr = returnStmt.getReturnValue();
            return_value = new Value();
            if (valueExpr) {
                auto value = evalExpr(*valueExpr);
                return_value->type = value.type;
                if (value.type.isString()) {
                    return_value->string = value.string;
                } else if (value.type.isFloat32()) {
                    return_value->f32 = value.f32;
                } else if (value.type.isFloat64()) {
                    return_value->f64 = value.f64;
                } else {
                    return_value->i64 = value.i64;
                }
            } else {
                return_value->type = Type::getInt();
                return_value->i64 = 0;
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
        case StmtKind::CompoundStmt: {
            auto& compoundStmt = llvm::cast<CompoundStmt>(stmt);
            beginScope();
            for (auto& statement : compoundStmt.getBody()) {
                evalStmt(*statement);
                if (statement->isReturnStmt()) {
                    break;
                }
            }
            endScope();
            break;
        }

        case StmtKind::DeferStmt: {
            auto& deferStmt = llvm::cast<DeferStmt>(stmt);
            evalExpr(deferStmt.getExpr());
            break;
        }

        default:

            llvm_unreachable("");
    }
}
