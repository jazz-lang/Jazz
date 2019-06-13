#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>
#include "ast/decl.h"
#include "ast/expr.h"
#include "ast/stmt.h"
#include "semck/typecheck.h"

namespace jazz {

class Module;
struct Type;
class Typechecker;
class Codegen;

class Value;

class ValueArray {
   public:
    std::vector<Value> values;
    Type type;
};

class Value {
   public:
    Type type;

    union {
        bool boolean;
        int8_t i8;
        int16_t i16;
        int32_t i32;
        int64_t i64;
        uint8_t u8;
        uint16_t u16;
        uint32_t u32;
        uint64_t u64;
        long double f64;
        float f32;
        llvm::StringMap<Value>* tuple_value;
        std::vector<Value>* array;
        std::string* string;
        uint8_t* ptr;
        Value* ref;
    };

    bool allocated;
    bool reference = false;
    ~Value() {}
    Value(const Value& val) {
        type = val.type;
        ptr = val.ptr;
        reference = val.reference;
        allocated = val.allocated;
    }

    Value(Type type = Type::getVoid()) : type(type) {
        i64 = 0;
        reference = false;
        allocated = false;
    }
    Value operator+(Value& rhs);
    Value operator-(Value& rhs);
    Value operator*(Value& rhs);
    Value operator/(Value& rhs);
    Value operator%(Value& rhs);
    Value& operator=(const Value& right) {
        if (right.type.isString()) {
            string = right.string;
            return *this;
        }
        ptr = right.ptr;
        return *this;
    }
    Value operator<(Value& rhs);
    Value operator>=(Value& rhs);
    Value operator>(Value& rhs);
    Value operator<=(Value& rhs);
    Value operator==(Value& rhs);
    Value operator!=(Value& rhs);
    Value operator>>(Value& rhs);
    Value operator<<(Value& rhs);
    Value operator&(Value& rhs);
    Value operator|(Value& rhs);
};

struct ComptimeScope {
    ComptimeScope(Codegen& irGenerator) : irGenerator(&irGenerator) {}

    void addLocalValue(std::string&& name, Value value) {
        bool didInsert = localValues.emplace(std::move(name), value).second;
        jazz_assert(didInsert);
    }

    const std::unordered_map<std::string, Value>& getLocalValues() const {
        return localValues;
    }

    void onScopeEnd();
    void clear();

   private:
    Codegen* irGenerator;
    std::unordered_map<std::string, Value> localValues;
};

struct Scope {
    Scope(Codegen& irGenerator) : irGenerator(&irGenerator) {}
    void addDeferredExpr(const Expr& expr) {
        deferredExprs.emplace_back(&expr);
    }
    void addDeinitToCall(llvm::Function* deinit, llvm::Value* value, Type type,
                         const Decl* decl) {
        deinitsToCall.emplace_back(DeferredDeinit{deinit, value, type, decl});
    }
    void addLocalValue(std::string&& name, llvm::Value* value) {
        bool didInsert = localValues.emplace(std::move(name), value).second;
        jazz_assert(didInsert);
    }
    const std::unordered_map<std::string, llvm::Value*>& getLocalValues()
        const {
        return localValues;
    }
    void onScopeEnd();
    void clear();

   private:
    struct DeferredDeinit {
        llvm::Function* function;
        llvm::Value* value;
        Type type;
        const Decl* decl;
    };

    llvm::SmallVector<const Expr*, 8> deferredExprs;
    llvm::SmallVector<DeferredDeinit, 8> deinitsToCall;
    std::unordered_map<std::string, llvm::Value*> localValues;
    Codegen* irGenerator;
};

class Codegen {
   public:
    Codegen();

    llvm::Module& compile(const Module& sourceModule);
    llvm::Value* codegenExpr(const Expr& expr);
    llvm::Type* toIR(Type type, Position location = Position());
    llvm::LLVMContext& getLLVMContext() { return ctx; }
    llvm::IRBuilder<>& getBuilder() { return builder; }
    std::vector<std::unique_ptr<llvm::Module>> getGeneratedModules() {
        return std::move(generatedModules);
    }

   private:
    friend struct Scope;

    using UnaryCreate = llvm::Value* (llvm::IRBuilder<>::*)(llvm::Value*,
                                                            const llvm::Twine&,
                                                            bool, bool);
    using BinaryCreate0 = llvm::Value* (
        llvm::IRBuilder<>::*)(llvm::Value*, llvm::Value*, const llvm::Twine&);
    using BinaryCreate1 =
        llvm::Value* (llvm::IRBuilder<>::*)(llvm::Value*, llvm::Value*,
                                            const llvm::Twine&, bool);
    using BinaryCreate2 =
        llvm::Value* (llvm::IRBuilder<>::*)(llvm::Value*, llvm::Value*,
                                            const llvm::Twine&, bool, bool);

    void codegenFunctionBody(const FunctionDecl& decl,
                             llvm::Function& function);
    void createDeinitCall(llvm::Function* deinit, llvm::Value* valueToDeinit,
                          Type type, const Decl* decl);

    void setLocalValue(Type type, std::string name, llvm::Value* value,
                       const Decl* decl);
    void setComptimeLocalValue(Type type, std::string name, Value value);
    llvm::Value* findValue(llvm::StringRef name, const Decl* decl);
    Value* findComptimeLocal(llvm::StringRef name, const Decl* decl);
    llvm::Value* codegenVarExpr(const VarExpr& expr);
    llvm::Value* codegenLvalueVarExpr(const VarExpr& expr);
    llvm::Value* codegenStringExpr(const StringLiteralExpr& expr);
    llvm::Value* codegenCharacterExpr(const CharacterLiteralExpr& expr);
    llvm::Value* codegenIntExpr(const IntLiteralExpr& expr);
    llvm::Value* codegenFloatExpr(const FloatLiteralExpr& expr);
    llvm::Value* codegenBoolExpr(const BoolLiteralExpr& expr);
    llvm::Value* codegenNullExpr(const NullLiteralExpr& expr);
    llvm::Value* codegenUndefinedExpr(const UndefinedLiteralExpr& expr);
    llvm::Value* codegenArrayExpr(const ArrayLiteralExpr& expr);
    llvm::Value* codegenTupleExpr(const TupleExpr& expr);
    llvm::Value* codegenImplicitNullComparison(llvm::Value* operand);
    llvm::Value* codegenNot(const UnaryExpr& expr);
    llvm::Value* codegenUnaryExpr(const UnaryExpr& expr);
    llvm::Value* codegenLvalueUnaryExpr(const UnaryExpr& expr);
    llvm::Value* codegenInc(const UnaryExpr& expr);
    llvm::Value* codegenDec(const UnaryExpr& expr);
    llvm::Value* codegenBinaryOp(llvm::Value* lhs, llvm::Value* rhs,
                                 BinaryCreate0 create);
    llvm::Value* codegenBinaryOp(llvm::Value* lhs, llvm::Value* rhs,
                                 BinaryCreate1 create);
    llvm::Value* codegenBinaryOp(llvm::Value* lhs, llvm::Value* rhs,
                                 BinaryCreate2 create);
    llvm::Value* codegenLogicalAnd(const Expr& left, const Expr& right);
    llvm::Value* codegenLogicalOr(const Expr& left, const Expr& right);
    llvm::Value* codegenBinaryOp(Token::Kind op, llvm::Value* lhs,
                                 llvm::Value* rhs, Type lhsType);
    llvm::Value* codegenShortCircuitBinaryOp(BinaryOperator op, const Expr& lhs,
                                             const Expr& rhs);
    llvm::Value* codegenBinaryExpr(const BinaryExpr& expr);
    void codegenAssignment(const BinaryExpr& expr);
    llvm::Value* codegenExprPass(const Expr& expr, llvm::Type* targetType);
    llvm::Value* codegenBuiltinConversion(const Expr& expr, Type type);
    void codegenAssert(llvm::Value* condition, Position location,
                       llvm::StringRef message = "Assertion failed");
    llvm::Value* codegenCallExpr(const CallExpr& expr,
                                 llvm::AllocaInst* thisAllocaForInit = nullptr);
    llvm::Value* codegenBuiltinCast(const CallExpr& expr);
    llvm::Value* codegenSizeofExpr(const SizeofExpr& expr);
    llvm::Value* codegenAddressofExpr(const AddressofExpr& expr);
    llvm::Value* codegenMemberAccess(llvm::Value* baseValue, Type memberType,
                                     llvm::StringRef memberName);
    llvm::Value* codegenLvalueMemberExpr(const MemberExpr& expr);
    llvm::Value* codegenMemberExpr(const MemberExpr& expr);
    llvm::Value* codegenTupleElementAccess(const MemberExpr& expr);
    llvm::Value* codegenLvalueSubscriptExpr(const SubscriptExpr& expr);
    llvm::Value* codegenSubscriptExpr(const SubscriptExpr& expr);
    llvm::Value* codegenUnwrapExpr(const UnwrapExpr& expr);
    llvm::Value* codegenLambdaExpr(const LambdaExpr& expr);
    llvm::Value* codegenIfExpr(const IfExpr& expr);
    llvm::Value* codegenLvalueExpr(const Expr& expr);
    llvm::Value* comptimeValueToLLVMVal(const Value& value);
    llvm::Constant* comptimeValueToLLVMValue(const Value& value);
    Value evalExpr(const Expr& expr);
    Value* evalLvalueExpr(const Expr& expr);
    Value evalVarExpr(const VarExpr& expr);
    Value evalBinaryExpr(const BinaryExpr& expr);
    Value evalBinaryOp(Token::Kind op, Value& lhs, Value& rhs, Type lhsType,
                       Position location = Position());
    Value evalUnaryOp(Token::Kind op, Value& lhs);
    void evalVarStmt(const VarStmt& stmt);
    void evalIfStmt(const IfStmt& stmt);
    void evalWhileStmt(const WhileStmt& stmt);
    void evalStmt(const Stmt& stmt);
    void codegenDeferredExprsAndDeinitCallsForReturn();
    void codegenBlock(llvm::ArrayRef<std::unique_ptr<Stmt>> stmts,
                      llvm::BasicBlock* destination,
                      llvm::BasicBlock* continuation);
    void codegenReturnStmt(const ReturnStmt& stmt);
    void codegenVarStmt(const VarStmt& stmt);
    void codegenIfStmt(const IfStmt& ifStmt);
    void codegenSwitchStmt(const SwitchStmt& switchStmt);
    void codegenWhileStmt(const WhileStmt& whileStmt);
    void codegenBreakStmt(const BreakStmt&);
    void codegenContinueStmt(const ContinueStmt&);
    llvm::Value* codegenAssignmentLHS(const Expr* lhs, const Expr* rhs);
    void codegenCompoundStmt(const CompoundStmt& stmt);
    void codegenStmt(const Stmt& stmt);

    void codegenDecl(const Decl& decl);
    void codegenFunctionDecl(const FunctionDecl& decl);
    llvm::StructType* codegenTypeDecl(const TypeDecl& decl);
    llvm::Value* codegenVarDecl(const VarDecl& decl);

    llvm::Value* getFunctionForCall(const CallExpr& call);
    FunctionDecl* getFunctionDecl(const CallExpr& call);
    llvm::Function* getFunctionProto(
        const FunctionDecl& decl,
        llvm::StringMap<Value> compile_time = llvm::StringMap<Value>());
    llvm::AllocaInst* createEntryBlockAlloca(Type type, const Decl* decl,
                                             llvm::Value* arraySize = nullptr,
                                             const llvm::Twine& name = "");
    llvm::Value* createLoad(llvm::Value* value);
    std::vector<llvm::Type*> getFieldTypes(const TypeDecl& decl);
    llvm::Type* getBuiltinType(llvm::StringRef name);
    llvm::Type* getLLVMTypeForPassing(const TypeDecl& typeDecl,
                                      bool isMutating);
    llvm::Value* getArrayLength(const Expr& object, Type objectType);

    void beginScope();
    void endScope();
    void deferEvaluationOf(const Expr& expr);
    DeinitDecl* getDefaultDeinitializer(const TypeDecl& typeDecl);
    void deferDeinitCall(llvm::Value* valueToDeinit, Type type,
                         const Decl* decl);
    Scope& globalScope() { return scopes.front(); }

   private:
    class FunctionInstantiation {
       public:
        FunctionInstantiation(const FunctionDecl& decl,
                              llvm::Function* function)
            : decl(decl), function(function) {}
        const FunctionDecl& getDecl() const { return decl; }
        llvm::Function* getFunction() const { return function; }

       private:
        const FunctionDecl& decl;
        llvm::Function* function;
    };

   private:
    std::vector<Scope> scopes;
    std::vector<ComptimeScope> comptimeScopes;
    llvm::LLVMContext ctx;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::vector<std::unique_ptr<llvm::Module>> generatedModules;
    llvm::BasicBlock::iterator lastAlloca;
    llvm::StringMap<const FunctionDecl&> functions =
        llvm::StringMap<const FunctionDecl&>();
    llvm::StringMap<FunctionInstantiation> functionInstantiations;
    std::vector<std::unique_ptr<Decl>> helperDecls;
    llvm::StringMap<std::pair<llvm::StructType*, const TypeDecl*>> structs;
    const Decl* currentDecl;

    llvm::SmallVector<llvm::BasicBlock*, 4> breakTargets;
    llvm::SmallVector<llvm::BasicBlock*, 4> continueTargets;

    bool eval_return = false;
    bool eval_continue = false;
    bool eval_break = false;
    bool inline_while = false;
    bool inline_break = false;
    bool inline_while_continue = false;
    bool eval_function = false;  // true if evaluating function
    Value* return_value = nullptr;
};

}  // namespace jazz
