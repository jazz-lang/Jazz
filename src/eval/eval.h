#pragma once


#include "ast/type.h"
#include "ast/decl.h"
#include "ast/expr.h"

using namespace jazz;

class EValue {
public: 

    EValue(Type type = Type::getInt()) {
        val = static_cast<uint8_t*>(0);
        this->type = type;
    }

    union {
        double f64;
        long double f80;
        float f32;
    };

    uint8_t* val;
    Type type;

    EValue operator+(const EValue& rhs);
    EValue operator-(const EValue& rhs);
    EValue operator/(const EValue& rhs);
    EValue operator*(const EValue& rhs);
    EValue operator%(const EValue& rhs);
    EValue operator!=(const EValue& rhs);
    EValue operator==(const EValue& rhs);
    EValue operator>(const EValue& rhs);
    EValue operator>=(const EValue& rhs);
    EValue operator<(const EValue& rhs);
    EValue operator<=(const EValue& rhs);
    EValue operator>>(const EValue& rhs);
    EValue operator<<(const EValue& rhs);
};

struct EvalScope {
    EvalScope() {}

    void addLocalValue(std::string&& name,EValue EValue) {
        bool didInsert = localValues.emplace(std::move(name),EValue).second;
        jazz_assert(didInsert);
    }

    const std::unordered_map<std::string,EValue>& getLocalValues() const {return localValues;}

    void onScopeEnd();
    void clear();

    private: 
        std::unordered_map<std::string,EValue> localValues;
};


class Evaluator {
    EValue evalExpr(const Expr& expr);
    EValue* evalLValueExpr(const Expr& expr);
    void evalStmt(const Stmt& stmt);
    EValue* findLocal(llvm::StringRef name,const Decl* decl);
    void beginScope() {
        scopes.push_back(EvalScope());
    }

    void endScope() {
        scopes.pop_back();
    }
    std::vector<EvalScope> scopes;

    EValue* self; // this

    bool eval_while = false; 
    bool eval_for = false;
    bool eval_continue = false;
    bool eval_break = false; 
    bool eval_return = false;

    EValue* return_value; 
    
};