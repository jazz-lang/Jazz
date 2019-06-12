#pragma once
#include "instructions.h"
#include "ast/type.h"

using namespace jazz;

class MValue {
public:
    union {
        int64_t i;
        float f;
        double d;
        uint8_t *value;
    };




};

class MIns {
public:
MIL_ins getKind() const {return kind;}

private: 
    MIL_ins kind;
    Type type;
};


class MIconst: public MIns {
public: 
    static bool classof(const MIns* ins) {
        return ins->getKind() >= kI8Const && ins->getKind() <= kI64Const;
    }

private: 
    uint64_t value;
    bool is_signed;
    bool eval;
};

class MBinop: public MIns {
public: 
    std::unique_ptr<MValue> lhs;
    std::unique_ptr<MValue> rhs;
    int Operator;
    bool overflowing;
};

class MCall: public MIns {
public: 
    std::unique_ptr<MValue> callee;
    std::vector<std::unique_ptr<MValue>> arguments;
};