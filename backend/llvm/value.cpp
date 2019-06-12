#include "llvm.h"
#include "utils.h"
#include <iostream>

using namespace jazz;

#define BIN_OP(OPERATOR,OP)  Value Value::OPERATOR(Value& rhs) {\ 
    Value val; \
    if (type.isInteger()) { \
        if (type.isSigned()) {\
           val.i64 = i64 OP rhs.i64;\
        } else {\
            val.u64 = u64 OP rhs.u64;\
        }\
    } else if (type.isFloat()) {\
        if (type.isFloat32() && rhs.type.isFloat32()) {\
            val.f32 = f32 OP rhs.f32;\
        } else if (type.isFloat64() && rhs.type.isFloat64()) {\
            val.f64 = f64 OP rhs.f64;\
        } else {}\
    } \
    return val;\
}

#define BIN_OP_PTR(OPERATOR,OP)  Value Value::OPERATOR(Value& rhs) {\ 
    Value val; \
    if (type.isInteger()) { \
        if (type.isSigned()) {\
           val.i64 = i64 OP rhs.i64;\
        } else {\
            val.u64 = u64 OP rhs.u64;\
        }\
    } else if (type.isFloat()) {\
        if (type.isFloat32() && rhs.type.isFloat32()) {\
            val.f32 = f32 OP rhs.f32;\
        } else if (type.isFloat64() && rhs.type.isFloat64()) {\
            val.f64 = f64 OP rhs.f64;\
        } else {}\
    } else if(type.isPointerType() && rhs.type.isInteger()) {\
        val.ptr = ptr OP rhs.i64; \
    } \
    return val;\
} \



BIN_OP_PTR(operator-,-)
BIN_OP(operator/,/)
BIN_OP(operator*,*)

Value Value::operator>>(Value& rhs) {
    Value val;
    if (type.isSigned()) {
        val.i64 = i64 >> rhs.i64;
    } else {
        val.u64 = u64 >> rhs.u64;
        
    }
    val.type = type;
    return val;
}

Value Value::operator<<(Value& rhs) {
    Value val;
    if (type.isSigned()) {
        val.i64 = i64 << rhs.i64;
    } else {
        val.u64 = u64 << rhs.u64;
        
    }
    val.type = type;
    return val;
}

Value Value::operator&(Value& rhs) {
    Value val;
    if (type.isSigned()) {
        val.i64 = i64 & rhs.i64;
    } else {
        val.u64 = u64 & rhs.u64;
        
    }
    val.type = type;
    return val;
}

Value Value::operator|(Value& rhs) {
    Value val;
    if (type.isSigned()) {
        val.i64 = i64 | rhs.i64;
    } else {
        val.u64 = u64 | rhs.u64;
        
    }
    val.type = type;
    return val;
}



Value Value::operator+(Value& rhs) {
    Value val;

    if (type.isInteger()) {
        if (type.isSigned()) {
            val.i64 = i64 + rhs.i64;
        } else {
            val.u64 = u64 + rhs.u64;
        }
    } else if (type.isFloat()) {
        if (type.isFloat32() && rhs.type.isFloat32()) {
            val.f32 = f32 + rhs.f32;
        } else if (type.isFloat64() && rhs.type.isFloat64()) {
            val.f64 = f64 + rhs.f64;
        } else {
            // TODO: Convert f32 to f64 or vice versa
        }
    } else if(type.isPointerType() && rhs.type.isInteger()) {
        val.ptr = ptr + rhs.i64;
    } else if ((type.isString() || (type.isPointerType() && type.getPointee().isChar()))) {
        val.string = new std::string(*string + *rhs.string);
        val.type = Type::getChar().makePointer();
        return val;
    }
    val.type = type;
    return val;

}

Value Value::operator>(Value& rhs) {
    auto val = Value(Type::getBool());
    if (type.isInteger()) {
        val.boolean = i64 > rhs.i64;
    } else if (type.isFloat32() || type.isFloat()) {
        val.boolean = f32 > rhs.f32;
    } else if (type.isFloat64()) {
        val.boolean = f64 > rhs.f64;
    } else {
        llvm_unreachable("unimplemented");
    }

    return val;
}

Value Value::operator>=(Value& rhs) {
    auto val = Value(Type::getBool());
    if (type.isInteger()) {
        val.boolean = i64 >= rhs.i64;
    } else if (type.isFloat32() || type.isFloat()) {
        val.boolean = f32 >= rhs.f32;
    } else if (type.isFloat64()) {
        val.boolean = f64 >= rhs.f64;
    } else {
        llvm_unreachable("unimplemented");
    }

    return val;
}

Value Value::operator==(Value& rhs) {
    auto val = Value(Type::getBool());
    if (type.isInteger()) {
        val.boolean = i64 == rhs.i64;
    } else if (type.isFloat32() || type.isFloat()) {
        val.boolean = f32 == rhs.f32;
    } else if (type.isFloat64()) {
        val.boolean = f64 == rhs.f64;
    } else if (type.isString() || type.getName() == "StringRef") {
        
        val.boolean = *string == *rhs.string;
    } else {
        std::cout << type.getName() << std::endl; 
        llvm_unreachable("unimplemented");
    }
    val.type = Type::getBool();

    return val;
}

Value Value::operator!=(Value& rhs) {
    auto val = Value(Type::getBool());
    if (type.isInteger()) {
        val.boolean = i64 != rhs.i64;
    } else if (type.isFloat32() || type.isFloat()) {
        val.boolean = f32 != rhs.f32;
    } else if (type.isFloat64()) {
        val.boolean = f64 != rhs.f64;
    } else {
        llvm_unreachable("unimplemented");
    }

    return val;
}

Value Value::operator<(Value& rhs) {
    auto val = Value(Type::getBool());
    if (type.isInteger()) {
        val.boolean = i64 < rhs.i64;
    } else if (type.isFloat32() || type.isFloat()) {
        val.boolean = f32 < rhs.f32;
    } else if (type.isFloat64()) {
        val.boolean = f64 < rhs.f64;
    } else {
        printf("%i\n",type.getKind());
        llvm_unreachable("unimplemented");
    }

    return val;
}

Value Value::operator<=(Value& rhs) {
    auto val = Value(Type::getBool());
    if (type.isInteger()) {
        val.boolean = i64 <= rhs.i64;
    } else if (type.isFloat32() || type.isFloat()) {
        val.boolean = f32 <= rhs.f32;
    } else if (type.isFloat64()) {
        val.boolean = f64 <= rhs.f64;
    } else {
        llvm_unreachable("unimplemented");
    }

    return val;
}

