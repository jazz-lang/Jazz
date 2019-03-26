#pragma once

#define TYPES_BEGIN \
    typedef enum    \
    {

#define TYPES_END \
    }             \
    TypeKind;
#define TYPE(type, idx) TYPE_##type = idx,

#include "jazz.h"

TYPES_BEGIN

TYPE(VOID, 0)
TYPE(I8, 1)
TYPE(I16, 2)
TYPE(I32, 3)
TYPE(I64, 4)
TYPE(F32, 5)
TYPE(F64, 6)
TYPE(BOOL, 7)
TYPE(BYTES, 8)
TYPE(HYBRID, 9)
TYPE(FUN, 10)
TYPE(OBJ, 11)
TYPE(ARRAY, 12)
TYPE(TYPE, 13)
TYPE(REF, 14)
TYPE(VIRTUAL, 15)
TYPE(HYBRID_OBJ, 16)
TYPE(ABSTRACT, 17)
TYPE(ENUM, 18)
TYPE(FUN_METHOD, 19)
TYPE(NULL, 20)
TYPE(LAST, 21)

TYPES_END