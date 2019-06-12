#pragma once

#define MIL_INSTRUCTIONS(V) \
    V(Nop) \
    V(Null) \
    V(IsSome) \
    V(Unwrap) \
    V(Undefined) \
    V(Move) \
    V(Return) \
    V(LoadArg) \
    V(StoreArg) \
    V(LoadVarArg) \
    V(Alloca) \
    V(Not) \
    V(BinOp) \
    V(Typeof) \
    V(Sizeof) \
    V(Typename) \
    V(Call) \
    V(BrNz) \
    V(BrZ) \
    V(Br)  \
    V(Store) \
    V(Load) \
    V(LoadGlobal) \
    V(StoreGlobal) \
    V(I8Const) \
    V(I16Const) \
    V(I32Const) \
    V(I64Const) \
    V(F32Const) \
    V(F64Const) \
    V(F80Const) \


#undef DEF_INS
#define DEF_INS(name) k##name,

typedef enum {
    MIL_INSTRUCTIONS(DEF_INS)
} MIL_ins;

#undef DEF_INS

