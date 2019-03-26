#pragma once

#ifndef INSTR_BEGIN
#define INSTR_BEGIN \
    typedef enum    \
    {
#define INSTR_END \
    }             \
    instructions;
#endif

#ifndef INSTRUCTION
#define INSTRUCTION(instr, _argc) instr,
#endif

INSTR_BEGIN

INSTRUCTION(InsIconst, 2)    // Move integer constant B to R(A)
INSTRUCTION(InsFconst, 2)    // Move float constant B to R(A)
INSTRUCTION(InsBconst, 2)    // Move boolean constant B to R(A)
INSTRUCTION(InsBytesLoad, 2) // Move bytes from pool at B to R(A)
INSTRUCTION(InsStrConst, 2)  // Move string from pool at B to R(A)
INSTRUCTION(InsNullConst, 1) // Move null constant to R(A)
INSTRUCTION(InsMov, 2)       // Move value from R(A) to R(B)

INSTRUCTION(InsAdd, 3)  // R(A) = R(B) + R(C)
INSTRUCTION(InsSub, 3)  // R(A) = R(B) - R(C)
INSTRUCTION(InsMul, 3)  // R(A) = R(B) * R(C)
INSTRUCTION(InsDiv, 3)  // R(A) = R(B) / R(C)
INSTRUCTION(InsMod, 3)  // R(A) = R(B) % R(C)
INSTRUCTION(InsShl, 3)  // R(A) = R(B) << R(C)
INSTRUCTION(InsSShr, 3) // R(A) = R(B) >> R(C)
INSTRUCTION(InsUShr, 3) // R(A) = R(B) >> R(C)
INSTRUCTION(
    InsAnd,
    3) // if R(B) and R(C) booleans: R(A) = R(B) && R(C) else R(A) = R(B) & R(C)
INSTRUCTION(
    InsOr,
    3)                 // if R(B) and R(C) booleans: R(A) = R(B) || R(C) else R(A) = R(B) | R(C)
INSTRUCTION(InsXOR, 3) // R(A) = R(B) ^ R(C)

INSTRUCTION(InsNeg, 2)            // R(A) = -R(B)
INSTRUCTION(InsNot, 2)            // R(A) = ~R(B)
INSTRUCTION(InsInvoke, -1)        // R(A) = invoke R(A) args
INSTRUCTION(InsInvoke0, 2)        // R(A) = invoke R(A) [R(B)]
INSTRUCTION(InsInvoke1, 3)        // R(A) = invoke R(A) [R(B),R(C)]
INSTRUCTION(InsInvoke2, 4)        // R(A) = invoke R(A) [R(B),R(C),R(F)]
INSTRUCTION(InsInvoke3, 5)        // R(A) = invoke R(A) [R(B),R(C),R(F),R(G)]
INSTRUCTION(InsInvoke4, 6)        // R(A) = invoke R(A) [R(B),R(C),R(F),R(G),R(H)]
INSTRUCTION(InsInvokeField, -1)   // R(A) = invoke R(A) args
INSTRUCTION(InsInvokeThis, -1)    // R(A) = invoke R(A) args
INSTRUCTION(InsInvokeClosure, -1) // R(A) = invoke R(A) args
INSTRUCTION(InsLoadGlobal, 2)     // R(A) = G(B)
INSTRUCTION(InsStoreGlobal, 2)    // G(A) = R(A)
INSTRUCTION(InsLoadThis, 2)
INSTRUCTION(InsStoreThis, 2)
INSTRUCTION(InsGetField, 3)
INSTRUCTION(InsStoreField, 3)
INSTRUCTION(InsHybridLoad, 3)
INSTRUCTION(InsHybridStore, 3)
INSTRUCTION(InsBrNz, 2)
INSTRUCTION(InsBrZ, 2)
INSTRUCTION(InsBrNull, 2)
INSTRUCTION(InsBrLt, 2)
INSTRUCTION(InsBrGt, 2)
INSTRUCTION(InsBrGte, 2)
INSTRUCTION(InsBrLte, 2)
INSTRUCTION(InsBrULt, 2)
INSTRUCTION(InsBrUGt, 2)
INSTRUCTION(InsBrUGte, 2)
INSTRUCTION(InsBrULte, 2)
INSTRUCTION(InsUDiv, 3)
INSTRUCTION(InsUMod, 3)
INSTRUCTION(InsCastToHybrid, 2)
INSTRUCTION(InsCastToFloat, 2)
INSTRUCTION(InsCastToUFloat, 2)
INSTRUCTION(InsCastToInt, 2)
INSTRUCTION(InsCastSafe, 2)
INSTRUCTION(InsCastUnsafe, 2)
INSTRUCTION(InsCastToVirt, 2)
INSTRUCTION(InsCheckNull, 1)
INSTRUCTION(InsLabel, 0)
INSTRUCTION(InsRet, 1)
INSTRUCTION(InsRetVoid, 0)
INSTRUCTION(InsThrow, 1)
INSTRUCTION(InsReThrow, 1)
INSTRUCTION(InsBranch, -1)

INSTRUCTION(InsTrap, 2)
INSTRUCTION(InsEndTrap, 1)
INSTRUCTION(InsLoadI8, 3)
INSTRUCTION(InsLoadI16, 3)
INSTRUCTION(InsLoadI32, 3)
INSTRUCTION(InsLoadMem, 3)
INSTRUCTION(InsLoadSIMDVector, 3)
INSTRUCTION(InsLoadSizedArray, 3)
INSTRUCTION(InsStoreI8, 3)
INSTRUCTION(InsStoreI16, 3)
INSTRUCTION(InsStoreI32, 3)
INSTRUCTION(InsStoreMem, 3)
INSTRUCTION(InsStoreSIMDVector, 3)
INSTRUCTION(InsStoreSizedArray, 3)

INSTRUCTION(InsInit, 1)
INSTRUCTION(InsGetArraySize, 2)
INSTRUCTION(InsRef, 2)
INSTRUCTION(InsWeakRef, 2)
INSTRUCTION(InsDeref, 2)
INSTRUCTION(InsSetRef, 2)
INSTRUCTION(InsRefData, 2)
INSTRUCTION(InsRefLoad, 3)
INSTRUCTION(InsNop, 0)
INSTRUCTION(InsLast, 0)

INSTR_END