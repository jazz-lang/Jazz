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

#include "instructions.def"

INSTR_END
