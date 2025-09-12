#include <stdio.h>
#include <assert.h>
#include <fstream>
#include <iostream>
#include <string>

#include "MemoryStore.h"
#include "RegisterInfo.h"

// --------------------------------------------------------------------------
// Reg data structure
// --------------------------------------------------------------------------

union REGS {
    RegisterInfo reg;
    uint64_t registers[REG_SIZE] {0};
};

union REGS regData;

uint64_t PC;

// --------------------------------------------------------------------------
// Decode constants
// --------------------------------------------------------------------------

// TODO Complete these enums, add more if necessary

enum OPCODES {
    // I-type opcodes
    OP_INTIMM  = 0b0010011, // Integer ALU immediate instructions addi, slli, slti, sltiu, xori, srli, srai, ori, andi
    // ...
};

enum FUNCT3 {
    // For integer ALU instructions
    FUNCT3_ADD  = 0b000, // add
    // ...
};

enum RI_FUNCT7 {
    // for R type add/sub instruction
    FUNCT7_ADD     = 0b0000000, // add
    // ...
};

// --------------------------------------------------------------------------
// Bit-level manipulation helpers
// --------------------------------------------------------------------------

// TODO You may wish to declare some helper functions for bit extractions
// and sign extensions

// --------------------------------------------------------------------------
// Utilities
// --------------------------------------------------------------------------

// initialize memory with program binary
bool initMemory(char *programFile, MemoryStore *myMem);

// dump registers and memory
void dump(MemoryStore *myMem);

// --------------------------------------------------------------------------
// Simulation functions
// --------------------------------------------------------------------------

// The simulator maintains the following struct as it simulates 
// RISC-V instructions. Feel free to add more fields if needed.
struct Instruction {
    uint64_t PC = 0;
    uint64_t instruction = 0; // raw instruction binary

    bool     isHalt = false;
    bool     isLegal = false;
    bool     isNop = false;

    bool     readsMem = false;
    bool     writesMem = false;
    bool     doesArithLogic = false;
    bool     writesRd = false;
    bool     readsRs1 = false;
    bool     readsRs2 = false;

    uint64_t opcode = 0;
    uint64_t funct3 = 0;
    uint64_t funct7 = 0;
    uint64_t rd = 0;
    uint64_t rs1 = 0;
    uint64_t rs2 = 0;

    uint64_t nextPC = 0;

    uint64_t op1Val = 0;
    uint64_t op2Val = 0;

    uint64_t arithResult = 0;
    uint64_t memAddress = 0;
    uint64_t memResult = 0;
};

// The following functions are the core of the simulator. Your task is to
// complete these functions in sim.cpp. Do not modify their signatures.
// However, feel free to declare more functions if needed.

// There is no strict rule on what each function should do, but the
// following comments give suggestions.

// Get raw instruction bits from memory
Instruction simFetch(uint64_t PC, MemoryStore *myMem);

// Determine instruction opcode, funct, reg names, and what resources to use
Instruction simDecode(Instruction inst);

// Collect reg operands for arith or addr gen
Instruction simOperandCollection(Instruction inst, REGS regData);

// Resolve next PC whether +4 or branch/jump target
Instruction simNextPCResolution(Instruction inst);

// Perform arithmetic/logic operations
Instruction simArithLogic(Instruction inst);

// Generate memory address for load/store instructions
Instruction simAddrGen(Instruction inst);

// Perform memory access for load/store instructions
Instruction simMemAccess(Instruction inst, MemoryStore *myMem);

// Write back results to registers
Instruction simCommit(Instruction inst, REGS &regData);

// Simulate the whole instruction using functions above
Instruction simInstruction(uint64_t &PC, MemoryStore *myMem, REGS &regData);