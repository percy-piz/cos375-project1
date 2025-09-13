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
    OP_LOAD = 0b0000011, // Load instructions lb, lh, lw, ld, lbu, lhu, lwu
    OP_JALR = 0b1100111,  // jalr

    // S-type opcode
    OP_STORE = 0b0100011, // Store instructions sb, sh, sw, sd

    // R-type opcodes
    OP_RTYPE = 0b0110011, // Register intstructions add, sub, sll, slt, sltu, xor, srl, sra, or, and

    // SB-type opcodes
    OP_BRANCH = 0b1100011, // Branch instructions beq, bne, blt, bge, bltu, bgeu

    // U-type opcodes
    OP_AUIPC = 0b0010111, // auipc
    OP_LUI = 0b0110111,  // lui

    // UJ-type opcodes
    OP_JAL = 0b1101111  //jal
};

enum FUNCT3 {
    // For integer ALU instructions
    FUNCT3_ADD  = 0b000, // add, sub, addi
    FUNCT3_SLL = 0b001, // sll, slli
    FUNCT3_SLT = 0b010, // slt, slti
    FUNCT3_SLTU = 0b011, // sltu, sltiu
    FUNCT3_XOR = 0b100, // xor, xori
    FUNCT3_SRL = 0b101, // srl, sra, srli, srai
    FUNCT3_OR = 0b110, // or, ori
    FUNCT3_AND = 0b111, // and, andi

    // For loads
    FUNCT3_LB = 0b000, // lb
    FUNCT3_LH = 0b001, // lh
    FUNCT3_LW = 0b010, // lw
    FUNCT3_LD = 0b011, // ld
    FUNCT3_LBU = 0b100, // lbu
    FUNCT3_LHU = 0b101, // lhu
    FUNCT3_LWU = 0b110, // lwu

    // For stores
    FUNCT3_SB = 0b000, // sb
    FUNCT3_SH = 0b001, // sh
    FUNCT3_SW = 0b010, //sw
    FUNCT3_SD = 0b011, //sd

    // For branches
    FUNCT3_BEQ = 0b000, // beq
    FUNCT3_BNE = 0b001, // bne
    FUNCT3_BLT = 0b100, // blt
    FUNCT3_BGE = 0b101, // bge
    FUNCT3_BLTU = 0b110, // bltu
    FUNCT3_BGEU = 0b111, // bgeu
};

enum RI_FUNCT7 {
    // for R type add/sub instruction
    FUNCT7_ADD = 0b0000000, // add, sll, slt, sltu, xor, or, and, srli, slli
    FUNCT7_SUB = 0b0100000, // sub, sra, srai
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