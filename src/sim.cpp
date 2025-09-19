#include "sim.h"

using namespace std;

// RV64I without csr, environment, or fence instructions

//           31           25 24 20 19 15 14   12 11           7 6     0
// R  type: | funct7       | rs2 | rs1 | funct3 | rd            | opcode |
// I  type: | imm[11:0]           | rs1 | funct3 | rd            | opcode |
// S  type: | imm[11:5]    | rs2 | rs1 | funct3 | imm[4:0]    | opcode |
// SB type: | imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode |
// U  type: | imm[31:12]                       | rd            | opcode |
// UJ type: | imm[20|10:1|11|19:12]            | rd            | opcode |

// initialize memory with program binary
bool initMemory(char *programFile, MemoryStore *myMem) {
    // open instruction file
    ifstream infile;
    infile.open(programFile, ios::binary | ios::in);

    if (!infile.is_open()) {
        fprintf(stderr, "\tError open input file\n");
        return false;
    }

    // get length of the file and read instruction file into a buffer
    infile.seekg(0, ios::end);
    int length = infile.tellg();
    infile.seekg (0, ios::beg);

    char *buf = new char[length];
    infile.read(buf, length);
    infile.close();

    int memLength = length / sizeof(buf[0]);
    int i;
    for (i = 0; i < memLength; i++) {
        myMem->setMemValue(i * BYTE_SIZE, buf[i], BYTE_SIZE);
    }

    return true;
}

// dump registers and memory
void dump(MemoryStore *myMem) {

    dumpRegisterState(regData.reg);
    dumpMemoryState(myMem);
}

// TODO All functions below (except main) are incomplete.
// Only ADDI is implemented. Your task is to complete these functions.

// Get raw instruction bits from memory
Instruction simFetch(uint64_t PC, MemoryStore *myMem) {
    // fetch current instruction
    uint64_t instruction;
    myMem->getMemValue(PC, instruction, WORD_SIZE);
    instruction = (uint32_t)instruction;

    Instruction inst;
    inst.PC = PC;
    inst.instruction = instruction;
    return inst;
}

// Determine instruction opcode, funct, reg names, and what resources to use
Instruction simDecode(Instruction inst) {
    inst.opcode = inst.instruction & 0b1111111;
    inst.funct3 = inst.instruction >> 12 & 0b111;
    inst.rd = inst.instruction >> 7 & 0b11111;
    inst.rs1 = inst.instruction >> 15 & 0b11111;
    inst.rs2 = inst.instruction >> 20 & 0b11111;
    inst.funct7 = inst.instruction >> 25 & 0b1111111;


    if (inst.instruction == 0xfeedfeed) {
        inst.isHalt = true;
        return inst; // halt instruction
    }
    if (inst.instruction == 0x00000013) {
        inst.isNop = true;
        inst.isLegal = true;
        return inst; // NOP instruction
    }
    inst.isLegal = true; // assume legal unless proven otherwise

    switch (inst.opcode) {
        case OP_INTIMM: {
            // flags for all OP_INTIMM
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = false;
        
            switch (inst.funct3) {
                case FUNCT3_ADD:   break;
                case FUNCT3_SLT:   break;
                case FUNCT3_SLTU:  break;
                case FUNCT3_XOR:   break;
                case FUNCT3_OR:    break;
                case FUNCT3_AND:   break;

                case FUNCT3_SLL:
                    if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false;
                    break;

                case FUNCT3_SRL:   /* srli / srai */
                    if (inst.funct7 == FUNCT7_ADD) { /* srli */ }
                    else if (inst.funct7 == FUNCT7_SUB) { /* srai */ }
                    else inst.isLegal = false;
                    break;

                default: inst.isLegal = false;
            }
            break;
        }

        //  R-type ALU (add, sub, sll, slt, sltu, xor, srl, sra, or, and)
        case OP_RTYPE: {
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = true;

            switch (inst.funct3) {
                case FUNCT3_ADD:
                    if       (inst.funct7 == FUNCT7_ADD) { /* add */ }
                    else if (inst.funct7 == FUNCT7_SUB) { /* sub */ }
                    else inst.isLegal = false;
                    break;

                case FUNCT3_SLL:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // sll
                case FUNCT3_SLT:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // slt
                case FUNCT3_SLTU: if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // sltu
                case FUNCT3_XOR:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // xor
                case FUNCT3_SRL:
                    if       (inst.funct7 == FUNCT7_ADD) { /* srl */ }
                    else if (inst.funct7 == FUNCT7_SUB) { /* sra */ }
                    else inst.isLegal = false;
                    break;

                case FUNCT3_OR:   if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // or
                case FUNCT3_AND:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // and
                default: inst.isLegal = false;
            }
            break;
        }
        case OP_INTIMM_32: { // e.g. 0b0011011
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = false;
            // funct3: 000=ADDIW, 001=SLLIW, 101=SRLIW/SRAIW (bit30 distinguishes)
            break;
        }

        case OP_RTYPE_32: {  // e.g. 0b0111011
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = true;
            // funct3: 000=ADDW/SUBW (funct7 distinguishes), 001=SLLW, 101=SRLW/SRAW
            break;
        }
        
        // Loads (lb, lh, lw, ld, lbu, lhu, lwu)
        case OP_LOAD:
            inst.readsRs1 = true;
            inst.writesRd = true;
            inst.readsMem = true;
            break;

        // Stores (sb, sh, sw, sd)
        case OP_STORE:
            inst.readsRs1 = true;
            inst.readsRs2 = true;
            inst.writesMem = true;
            break;

        // Branches (beq, bne, blt, bge, bltu, bgeu)
        case OP_BRANCH:
            inst.readsRs1 = true;
            inst.readsRs2 = true;
            break;

        // Jumps
        case OP_JAL:
            inst.writesRd = true;
            inst.doesArithLogic = true;
            break;

        case OP_JALR:
            inst.readsRs1 = true;
            inst.writesRd = true;
            inst.doesArithLogic = true;
            break;

        // U-type
        case OP_LUI:
            inst.doesArithLogic = true;
            inst.writesRd = true;
            break;

        case OP_AUIPC:
            inst.doesArithLogic = true;
            inst.writesRd = true;
            break;

        default:
            inst.isLegal = false;
    }
    return inst;
}

// -------- helper functions for bit manipulation ----------
static inline uint32_t getBits(uint32_t x, int hi, int lo) {
    return (x >> lo) & ((1u << (hi - lo + 1)) - 1u);
}

// Sign-extend a value that is 'width' bits wide to 64-bit
static inline int64_t signExtend64(uint64_t val, int width) {
    const uint64_t m = 1ull << (width - 1);
    return (int64_t)((val ^ m) - m);
}

// ------- Immediate builders-------------
// I-type: imm[11:0] = [31:20]
static inline int64_t immI(uint32_t inst) {
    uint32_t raw = getBits(inst, 31, 20);
    return signExtend64(raw, 12);
}

// S-type: imm[11:5]=[31:25], imm[4:0]=[11:7]
static inline int64_t immS(uint32_t inst) {
    uint32_t raw = (getBits(inst, 31, 25) << 5) | getBits(inst, 11, 7);
    return signExtend64(raw, 12);
}

// B-type: imm[12]=[31], imm[11]=[7], imm[10:5]=[30:25], imm[4:1]=[11:8], final <<1
static inline int64_t immB(uint32_t inst) {
    uint32_t raw =
        (getBits(inst,31,31) << 12) |
        (getBits(inst,7,7)   << 11) |
        (getBits(inst,30,25) << 5)  |
        (getBits(inst,11,8)  << 1);
    return signExtend64(raw, 13);
}

static inline int64_t immU(uint32_t inst) {
    uint32_t raw = inst & 0xFFFFF000u;          // imm << 12 in 32-bit space
    return (int64_t)(int32_t)raw;              // sign-extend to 64
}

// J-type: imm[20]=[31], imm[19:12]=[19:12], imm[11]=[20], imm[10:1]=[30:21], final <<1
static inline int64_t immJ(uint32_t inst) {
    uint32_t raw =
        (getBits(inst,31,31) << 20) |
        (getBits(inst,19,12) << 12) |
        (getBits(inst,20,20) << 11) |
        (getBits(inst,30,21) << 1);
    return signExtend64(raw, 21);
}


// Collect reg operands for arith or addr gen
Instruction simOperandCollection(Instruction inst, REGS regData) {
    if (inst.readsRs1) inst.op1Val = regData.registers[inst.rs1];
    if (inst.readsRs2) inst.op2Val = regData.registers[inst.rs2];

    // Add immediates where appropriate
    switch (inst.opcode) {
        case OP_INTIMM:  // addi/xori/.../slli/srli/srai
        case OP_LOAD:    // lb/lh/lw/ld/lbu/lhu/lwu
        case OP_JALR:    // jalr
            inst.op2Val = (uint64_t) immI((uint32_t)inst.instruction);
            break;

        case OP_STORE:   // sb/sh/sw/sd
            // For stores, op2Val already holds rs2 (value to store).
            // Compute address later, or here if you prefer:
            // inst.memAddress = inst.op1Val + (uint64_t) immS((uint32_t)inst.instruction);
            break;

        case OP_BRANCH:  // beq/bne/blt/bge/bltu/bgeu
            // Keep rs1/rs2 values; branch offset comes from B-imm (used in NextPC):
            // you can stash it temporarily in op2Val or another field if you like
            break;

        case OP_LUI:
            // Result is the U-imm itself
            inst.op1Val = (uint64_t) immU((uint32_t)inst.instruction);
            break;

        case OP_AUIPC:
            // Result = PC + U-imm; use ALU later, or precompute here:
            inst.op1Val = inst.PC;
            inst.op2Val = (uint64_t) immU((uint32_t)inst.instruction);
            break;

        case OP_JAL:
            // Jump offset for NextPC (use immJ in next stage)
            break;
    }
    return inst;
}

// Resolve next PC whether +4 or branch/jump target
Instruction simNextPCResolution(Instruction inst) {
    inst.nextPC = inst.PC + 4; 

    switch (inst.opcode) {
        case OP_BRANCH: {
            bool take = false;
            switch (inst.funct3) {
                case FUNCT3_BEQ:  take = (inst.op1Val == inst.op2Val); break;
                case FUNCT3_BNE:  take = (inst.op1Val != inst.op2Val); break;
                case FUNCT3_BLT:  take = ((int64_t)inst.op1Val <  (int64_t)inst.op2Val); break;
                case FUNCT3_BGE:  take = ((int64_t)inst.op1Val >= (int64_t)inst.op2Val); break;
                case FUNCT3_BLTU: take = (inst.op1Val <  inst.op2Val); break;
                case FUNCT3_BGEU: take = (inst.op1Val >= inst.op2Val); break;
            }
            if (take) inst.nextPC = inst.PC + immB((uint32_t)inst.instruction);
            break;
        }

        case OP_JAL:
            inst.nextPC = inst.PC + immJ((uint32_t)inst.instruction);
            break;

        case OP_JALR:
            inst.nextPC = (inst.op1Val + immI((uint32_t)inst.instruction)) & ~1ull;
            break;

        default:
            // leave as PC+4
            break;
    }
    return inst;
}


// Perform arithmetic/logic operations
Instruction simArithLogic(Instruction inst) {
    if (!inst.doesArithLogic) return inst;

    uint64_t a    = inst.op1Val;
    uint64_t b    = inst.op2Val;
    uint64_t res  = 0;

    switch (inst.opcode) {

        // I-type ALU: addi, slti, sltiu, xori, ori, andi, slli, srli, srai
        case OP_INTIMM: {
            uint64_t shamt = b & 0x3F; // RV64 shifts use 6 bits
            switch (inst.funct3) {
                case FUNCT3_ADD:   // ADDI
                    res = a + (int64_t)b;
                    break;
                case FUNCT3_SLT:   // SLTI
                    res = ((int64_t)a < (int64_t)b) ? 1 : 0;
                    break;
                case FUNCT3_SLTU:  // SLTIU
                    res = (a < (uint64_t)b) ? 1 : 0;
                    break;
                case FUNCT3_XOR:   // XORI
                    res = a ^ (uint64_t)b;
                    break;
                case FUNCT3_OR:    // ORI
                    res = a | (uint64_t)b;
                    break;
                case FUNCT3_AND:   // ANDI
                    res = a & (uint64_t)b;
                    break;
                case FUNCT3_SLL:   // SLLI
                    // legality checked in decode
                    res = a << shamt;
                    break;
                case FUNCT3_SRL:   // SRLI or SRAI
                    if (inst.funct7 == FUNCT7_ADD) {
                        res = a >> shamt;                                       // SRLI
                    } else {
                        res = (uint64_t)((int64_t)a >> shamt);                   // SRAI
                    }
                    break;
                default:
                    inst.isLegal = false; return inst;
            }
            break;
        }

        // R-type ALU: add, sub, sll, slt, sltu, xor, srl, sra, or, and
        case OP_RTYPE: {
            uint64_t shamt = (inst.op2Val & 0x3F); // RV64 shifts use 6 bits
            switch (inst.funct3) {
                case FUNCT3_ADD:
                    if (inst.funct7 == FUNCT7_ADD) res = a + inst.op2Val;   // ADD
                    else                           res = a - inst.op2Val;   // SUB
                    break;
                case FUNCT3_SLL:   res = a << shamt;                                    break;
                case FUNCT3_SLT:   res = ((int64_t)a < (int64_t)inst.op2Val) ? 1 : 0; break;
                case FUNCT3_SLTU:  res = (a < inst.op2Val) ? 1 : 0;                   break;
                case FUNCT3_XOR:   res = a ^ inst.op2Val;                               break;
                case FUNCT3_SRL:
                    if (inst.funct7 == FUNCT7_ADD)  res = a >> shamt;                     // SRL
                    else                            res = (uint64_t)((int64_t)a >> shamt); // SRA
                    break;
                case FUNCT3_OR:    res = a | inst.op2Val;                               break;
                case FUNCT3_AND:   res = a & inst.op2Val;                               break;
                default:
                    inst.isLegal = false; return inst;
            }
            break;
        }

        // I-type 32 bit: addiw, slliw, srliw, sraiw
        case OP_INTIMM_32: {
            uint32_t a32 = (uint32_t)(a & 0xFFFFFFFFu);
            int32_t  imm = (int32_t)b;     // immI already sign-extended
            switch (inst.funct3) {
                case FUNCT3_ADD: {         // ADDIW
                    int32_t r = (int32_t)(a32 + (uint32_t)imm);
                    res = (uint64_t)(int64_t)r; // sign-extend 32 to 64
                    break;
                }
                case FUNCT3_SLL: {         // SLLIW
                    uint32_t sh = ((uint32_t)imm) & 0x1F; // W shifts use 5 bits
                    int32_t  r  = (int32_t)(a32 << sh);
                    res = (uint64_t)(int64_t)r;
                    break;
                }
                case FUNCT3_SRL: {         // SRLIW or SRAIW
                    uint32_t sh = ((uint32_t)imm) & 0x1F;
                    bool isSRA = ((inst.instruction >> 30) & 1u) != 0;
                    int32_t r = isSRA ? ((int32_t)a32 >> sh)
                                      : (int32_t)((uint32_t)a32 >> sh);
                    res = (uint64_t)(int64_t)r;
                    break;
                }
                default:
                    inst.isLegal = false; return inst;
            }
            break;
        }

        // R-type 32 bit: addw, subw, sllw, srlw, sraw
        case OP_RTYPE_32: {
            uint32_t a32 = (uint32_t)(a & 0xFFFFFFFFu);
            uint32_t b32 = (uint32_t)(inst.op2Val & 0xFFFFFFFFu);
            switch (inst.funct3) {
                case FUNCT3_ADD: {         // ADDW or SUBW
                    int32_t r = (inst.funct7 == FUNCT7_ADD)
                                ? (int32_t)(a32 + b32)
                                : (int32_t)(a32 - b32);
                    res = (uint64_t)(int64_t)r;
                    break;
                }
                case FUNCT3_SLL: {         // SLLW
                    uint32_t sh = b32 & 0x1F;
                    int32_t  r  = (int32_t)(a32 << sh);
                    res = (uint64_t)(int64_t)r;
                    break;
                }
                case FUNCT3_SRL: {         // SRLW or SRAW
                    uint32_t sh = b32 & 0x1F;
                    int32_t r = (inst.funct7 == FUNCT7_ADD)
                                ? (int32_t)((uint32_t)a32 >> sh)   // SRLW
                                : ((int32_t)a32 >> sh);            // SRAW
                    res = (uint64_t)(int64_t)r;
                    break;
                }
                default:
                    inst.isLegal = false; return inst;
            }
            break;
        }

        case OP_LUI:
            // op1Val already holds sext(imm << 12)
            res = a;
            break;

        case OP_AUIPC:
            // op1Val = PC, op2Val = sext(imm << 12)
            res = a + b;
            break;

        case OP_JAL:
        case OP_JALR:
            // link is PC + 4
            res = inst.PC + 4;
            break;

        default:
            // branches and pure memory ops do not produce ALU results
            break;
    }

    inst.arithResult = res;
    return inst;
}

// Generate memory address for load/store instructions
Instruction simAddrGen(Instruction inst) {
    switch (inst.opcode) {
        case OP_LOAD: {
            int64_t off = immI((uint32_t)inst.instruction);
            inst.memAddress = inst.op1Val + (uint64_t)off;  // rs1 + immI
            break;
        }
        case OP_STORE: {
            int64_t off = immS((uint32_t)inst.instruction);
            inst.memAddress = inst.op1Val + (uint64_t)off;  // rs1 + immS
            break;
        }
        default:
            // other instructions don’t compute a memory address
            break;
    }
    return inst;
}

// Perform memory access for load/store instructions
Instruction simMemAccess(Instruction inst, MemoryStore *myMem) {
    // LOADS
    if (inst.readsMem) {
        uint64_t addr = inst.memAddress;

        switch (inst.funct3) {
            case FUNCT3_LB: {   // signed 8-bit
                uint8_t v = 0;
                myMem->getMemValue(addr, v, BYTE_SIZE);
                inst.memResultesult = (uint64_t)(int64_t)(int8_t)v;
                break;
            }
            case FUNCT3_LH: {   // signed 16-bit
                uint16_t v = 0;
                myMem->getMemValue(addr, v, HALF_SIZE);
                inst.memResultesult = (uint64_t)(int64_t)(int16_t)v;
                break;
            }
            case FUNCT3_LW: {   // signed 32-bit
                uint32_t v = 0;
                myMem->getMemValue(addr, v, WORD_SIZE);
                inst.memResultesult = (uint64_t)(int64_t)(int32_t)v;
                break;
            }
            case FUNCT3_LD: {   // 64-bit
                uint64_t v = 0;
                myMem->getMemValue(addr, v, DWORD_SIZE);
                inst.memResultesult = v;
                break;
            }
            case FUNCT3_LBU: {  // zero-extend 8-bit
                uint8_t v = 0;
                myMem->getMemValue(addr, v, BYTE_SIZE);
                inst.memResultesult = (uint64_t)v;
                break;
            }
            case FUNCT3_LHU: {  // zero-extend 16-bit
                uint16_t v = 0;
                myMem->getMemValue(addr, v, HALF_SIZE);
                inst.memResultesult = (uint64_t)v;
                break;
            }
            case FUNCT3_LWU: {  // zero-extend 32-bit (RV64)
                uint32_t v = 0;
                myMem->getMemValue(addr, v, WORD_SIZE);
                inst.memResultesult = (uint64_t)v;
                break;
            }
            default:
                inst.isLegal = false;
                return inst;
        }
    }

    // STORES
    if (inst.writesMem) {
        uint64_t addr = inst.memAddress;
        uint64_t val  = inst.op2Val;  // rs2 holds the store data

        switch (inst.funct3) {
            case FUNCT3_SB: {   // store 8-bit
                uint8_t v = (uint8_t)(val & 0xFFu);
                myMem->setMemValue(addr, v, BYTE_SIZE);
                break;
            }
            case FUNCT3_SH: {   // store 16-bit
                uint16_t v = (uint16_t)(val & 0xFFFFu);
                myMem->setMemValue(addr, v, HALF_SIZE);
                break;
            }
            case FUNCT3_SW: {   // store 32-bit
                uint32_t v = (uint32_t)(val & 0xFFFFFFFFu);
                myMem->setMemValue(addr, v, WORD_SIZE);
                break;
            }
            case FUNCT3_SD: {   // store 64-bit
                uint64_t v = val;
                myMem->setMemValue(addr, v, DWORD_SIZE);
                break;
            }
            default:
                inst.isLegal = false;
                return inst;
        }
    }

    return inst;
}

// Write back results to registers
Instruction simCommit(Instruction inst, REGS &regData) {
    if (!inst.isLegal || inst.isNop) return inst; // no writeback

    uint64_t value = 0;
    bool doWrite = false;

    if (inst.writesRd) {
        switch (inst.opcode) {
            case OP_LOAD:
                value = inst.memResultesult;
                doWrite = true;
                break;

            case OP_INTIMM:
            case OP_RTYPE:
            case OP_INTIMM_32:
            case OP_RTYPE_32:
            case OP_LUI:
            case OP_AUIPC:
            case OP_JAL:
            case OP_JALR:
                value = inst.arithResult;
                doWrite = true;
                break;

            default:
                // branches, stores, etc. don’t write rd
                break;
        }
    }

    // Commit only if writing is needed and rd is not x0
    if (doWrite && inst.rd != 0) {
        regData.registers[inst.rd] = value;
    }

    // Always enforce x0 = 0
    regData.registers[0] = 0;

    return inst;
}

// Simulate the whole instruction using functions above
Instruction simInstruction(uint64_t &PC, MemoryStore *myMem, REGS &regData) {
    Instruction inst = simFetch(PC, myMem);
    inst = simDecode(inst);
    if (!inst.isLegal || inst.isHalt) return inst;
    inst = simOperandCollection(inst, regData);
    inst = simNextPCResolution(inst);
    inst = simArithLogic(inst);
    inst = simAddrGen(inst);
    inst = simMemAccess(inst, myMem);
    inst = simCommit(inst, regData);
    PC = inst.nextPC;
    return inst;
}

int main(int argc, char** argv) {

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <instruction_file>\n", argv[0]);
        return -1;
    }

    // initialize memory store with buffer contents
    MemoryStore *myMem = createMemoryStore();
    if (!initMemory(argv[1], myMem)) {
        fprintf(stderr, "Failed to initialize memory with program binary.\n");
        return -1;
    }

    // initialize registers and program counter
    regData.reg = {};
    PC = 0;
    bool err = false;
    
    // start simulation
    while (!err) {
        Instruction inst = simInstruction(PC, myMem, regData);
        if (inst.isHalt) {
            // Normal dump and exit
            dump(myMem);
            return 0;
        }
        if (!inst.isLegal) {
            fprintf(stderr, "Illegal instruction encountered at PC: 0x%lx\n", inst.PC);
            err = true;
        }
    }

    // dump and exit with error
    dump(myMem);
    exit(127);
    return -1;
}



