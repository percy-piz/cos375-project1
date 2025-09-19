#include "sim.h"

using namespace std;

// RV64I without csr, environment, or fence instructions

//           31          25 24 20 19 15 14    12 11          7 6      0
// R  type: | funct7       | rs2 | rs1 | funct3 | rd          | opcode |
// I  type: | imm[11:0]          | rs1 | funct3 | rd          | opcode |
// S  type: | imm[11:5]    | rs2 | rs1 | funct3 | imm[4:0]    | opcode |
// SB type: | imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode |
// U  type: | imm[31:12]                        | rd          | opcode |
// UJ type: | imm[20|10:1|11|19:12]             | rd          | opcode |

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
                case FUNCT3_ADD:    break;
                case FUNCT3_SLT:    break;
                case FUNCT3_SLTU:   break;
                case FUNCT3_XOR:    break;
                case FUNCT3_OR:     break;
                case FUNCT3_AND:    break;

                case FUNCT3_SLL:
                    if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false;
                    break;

                case FUNCT3_SRL:   /* srli / srai */
                    if (inst.funct7 == FUNCT7_ADD) { /* srli */ }
                    else if (inst.funct7 == FUNCT7_SUB) { /* srai */ }
                    else inst.isLegal = false;
                    break;

                default: inst.isLegal = false;
            }
            break;
        }

        //  R-type ALU (add, sub, sll, slt, sltu, xor, srl, sra, or, and)
        case OP_RTYPE: {
            inst.doesArithLogic = true;
            inst.writesRd = true;
            inst.readsRs1 = true;
            inst.readsRs2 = true;

            switch (inst.funct3) {
                case FUNCT3_ADD:
                    if      (inst.funct7 == FUNCT7_ADD) { /* add */ }
                    else if (inst.funct7 == FUNCT7_SUB) { /* sub */ }
                    else inst.isLegal = false;
                    break;

                case FUNCT3_SLL:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // sll
                case FUNCT3_SLT:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // slt
                case FUNCT3_SLTU: if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // sltu
                case FUNCT3_XOR:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // xor
                case FUNCT3_SRL:
                    if      (inst.funct7 == FUNCT7_ADD) { /* srl */ }
                    else if (inst.funct7 == FUNCT7_SUB) { /* sra */ }
                    else inst.isLegal = false;
                    break;

                case FUNCT3_OR:   if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // or
                case FUNCT3_AND:  if (inst.funct7 != FUNCT7_ADD) inst.isLegal = false; break; // and
                default: inst.isLegal = false;
            }
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
            break;

        case OP_JALR:
            inst.readsRs1 = true;
            inst.writesRd = true;
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

// Collect reg operands for arith or addr gen
Instruction simOperandCollection(Instruction inst, REGS regData) {
    if (inst.readsRs1)
        inst.op1Val = regData.registers[inst.rs1]; // Read rs1 if needed
    if (inst.readsRs2)
        inst.op2Val = regData.registers[inst.rs2]; // Read rs2 if needed
    return inst;
}

// Resolve next PC whether +4 or branch/jump target
Instruction simNextPCResolution(Instruction inst) {

    inst.nextPC = inst.PC + 4;

    return inst;
}

// Perform arithmetic/logic operations
Instruction simArithLogic(Instruction inst) {
    uint64_t imm12  = inst.instruction >> 20 & 0b111111111111;
    uint64_t sext_imm12 = (imm12 & 0x800) ? (imm12 | 0xFFFFFFFFFFFFF000) : imm12;

    inst.arithResult = inst.op1Val + sext_imm12;
      
    return inst;
}

// Generate memory address for load/store instructions
Instruction simAddrGen(Instruction inst) {
    return inst;
}

// Perform memory access for load/store instructions
Instruction simMemAccess(Instruction inst, MemoryStore *myMem) {
    return inst;
}

// Write back results to registers
Instruction simCommit(Instruction inst, REGS &regData) {

    // regData here is passed by reference, so changes will be reflected in original
    regData.registers[inst.rd] = inst.arithResult;

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



