#include <inttypes.h>

// The memory is 64 KB large.
#define MEMORY_SIZE 0x10000

// The various sizes at which you can manipulate the memory.
enum MemEntrySize
{
    BYTE_SIZE = 1,
    HALF_SIZE = 2,
    WORD_SIZE = 4,
    DOUBLE_SIZE = 8
};

// A memory abstraction interface. Allows values to be set and retrieved at a number of
// different size granularities. The implementation is also capable of printing out memory
// values over a given address range. The memory abstraction will properly store and 
// retrieve multi-byte data respecting little-endian ordering.
class MemoryStore
{
    public:
        virtual int getMemValue(uint64_t address, uint64_t & value, MemEntrySize size) = 0;
        virtual int setMemValue(uint64_t address, uint64_t value, MemEntrySize size) = 0;
        virtual int printMemory(uint64_t startAddress, uint64_t endAddress) = 0;
        virtual ~MemoryStore() {}
};

// Creates a memory store.
extern MemoryStore *createMemoryStore();

// Dumps the section of memory relevant for the test.
extern void dumpMemoryState(MemoryStore *mem);
