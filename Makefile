# Makefile

# Build targets:
# make sim # build the functional simulator
# make all # build the functional simulator and all tests
# make tests # build all assembly tests
# make clean $ removes sim, and all .bin and .elf files in test/

# Note: If you're having trouble getting the assembler and objcopy executables to work,
# you might need to mark those files as executables using 'chmod +x filename'

# This Makefile checks if it's being run on one of the Nobel machines
ALLOWED_HOSTS = davisson.princeton.edu compton.princeton.edu
CURRENT_HOST := $(shell hostname)

HOST_OK := $(filter $(CURRENT_HOST),$(ALLOWED_HOSTS))
ifeq ($(HOST_OK),)
    $(error This project is highly recommended to be completed on Nobel)
endif

# Compiler settings
CC = g++
# Note: All builds will contain debug information
CFLAGS = --std=c++14 -Wall -g -pedantic -O2

# Source and header files
SIM_SRC = sim.cpp
SIM_SRCS = $(addprefix src/, $(SIM_SRC))
COMMON_HDRS = $(wildcard src/*.h)
COMMON_OBJS = $(wildcard src/*.o)

ASSEMBLY_TESTS = $(wildcard test/*.s)
ASSEMBLY_TARGETS = $(ASSEMBLY_TESTS:.s=.bin)

ASSEMBLER = bin/riscv64-elf-as
OBJCOPY = bin/riscv64-elf-objcopy

# Main targets
all: sim tests

sim: $(SIM_SRCS) $(COMMON_HDRS)
	$(CC) $(CFLAGS) -o sim $(COMMON_OBJS) $(SIM_SRCS)

# Test targets
tests: $(ASSEMBLY_TARGETS)

$(ASSEMBLY_TARGETS) : test/%.bin : test/%.s
	$(ASSEMBLER) test/$*.s -o test/$*.elf
	$(OBJCOPY) test/$*.elf -j .text -O binary test/$*.bin

# Clean function
clean:
	rm -f sim
	rm -f test/*.bin test/*.elf

# Phony targets
.PHONY: all debug tests clean

# To dump elf:
# riscv64-unknown-elf-objdump -D -j .text -M no-aliases *.elf