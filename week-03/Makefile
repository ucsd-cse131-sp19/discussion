EXAMPLE_DIR = examples

ASM_FILES = $(wildcard $(EXAMPLE_DIR)/*.s)
EXE_FILES = $(patsubst %.s,%.run,$(ASM_FILES))
OBJ_FILES = $(patsubst %.s,%.o,$(ASM_FILES))
EXAMPLES  = $(patsubst $(EXAMPLE_DIR)/%.s,%,$(ASM_FILES))

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf32
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

.PHONY: all clean $(EXAMPLES)
.PRECIOUS: $(OBJ_FILES) $(EXE_FILES)

all: $(EXE_FILES)

$(EXAMPLES):%: $(EXAMPLE_DIR)/%.run
	@$<

%.run: %.o $(EXAMPLE_DIR)/main.c
	clang -g -m32 -o $@ $^

%.o: %.s
	nasm -f $(FORMAT) -o $@ $<

clean:
	rm -f $(EXAMPLE_DIR)/*.o $(EXAMPLE_DIR)/*.run
