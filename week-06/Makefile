COMPILER=diamondback
EXT=diamond
STACK=stack --allow-different-user
COMPILEREXEC=bins/$(COMPILER)

TESTS=$(patsubst input/%.$(EXT),%,$(wildcard input/*.$(EXT)))

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf32
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

.PHONY: clean $(TESTS)

# make run silent
.SILENT:

$(TESTS):%: output/%.result


output/%.anf: input/%.$(EXT)
	bins/anf $< > $@

output/%.result: output/%.run
	$< > $@ && cat $@

output/%.vresult: output/%.run
	valgrind $(LIMIT) $< > $@

output/%.run: output/%.o main.c
	clang -g -m32 -mstackrealign -o $@ $^

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.$(EXT)
	$(LIMIT) $(COMPILEREXEC) $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run output/*.log output/*.result output/*.anf
