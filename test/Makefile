CC=gcc
OCAMLOPT = ocamlopt -w Aelzv

## Specific tests
.PHONY: clean test

ASM_MODULES=../src/stats2.cmo	\
	../src/global.cmo	\
	../src/distglobal.cmo	\
	../src/rep.cmo		\
	../src/stringrep.cmo	\
	../src/gaussian.cmo	\
	../src/asmrep.cmo	\
	../src/fitness.cmo

LLVM_MODULES=../src/stats2.cmo	\
	../src/global.cmo	\
	../src/distglobal.cmo	\
	../src/rep.cmo		\
	../src/llvmrep.cmo	\
	../src/fitness.cmo

all: test

gcd-test/gcd.s: gcd-test/gcd.c
	$(CC) -o $@ -S $^

gcd-test/gcd.ll: gcd-test/gcd.c
	clang -S -emit-llvm -o $@ $^

%.cmx: %.ml
	$(MAKE) -C ../src/ $(MAKECMDGOALS)

asm_test: $(ASM_MODULES:.cmo=.cmx) asm_test.ml
	$(OCAMLOPT) -o $@ -I ../src bigarray.cmxa unix.cmxa str.cmxa nums.cmxa $^

asm_mut_test: $(ASM_MODULES:.cmo=.cmx) asm_mut_test.ml
	$(OCAMLOPT) -o $@ -I ../src bigarray.cmxa unix.cmxa str.cmxa nums.cmxa $^

llvm_test: $(LLVM_MODULES:.cmo=.cmx) llvm_test.ml
	$(OCAMLOPT) -o $@ -I ../src bigarray.cmxa unix.cmxa str.cmxa nums.cmxa $^

llvm_mut_test: $(LLVM_MODULES:.cmo=.cmx) llvm_mut_test.ml
	$(OCAMLOPT) -o $@ -I ../src bigarray.cmxa unix.cmxa str.cmxa nums.cmxa $^

# run all tests
test: *.test gcd-test/gcd.s asm_test asm_mut_test
	FAILED=0; \
	for test in *.test ;do \
		./$$test >/dev/null 2>/dev/null; \
		if [ "$$?" -eq "0" ];then result=PASS; \
		else result=FAIL; FAILED=$$(echo "$$FAILED+1"|bc); fi; \
		echo "=============== $$result $$test"; \
	done; echo "failed $$FAILED tests";

clean:
	rm -rf asm_test; asm_mut_test; llvm_test; llvm_mut_test; \
	$(MAKE) -C gcd-test/ $(MAKECMDGOALS);		\
	$(MAKE) -C gcd-test-string/ $(MAKECMDGOALS);	\
	$(MAKE) -C gcd-multi-test/ $(MAKECMDGOALS)
