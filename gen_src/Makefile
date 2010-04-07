LINUX := ../../linux-2.6.32.9
CIL := $(abspath ../cil-tools)
GEN_SRC := $(abspath .)

LINUX32-$(shell uname -m) := linux32
LINUX32 := $(LINUX32-x86_64)

all: osck_checks

../cil-tools/doone:
	@echo "You must build"
	@echo "  osck/analysis/cil-svn"
	@echo "  osck/analysis/cil-tools"
	@exit 1

intermediates: $(LINUX)/.config ../cil-tools/doone
	rm -rf intermediates tmp.intermediates
	$(LINUX32) make -C $(LINUX) clean
	$(LINUX32) make -C $(LINUX) \
		CC="$(CIL)/invoke_gcc.py \
		-cilo $(GEN_SRC)/tmp.intermediates" \
		CC_OPT=gcc \
		vmlinux -j 8
	mv tmp.intermediates intermediates

osck_checks: intermediates ../cil-tools/main
	rm -rf osck_checks tmp.osck_checks
	mkdir tmp.osck_checks
	$(CIL)/main $(GEN_SRC)/intermediates $(GEN_SRC)/tmp.osck_checks
	mv tmp.osck_checks osck_checks

clean:
	rm -rf intermediates tmp.intermediates
	rm -rf osck_checks tmp.osck_checks
