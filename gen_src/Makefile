LINUX := ../../linux-2.6.32.9
KBUILDD := $(abspath kbuild)
CIL := $(abspath ../cil-tools)
GEN_SRC := $(abspath .)

HOST_ARCH=$(shell uname -m)
KERNEL_ARCH=x86_64

LINUX32_x86_64_i386 := linux32
LINUX32 := ${LINUX32_$(HOST_ARCH)_$(KERNEL_ARCH)}

all: osck_checks

../cil-tools/%:
	@echo "You must build"
	@echo "  osck/analysis/cil-svn"
	@echo "  osck/analysis/cil-tools"
	@exit 1

kernel.config:
	@echo "Please put a config file in kernel.config in this directory"
	@exit 1
	
kbuild:
	mkdir -p kbuild
	$(LINUX32) make -C $(LINUX) O=$(KBUILDD) defconfig

intermediates: kbuild kernel.config ../cil-tools/doone
	rm -rf intermediates tmp.intermediates
	mkdir tmp.intermediates
	$(LINUX32) make -C kbuild clean
	cp kernel.config kbuild/.config
	$(LINUX32) make -C kbuild \
		CC="$(CIL)/invoke_gcc.py \
		-cilo $(GEN_SRC)/tmp.intermediates" \
		CC_OPT=gcc \
		vmlinux -j 8
	touch tmp.intermediates
	mv tmp.intermediates intermediates

osck_checks: intermediates ../cil-tools/main
	rm -rf osck_checks tmp.osck_checks
	mkdir tmp.osck_checks
	$(CIL)/main $(GEN_SRC)/intermediates $(GEN_SRC)/tmp.osck_checks
	mv tmp.osck_checks osck_checks

clean:
	rm -rf intermediates tmp.intermediates
	rm -rf osck_checks tmp.osck_checks
	rm -rf kbuild
