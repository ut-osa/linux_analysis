CIL=../cil-svn
OCC=ocamlopt
EXT=cmxa
OBJ=cmx

ARCH = x86_64
BITS_x86_64 = 64
BITS_i386 = 32

BITS = ${BITS_$(ARCH)}

SRC=machine.ml tools.ml typever.ml allocs.ml typeapi.ml main.ml
OBJS=$(SRC:.ml=.$(OBJ))

DOONE_SRC=machine.ml doone.ml
DOONE_OBJS=$(DOONE_SRC:.ml=.$(OBJ))

all: main doone

main: .depend $(OBJS)
	$(OCC) -ccopt -L$(CIL)/obj/x86_LINUX/ -o main \
		nums.$(EXT) unix.$(EXT) str.$(EXT) $(CIL)/obj/x86_LINUX/cil.$(EXT) \
		$(OBJS)

doone: .depend $(DOONE_OBJS)
	$(OCC) -ccopt -L$(CIL)/obj/x86_LINUX/ -o doone \
		nums.$(EXT) unix.$(EXT) str.$(EXT) $(CIL)/obj/x86_LINUX/cil.$(EXT) \
		$(DOONE_OBJS)

.depend: $(SRC) $(DOONE_SRC)
	ocamldep -I $(CIL)/obj/x86_LINUX $^ > .depend

%.$(OBJ): %.ml
	$(OCC) -c -I $(CIL)/obj/x86_LINUX $<

machine.ml:
	echo let machine_str = \
      \"`$(CIL)/obj/x86_LINUX/machdep-ml$(BITS).exe --env`\" > machine.ml

clean:
	rm *.$(EXT) *.$(OBJ) *.o .depend $(MACHINE_ML) main doone

include .depend
