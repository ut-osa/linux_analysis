CIL=../cil-svn
OCC=ocamlopt
EXT=cmxa
OBJ=cmx

SRC=machine32.ml tools.ml castreport.ml typever.ml allocs.ml typeapi.ml main.ml
OBJS=$(SRC:.ml=.$(OBJ))

DOONE_SRC=machine32.ml doone.ml
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

machine32.ml:
	echo let machine_str = \"`$(CIL)/obj/x86_LINUX/machdep-ml32.exe --env`\" \
	> machine32.ml

clean:
	rm *.cmi *.cmx *.o .depend machine32.ml main doone

include .depend
