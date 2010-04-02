CIL=../cil-svn
OCC=ocamlopt
EXT=cmxa
OBJ=cmx

SRC=machine32.ml tools.ml allocs.ml typever.ml typeapi.ml main.ml
OBJS=$(SRC:.ml=.$(OBJ))

all: main

main: .depend $(OBJS)
	$(OCC) -ccopt -L$(CIL)/obj/x86_LINUX/ -o main \
		nums.$(EXT) unix.$(EXT) str.$(EXT) $(CIL)/obj/x86_LINUX/cil.$(EXT) \
		$(OBJS)

.depend: $(SRC)
	ocamldep -I $(CIL)/obj/x86_LINUX $(SRC) > .depend

%.$(OBJ): %.ml
	$(OCC) -c -I $(CIL)/obj/x86_LINUX  $<

machine32.ml:
	echo let machine_str = \"`$(CIL)/obj/x86_LINUX/machdep-ml32.exe --env`\" \
	> machine32.ml

clean:
	rm *.cmi *.cmx *.o .depend machine32.ml main

include .depend
