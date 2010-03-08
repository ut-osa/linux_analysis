CIL=../cil-svn
OCC=ocamlopt
EXT=cmxa
OBJ=cmx

all: main

main: main.$(OBJ)
	$(OCC) -ccopt -L$(CIL)/obj/x86_LINUX/ -o main \
		nums.$(EXT) unix.$(EXT) str.$(EXT) $(CIL)/obj/x86_LINUX/cil.$(EXT) \
		main.$(OBJ)

main.$(OBJ): main.ml
	$(OCC) -c -I $(CIL)/obj/x86_LINUX/ main.ml

clean:
	rm *.cmi *.cmx *.o main
