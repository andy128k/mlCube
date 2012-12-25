NATIVE=defined

ifdef NATIVE
# native
LINKER=ocamlopt
CMO=cmx
CMA=cmxa
else
# bytecode
LINKER=ocamlc
CMO=cmo
CMA=cma
endif

TARGET=mlCube
FLAGS=-I . -I +lablgtk2
LIBS=unix.$(CMA) lablgtk.$(CMA) lablglade.$(CMA) gtkInit.$(CMO)

CMIS=geometry.cmi Cube.cmi Solver.cmi
CMOS=geometry.$(CMO) Cube.$(CMO) Solver.$(CMO) main.$(CMO)

GENSRC = Cube.ml  geometry.ml  main.ml  Solver.ml

.SUFFIXES: .ml .mli .cmi .cmo .cma .cmx .cmxa .o

all: $(TARGET)

$(TARGET): $(CMIS) $(CMOS)
	$(LINKER) $(FLAGS) -o $(TARGET) $(LIBS) $(CMOS)

build: clean clean_target all 

clean_target:
	-rm -f $(TARGET)

.ml.cmo:
	ocamlc $(FLAGS) -c $<

.ml.cmx:
	ocamlopt $(FLAGS) -c $<

.mli.cmi:
	ocamlc $(FLAGS) -c $<

clean:
	-rm -f *.o
	-rm -f *.cmx
	-rm -f *.cmo
	-rm -f *.cmi
	-rm -f *.cma
	-rm -f *~

depend: $(GENSRC)
	- cp .depend .depend.bak
	ocamldep *.mli *.ml > .depend

include .depend
