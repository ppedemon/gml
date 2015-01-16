
LEX       = alex
HAPPY     = happy

GHC       = ghc
GHC_FLAGS = -package std -package lang -O -fvia-C -O2-for-C

.PHONY: depend clean
.SUFFIXES: .o .hs .hi .x .y

.o.hi:
	@:
	
.hs.o:
	$(GHC) -c $< $(GHC_FLAGS)
	
.x.hs:
	$(LEX) $<
	
.y.hs:
	$(HAPPY) $<

ALEX_INPUT  = GMLLexer.x
HAPPY_INPUT = GMLParser.y

SRCS =  Alex.hs      \
	GMLSyn.hs    \
	GML.hs       \
	Eval.hs      \
	Utils.hs     \
	Point.hs     \
	Vector.hs    \
	Matrix.hs    \
	Object.hs    \
	Intersect.hs \
	Surface.hs   \
	Render.hs    \
	Light.hs     \
	Simplify.hs

ALL_SRCS = $(SRCS) $(ALEX_INPUT:.x=.hs) $(HAPPY_INPUT:.y=.hs)

OBJS = $(ALL_SRCS:.hs=.o)

depend: $(ALL_SRCS)
	ghc -M $(GHC_FLAGS) $(ALL_SRCS)

gml: $(ALEX_INPUT) $(HAPPY_INPUT) $(OBJS)
	@rm -f $@
	$(GHC) -o $@ $(GHC_FLAGS) $(OBJS)
	@strip $@

clean:
	@rm -f *.o *.hi *.hi-boot

gmlprof:: GHC_FLAGS += -prof -auto-all
gmlprof:: gml

# DO NOT DELETE: Beginning of Haskell dependencies
Alex.o : Alex.hs
GMLSyn.o : GMLSyn.hs
GML.o : GML.hs
GML.o : ./Eval.hi
GML.o : ./GMLParser.hi
GML.o : ./GMLLexer.hi
Eval.o : Eval.hs
Eval.o : ./Render.hi
Eval.o : ./Object.hi
Eval.o : ./GMLSyn.hi
Eval.o : ./Light.hi
Eval.o : ./Utils.hi
Eval.o : ./Point.hi
Utils.o : Utils.hs
Point.o : Point.hs
Vector.o : Vector.hs
Vector.o : ./Point.hi
Matrix.o : Matrix.hs
Matrix.o : ./Vector.hi
Matrix.o : ./Point.hi
Matrix.o : ./Utils.hi
Object.o : Object.hs
Object.o : ./Matrix.hi
Object.o : ./Vector.hi
Object.o : ./Point.hi
Intersect.o : Intersect.hs
Intersect.o : ./Object.hi
Intersect.o : ./Utils.hi
Intersect.o : ./Simplify.hi
Intersect.o : ./Matrix.hi
Intersect.o : ./Vector.hi
Intersect.o : ./Point.hi
Surface.o : Surface.hs
Surface.o : ./Object.hi
Surface.o : ./Point.hi
Render.o : Render.hs
Render.o : ./Simplify.hi
Render.o : ./Intersect.hi
Render.o : ./Surface.hi
Render.o : ./Object.hi
Render.o : ./Matrix.hi
Render.o : ./Vector.hi
Render.o : ./Light.hi
Render.o : ./Point.hi
Render.o : ./Utils.hi
Light.o : Light.hs
Light.o : ./Intersect.hi
Light.o : ./Object.hi
Light.o : ./Vector.hi
Light.o : ./Point.hi
Light.o : ./Utils.hi
Simplify.o : Simplify.hs
Simplify.o : ./Utils.hi
Simplify.o : ./Object.hi
Simplify.o : ./Matrix.hi
Simplify.o : ./Vector.hi
Simplify.o : ./Point.hi
GMLLexer.o : GMLLexer.hs
GMLLexer.o : ./GMLSyn.hi
GMLLexer.o : ./Alex.hi
GMLParser.o : GMLParser.hs
GMLParser.o : ./GMLLexer.hi
GMLParser.o : ./GMLSyn.hi
# DO NOT DELETE: End of Haskell dependencies
