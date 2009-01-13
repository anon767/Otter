include ../Makefile.common

SRCS = AST.cpp ASTKind.cpp  ASTUtil.cpp BitBlast.cpp SimpBool.cpp  ToCNF.cpp ToSAT.cpp Transform.cpp
OBJS = $(SRCS:.cpp=.o)

#Make the ast library for use by other modules
libast.a:	$(OBJS)
	-rm -rf $@
	$(AR) rc libast.a $(OBJS)
	$(RANLIB) libast.a

ASTKind.o:	ASTKind.kinds
	$(CXX) $(CXXFLAGS)   -c -o ASTKind.o ASTKind.cpp

# ASTKind.h and ASTKind.cpp are automatically generated
ASTKind.h ASTKind.cpp:	ASTKind.kinds genkinds.pl
	./genkinds.pl

# cnftest:	cnftest.o ToCNF.o AST.o ASTUtil.o ASTKind.o BitBlast.o AST.h
#	$(CC) $(LDFLAGS) ToCNF.o BitBlast.o ASTKind.o  ASTUtil.o AST.o  cnftest.o  -o cnftest 

# bbtest: $(OBJS)
#	$(CC) $(LDFLAGS) BitBlast.o ASTKind.o  ASTUtil.o AST.o  bbtest.o -o bbtest 

# asttest: $(OBJS)
#	$(CC) $(LDFLAGS) ASTKind.o  ASTUtil.o AST.o asttest.o  -lstdc++ -o asttest

clean:
	rm -rf *.o *~ bbtest asttest cnftest *.a  ASTKind.h ASTKind.cpp .#*

depend:
	makedepend -Y -- $(CFLAGS) -- $(SRCS)
# DO NOT DELETE

AST.o: AST.h ../AST/ASTUtil.h ASTKind.h ../sat/Solver.h
AST.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h ../sat/Heap.h
AST.o: ../sat/SolverTypes.h ../sat/VarOrder.h
asttest.o: AST.h ../AST/ASTUtil.h ../AST/ASTKind.h ../sat/Solver.h
asttest.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h
asttest.o: ../sat/Heap.h ../sat/SolverTypes.h ../sat/VarOrder.h
ASTUtil.o: ASTUtil.h
bbtest.o: AST.h ../AST/ASTUtil.h ../AST/ASTKind.h ../sat/Solver.h
bbtest.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h
bbtest.o: ../sat/Heap.h ../sat/SolverTypes.h ../sat/VarOrder.h
BitBlast.o: AST.h ../AST/ASTUtil.h ../AST/ASTKind.h ../sat/Solver.h
BitBlast.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h
BitBlast.o: ../sat/Heap.h ../sat/SolverTypes.h ../sat/VarOrder.h
cnftest.o: AST.h ../AST/ASTUtil.h ../AST/ASTKind.h ../sat/Solver.h
cnftest.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h
cnftest.o: ../sat/Heap.h ../sat/SolverTypes.h ../sat/VarOrder.h
ToCNF.o: AST.h ../AST/ASTUtil.h ../AST/ASTKind.h ../sat/Solver.h
ToCNF.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h ../sat/Heap.h
ToCNF.o: ../sat/SolverTypes.h ../sat/VarOrder.h
ToSAT.o: AST.h ../AST/ASTUtil.h ../AST/ASTKind.h ../sat/Solver.h
ToSAT.o: ../sat/SolverTypes.h ../sat/Global.h ../sat/VarOrder.h ../sat/Heap.h
ToSAT.o: ../sat/SolverTypes.h ../sat/VarOrder.h
Transform.o : AST.h ../AST/ASTUtil.h ../AST/ASTKind.h
