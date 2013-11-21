include ../options

INCLUDE=-I../libs/gmock-gtest -I../lib -I$(BOOST) -I../src
LIBS=-L../lib  -lserialization -lgmock-gtest -ljson
CLIBS=-L../lib  -lserialization -ljson -L/opt/cray/cce/8.2.1/CC/x86-64/lib/x86-64/ -lcray-c++-rts -lstdc++
SOURCES=InitializationUnittest.cpp SavePointsUnittest.cpp WriteUnittest.cpp

COMPILER=$(CXX) $(CXXFLAGS)

unit_test: $(SOURCES)
	$(COMPILER) $(SOURCES) $(LIBS)  -o unit_test $(INCLUDE)

c_test: c_test.c
	$(CC)  $(INCLUDE) c_test.c $(CLIBS) -o c_test
#	$(CC)  $(INCLUDE) $(CLIBS) c_test.c -o c_test

ftn_test: ftn_test.f90
	$(FTN) $(INCLUDE) ftn_test.f90 $(CLIBS) -o ftn_test
#	$(FTN) $(INCLUDE) $(CLIBS) ftn_test.f90 -o ftn_test

install: unit_test c_test ftn_test
	cp unit_test ../bin
	cp c_test ../bin
	cp ftn_test ../bin

clean:
	rm -f *.o *.i *.json *.dat
	rm -f unit_test c_test ftn_test

