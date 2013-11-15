include ../makefile.inc

INCLUDE=-I../libs/gmock-gtest -I../src -I$(BOOST)
LIBS=-L../lib  -lserialization -lgmock-gtest -ljson
SOURCES=InitializationUnittest.cpp SavePointsUnittest.cpp WriteUnittest.cpp

COMPILER=$(CXX) $(CXXFLAGS)

unit_test:
	$(COMPILER) $(SOURCES) $(LIBS)  -o unit_test $(INCLUDE)

install: unit_test
	cp unit_test ../bin

clean:
	rm -f *.o
	rm -f unit_test
