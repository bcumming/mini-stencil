include ../../options

SOURCE=gmock-gtest-all.cc  gmock_main.cc
OBJ=gmock-gtest-all.o  gmock_main.o
INCLUDE=-Igtest -Igmock
INCLUDE=-I.

all: lib

install: lib
	cp libgmock-gtest.a ../../lib

gmock-gtest-all.o: gmock-gtest-all.cc
	$(CXX) $(CXXFLAGS) $(INCLUDE) -c gmock-gtest-all.cc

gmock_main.o: gmock_main.cc
	$(CXX) $(CXXFLAGS) $(INCLUDE) -c gmock_main.cc

lib: $(OBJ)
	ar rcs libgmock-gtest.a $(OBJ)

clean:
	rm -f *.o
	rm -f *.a

