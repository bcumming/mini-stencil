FTN = ftn
FFLAGS = -O2
FFLAGS += -e Z # use c-style preprocessing

SOURCES = m_constants.f90 m_operators.f90 main.f90 m_cdf.f90 m_serializer.f90
OBJ = m_constants.o m_operators.o main.o m_cdf.o m_serializer.o

.SUFFIXES: .f90

all: main

.f90.o: $<
	$(FTN) $(FFLAGS) -c $< -o $@

main: $(OBJ)
	$(FTN) *.o -o $@

clean:
	rm -f main
	rm -f *.o
	rm -f *.i
