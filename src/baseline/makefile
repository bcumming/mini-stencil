FTN = ftn
FFLAGS = -O2
FFLAGS += -e Z -D_SERIALIZE # use c-style preprocessing

SOURCES = m_constants.f90 m_stats.f90 m_serializer.f90 m_operators.f90 m_cdf.f90
OBJ = m_constants.o m_stats.o m_serializer.o m_operators.o m_cdf.o

#PAPI_PATH=/project/csstaff/bcumming/repos/papi/install/
#PAPIWRAP_PATH=/scratch/santis/bcumming/papi-wrap/
#LDFLAGS_FORTRAN=-L$(PAPIWRAP_PATH) -lpapi_wrap
#LDFLAGS_FORTRAN+=-L$(PAPI_PATH)/lib -lpapi -lpfm
#LDFLAGS_FORTRAN+=-L/opt/cray/cce/8.2.1/CC/x86-64/lib/x86-64/ -lcray-c++-rts
#IFLAGS=-I$(PAPIWRAP_PATH) -DUSE_PAPI_WRAP

LDFLAGS_SERIALIZE=-I../serialize/lib -L../serialize/lib  -lserialization -ljson
LDFLAGS_CRAY=-L/opt/cray/cce/8.2.1/CC/x86-64/lib/x86-64/ -lcray-c++-rts -lstdc++
LDFLAGS=$(LDFLAGS_SERIALIZE) $(LDFLAGS_CRAY)

.SUFFIXES: .f90

all: main test

.f90.o: $<
	$(FTN) $(FFLAGS) $(IFLAGS) -c $< -o $@

main: $(OBJ) main.f90
	$(FTN) $(FFLAGS) $(IFLAGS) $(LDFLAGS_FORTRAN) *.o main.f90  -o $@

test: $(OBJ) test.f90
	$(FTN) $(FFLAGS) $(IFLAGS) $(LDFLAGS) *.o test.f90  -o $@

clean:
	rm -f main test
	rm -f *.o
	rm -f *.i

