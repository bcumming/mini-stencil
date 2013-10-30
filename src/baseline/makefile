FTN = ftn
FFLAGS = -O2
FFLAGS += -e Z # use c-style preprocessing

SOURCES = m_constants.f90 m_stats.f90 m_operators.f90 main.f90 m_cdf.f90 m_serializer.f90
OBJ = m_constants.o m_stats.o m_operators.o main.o m_cdf.o m_serializer.o

PAPI_PATH=/project/csstaff/bcumming/repos/papi/install/
PAPIWRAP_PATH=/scratch/santis/bcumming/papi-wrap/
LDFLAGS_FORTRAN=-L$(PAPIWRAP_PATH) -lpapi_wrap
LDFLAGS_FORTRAN+=-L$(PAPI_PATH)/lib -lpapi -lpfm
LDFLAGS_FORTRAN+=-L/opt/cray/cce/8.2.1/CC/x86-64/lib/x86-64/ -lcray-c++-rts
IFLAGS=-I$(PAPIWRAP_PATH) -DUSE_PAPI_WRAP

.SUFFIXES: .f90

all: main

.f90.o: $<
	$(FTN) $(FFLAGS) $(IFLAGS) -c $< -o $@

main: $(OBJ)
	$(FTN) $(LDFLAGS_FORTRAN) *.o -o $@

clean:
	rm -f main
	rm -f *.o
	rm -f *.i

