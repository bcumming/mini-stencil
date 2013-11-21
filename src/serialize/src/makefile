include ../options

HEADERS= DataFieldInfo.h IndexTable.h IndexTableEntry.h MetaDataSerializer.h MetaInfo.h SavePoint.h SerializationExceptions.h SerializationFramework.h Serializer.h Serializer.cpp SerializerInput.h SerializerInput.impl.h SerializerOutput.h SerializerOutput.impl.h TypeName.h SerializationWrapper.h
SOURCES=DataFieldInfo.cpp IndexTable.cpp IndexTableEntry.cpp MetaDataSerializer.cpp MetaInfo.cpp SerializationWrapper.cpp
OBJ=DataFieldInfo.o IndexTable.o IndexTableEntry.o MetaDataSerializer.o MetaInfo.o SerializationWrapper.o Serializer.o m_serializer.o

INCLUDE=-I../libs/libjson -I$(BOOST)

COMPILER=$(CXX) $(CXXFLAGS)

install: libserialization.a m_serializer.o
	cp libserialization.a ../lib

DataFieldInfo.o: DataFieldInfo.cpp $(HEADERS)
	$(COMPILER) -c DataFieldInfo.cpp $(INCLUDE)

IndexTable.o: IndexTable.cpp $(HEADERS)
	$(COMPILER) -c IndexTable.cpp $(INCLUDE)

IndexTableEntry.o: IndexTableEntry.cpp $(HEADERS)
	$(COMPILER) -c IndexTableEntry.cpp $(INCLUDE)

MetaDataSerializer.o: MetaDataSerializer.cpp $(HEADERS)
	$(COMPILER) -c MetaDataSerializer.cpp $(INCLUDE)

MetaInfo.o: MetaInfo.cpp $(HEADERS)
	$(COMPILER) -c MetaInfo.cpp $(INCLUDE)

SerializationWrapper.o: SerializationWrapper.cpp $(HEADERS)
	$(COMPILER) -c SerializationWrapper.cpp $(INCLUDE)

Serializer.o: Serializer.cpp $(HEADERS)
	$(COMPILER) -c Serializer.cpp $(INCLUDE)

m_serializer.o: m_serializer.f90
	$(FTN) $(FTNFLAGS) m_serializer.f90 -c

libserialization.a: $(OBJ)
	ar rcs libserialization.a $(OBJ)

clean:
	rm -f *.o *.i *.mod
	rm -f *.a
