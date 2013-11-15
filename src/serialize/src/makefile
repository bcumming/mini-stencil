include ../makefile.inc

HEADERS= DataFieldInfo.h IndexTable.h IndexTableEntry.h MetaDataSerializer.h MetaInfo.h SavePoint.h SerializationExceptions.h SerializationFramework.h Serializer.h Serializer.cpp SerializerInput.h SerializerInput.impl.h SerializerOutput.h SerializerOutput.impl.h TypeName.h SerializationWrapper.h
SOURCES=DataFieldInfo.cpp IndexTable.cpp IndexTableEntry.cpp MetaDataSerializer.cpp MetaInfo.cpp SerializationWrapper.cpp
OBJ=DataFieldInfo.o IndexTable.o IndexTableEntry.o MetaDataSerializer.o MetaInfo.o SerializationWrapper.o Serializer.o

FLAGS=$(CXXFLAGS)
INCLUDE=-I../libs/libjson -I$(BOOST)

COMPILER=$(CXX) $(CXXFLAGS)

install: libserialization.a
	cp libserialization.a ../lib

DataFieldInfo.o: DataFieldInfo.cpp $(HEADERS)
	$(COMPILER) -c DataFieldInfo.cpp $(FLAGS) $(INCLUDE)

IndexTable.o: IndexTable.cpp $(HEADERS)
	$(COMPILER) -c IndexTable.cpp $(FLAGS) $(INCLUDE)

IndexTableEntry.o: IndexTableEntry.cpp $(HEADERS)
	$(COMPILER) -c IndexTableEntry.cpp $(FLAGS) $(INCLUDE)

MetaDataSerializer.o: MetaDataSerializer.cpp $(HEADERS)
	$(COMPILER) -c MetaDataSerializer.cpp $(FLAGS) $(INCLUDE)

MetaInfo.o: MetaInfo.cpp $(HEADERS)
	$(COMPILER) -c MetaInfo.cpp $(FLAGS) $(INCLUDE)

SerializationWrapper.o: SerializationWrapper.cpp $(HEADERS)
	$(COMPILER) -c SerializationWrapper.cpp $(FLAGS) $(INCLUDE)

Serializer.o: Serializer.cpp $(HEADERS)
	$(COMPILER) -c Serializer.cpp $(FLAGS) $(INCLUDE)

libserialization.a: $(OBJ)
	ar rcs libserialization.a $(OBJ)

clean:
	rm -f *.o
	rm -f *.a
