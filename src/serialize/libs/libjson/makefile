include ../../options

SOURCE_PATH=_internal/Source/

HEADERS=$(SOURCE_PATH)/internalJSONNode.h $(SOURCE_PATH)/JSONAllocator.h $(SOURCE_PATH)/JSON_Base64.h $(SOURCE_PATH)/JSONChildren.h $(SOURCE_PATH)/JSONDebug.h $(SOURCE_PATH)/JSONDefs $(SOURCE_PATH)/JSONDefs.h $(SOURCE_PATH)/JSONGlobals.h $(SOURCE_PATH)/JSONMemory.h $(SOURCE_PATH)/JSONMemoryPool.h $(SOURCE_PATH)/JSONNode.h $(SOURCE_PATH)/JSONPreparse.h $(SOURCE_PATH)/JSONSharedString.h $(SOURCE_PATH)/JSONSingleton.h $(SOURCE_PATH)/JSONStats.h $(SOURCE_PATH)/JSONStream.h $(SOURCE_PATH)/JSONValidator.h $(SOURCE_PATH)/JSONWorker.h $(SOURCE_PATH)/NumberToString.h libjson.h JSONOptions.h

SOURCE= $(SOURCE_PATH)/internalJSONNode.cpp $(SOURCE_PATH)/JSONAllocator.cpp $(SOURCE_PATH)/JSONChildren.cpp $(SOURCE_PATH)/JSONDebug.cpp $(SOURCE_PATH)/JSONIterators.cpp $(SOURCE_PATH)/JSONMemory.cpp $(SOURCE_PATH)/JSONNode.cpp $(SOURCE_PATH)/JSONNode_Mutex.cpp $(SOURCE_PATH)/JSONPreparse.cpp $(SOURCE_PATH)/JSONStream.cpp $(SOURCE_PATH)/JSONValidator.cpp $(SOURCE_PATH)/JSONWorker.cpp $(SOURCE_PATH)/JSONWriter.cpp $(SOURCE_PATH)/libjson.cpp

OBJ= internalJSONNode.o JSONAllocator.o JSONChildren.o JSONDebug.o JSONIterators.o JSONMemory.o JSONNode.o JSONNode_Mutex.o JSONPreparse.o JSONStream.o JSONValidator.o JSONWorker.o JSONWriter.o libjson.o

# compiler and flags
COMPILER=$(CXX) $(CXXFLAGS)

all: lib

install: lib
	cp libjson.a ../../lib

internalJSONNode.o: $(SOURCE_PATH)/internalJSONNode.cpp
	$(COMPILER) -c $(SOURCE_PATH)/internalJSONNode.cpp
JSONAllocator.o: $(SOURCE_PATH)/JSONAllocator.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONAllocator.cpp
JSONChildren.o: $(SOURCE_PATH)/JSONChildren.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONChildren.cpp
JSONDebug.o: $(SOURCE_PATH)/JSONDebug.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONDebug.cpp
JSONIterators.o: $(SOURCE_PATH)/JSONIterators.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONIterators.cpp
JSONMemory.o: $(SOURCE_PATH)/JSONMemory.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONMemory.cpp
JSONNode.o: $(SOURCE_PATH)/JSONNode.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONNode.cpp
JSONNode_Mutex.o: $(SOURCE_PATH)/JSONNode_Mutex.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONNode_Mutex.cpp
JSONPreparse.o: $(SOURCE_PATH)/JSONPreparse.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONPreparse.cpp
JSONStream.o: $(SOURCE_PATH)/JSONStream.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONStream.cpp
JSONValidator.o: $(SOURCE_PATH)/JSONValidator.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONValidator.cpp
JSONWorker.o: $(SOURCE_PATH)/JSONWorker.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONWorker.cpp
JSONWriter.o: $(SOURCE_PATH)/JSONWriter.cpp
	$(COMPILER) -c $(SOURCE_PATH)/JSONWriter.cpp
libjson.o: $(SOURCE_PATH)/libjson.cpp
	$(COMPILER) -c $(SOURCE_PATH)/libjson.cpp

lib: $(OBJ)
	ar rcs libjson.a $(OBJ)

clean:
	rm -f *.o *.a
