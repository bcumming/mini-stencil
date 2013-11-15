#include "SerializationFramework.h"
#include "SerializationWrapper.h"

#include <iostream>
#include <cassert>

static SavePoint savePoint;
static Serializer serializer;

#ifdef SERIALIZATION_FILTER_EXCEPTIONS
#  define SERIALIZATION_TRY(expr) \
    try { expr; } \
    catch(SerializationException& ex) { std::cerr << "Serialization error: " << ex.what(); assert(false); }
#else
#  define SERIALIZATION_TRY(expr) expr;
#endif

/////////////////////// INITIALIZATION AND FINALIZATION ///////////////////////
void fs_Initialize(int singleFile, int overwrite,
                   const char* basepath, int pathLength,
                   const char* basename, int nameLength)
{
    std::string basepathStr, basenameStr;
    int params = 2 + (pathLength!=0) + (nameLength!=0);

    // Fill with default values the lacing parameters
    if (pathLength <= 0) basepathStr = Serializer::defaultBasepath();
    else basepathStr = std::string(basepath, pathLength);

    if (nameLength <= 0) basenameStr = Serializer::defaultBasename();
    else basenameStr = std::string(basename, nameLength);

    // Initialize and "filter out" exceptions
    SERIALIZATION_TRY(( \
        serializer.Init(singleFile>0, overwrite>0, basepathStr, basenameStr) \
    ));

#ifndef NDEBUG
    std::cout << "Initializing SerializationFramework with "
              << params << " parameters" << std::endl;
#endif
}

void fs_Finalize()
{
    SERIALIZATION_TRY(savePoint.clear());
    SERIALIZATION_TRY(serializer.clear());
#ifndef NDEBUG
    std::cout << "Finalizing SerializationFramework" << std::endl;
#endif
}







///////////////////////////// SAVEPOINT MANAGEMENT /////////////////////////////

void fs_SetSavePointName(const char* name, int nameLength)
{
    savePoint.clear();
    savePoint.set_Name(std::string(name, nameLength));
}

void fs_PrintSavePoint()
{
    std::cout << savePoint.ToString() << std::endl;
}

template<typename TValue>
static inline
void fs_AddSavePointMetaInfo(const char* key, int keyLength, TValue value)
{
    std::string keystr(key, keyLength);
    savePoint.AddMetaInfo( meta_info(keystr, value) );
}

void fs_AddSavePointMetaInfo_i(const char* key, int keyLength, int value)
{
    fs_AddSavePointMetaInfo(key, keyLength, value);
}

void fs_AddSavePointMetaInfo_f(const char* key, int keyLength, float value)
{
    fs_AddSavePointMetaInfo(key, keyLength, value);
}

void fs_AddSavePointMetaInfo_d(const char* key, int keyLength, double value)
{
    fs_AddSavePointMetaInfo(key, keyLength, value);
}

void fs_AddSavePointMetaInfo_s(const char* key, int keyLength,
                               const char* value, int valueLength)
{
    fs_AddSavePointMetaInfo(key, keyLength, std::string(value, valueLength));
}

////////////////////////////// FIELD REGISTERING //////////////////////////////

template<typename TValue>
static inline void fs_RegisterField(const char* fieldName, int nameLength,
                        int iSize, int jSize, int kSize, int lSize,
                        int iMinusHalo, int iPlusHalo,
                        int jMinusHalo, int jPlusHalo,
                        int kMinusHalo, int kPlusHalo,
                        int lMinusHalo, int lPlusHalo
                        )
{
    assert(serializer.initialized() &&
      "It is mandatory to initialize the serializer before to register fields");
    std::string namestr(fieldName, nameLength);

    SERIALIZATION_TRY(( \
        serializer.RegisterField<TValue>(namestr, iSize, jSize, kSize, lSize, \
                iMinusHalo, iPlusHalo, jMinusHalo, jPlusHalo, \
                kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo \
            ) \
    ));
}


void fs_RegisterField_i(const char* fieldName, int nameLength,
                        int iSize, int jSize, int kSize, int lSize,
                        int iMinusHalo, int iPlusHalo,
                        int jMinusHalo, int jPlusHalo,
                        int kMinusHalo, int kPlusHalo,
                        int lMinusHalo, int lPlusHalo)
{
    fs_RegisterField<int>(fieldName, nameLength, iSize, jSize, kSize, lSize,
                          iMinusHalo, iPlusHalo, jMinusHalo, jPlusHalo,
                          kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo);
}


void fs_RegisterField_f(const char* fieldName, int nameLength,
                        int iSize, int jSize, int kSize, int lSize,
                        int iMinusHalo, int iPlusHalo,
                        int jMinusHalo, int jPlusHalo,
                        int kMinusHalo, int kPlusHalo,
                        int lMinusHalo, int lPlusHalo)
{
    fs_RegisterField<float>(fieldName, nameLength, iSize, jSize, kSize, lSize,
                            iMinusHalo, iPlusHalo, jMinusHalo, jPlusHalo,
                            kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo);
}


void fs_RegisterField_d(const char* fieldName, int nameLength,
                        int iSize, int jSize, int kSize, int lSize,
                        int iMinusHalo, int iPlusHalo,
                        int jMinusHalo, int jPlusHalo,
                        int kMinusHalo, int kPlusHalo,
                        int lMinusHalo, int lPlusHalo)
{
    fs_RegisterField<double>(fieldName, nameLength, iSize, jSize, kSize, lSize,
                             iMinusHalo, iPlusHalo, jMinusHalo, jPlusHalo,
                             kMinusHalo, kPlusHalo, lMinusHalo, lPlusHalo);
}





void fs_GetFieldSize(const char* fieldName, int nameLength,
                     int* iSize, int* jSize, int* kSize, int* lSize)
{
    std::string namestr(fieldName, nameLength);

    SERIALIZATION_TRY( \
        const DataFieldInfo& fieldInfo = serializer.GetFieldInfo(namestr); \
        \
        *iSize = fieldInfo.iSize(); \
        *jSize = fieldInfo.jSize(); \
        *kSize = fieldInfo.kSize(); \
        *lSize = fieldInfo.lSize(); \
    );
}


//////////////////////////////// FIELD STORAGE ////////////////////////////////

template<typename TValue>
static inline
void fs_WriteData(const char* name, int nameLength, const TValue* data)
{
    std::string namestr(name, nameLength);
    SERIALIZATION_TRY(( serializer.Save(namestr, data, savePoint) ));
}


void fs_WriteData_i(const char* name, int namelength, const int* data)
{
    fs_WriteData(name, namelength, data);
}


void fs_WriteData_f(const char* name, int namelength, const float* data)
{
    fs_WriteData(name, namelength, data);
}


void fs_WriteData_d(const char* name, int namelength, const double* data)
{
    fs_WriteData(name, namelength, data);
}


//////////////////////////////// FIELD LOADING ////////////////////////////////

template<typename TValue>
static inline
void fs_ReadData(const char* name, int nameLength, TValue* data)
{
    std::string namestr(name, nameLength);
    SERIALIZATION_TRY(( serializer.Load(namestr, data, savePoint) ));
}


void fs_ReadData_i(const char* name, int namelength, int* data)
{
    fs_ReadData(name, namelength, data);
}


void fs_ReadData_f(const char* name, int namelength, float* data)
{
    fs_ReadData(name, namelength, data);
}


void fs_ReadData_d(const char* name, int namelength, double* data)
{
    fs_ReadData(name, namelength, data);
}


//////////////////////////////////// DEBUG ////////////////////////////////////


void fs_PrintSerializerInfo()
{
    std::cerr << serializer.ToString() << "\n";
}


///////////////////////// RETURN OBJECTS FOR C++ ONLY /////////////////////////

Serializer& fs_Serializer() { return serializer; }

SavePoint& fs_SavePoint() { return savePoint; }
