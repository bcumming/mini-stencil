#pragma once

#ifdef __cplusplus
#include "SerializationFramework.h"

extern "C"
{
#endif
    // Init / End
    void fs_Initialize(int singleFile, int overwrite,
                       const char* basepath, int pathLength,
                       const char* basename, int nameLength);
    void fs_Finalize();

    // SavePoint management
    void fs_SetSavePointName(const char* name, int nameLength);
    void fs_PrintSavePoint();
    void fs_AddSavePointMetaInfo_i(const char*, int, int);
    void fs_AddSavePointMetaInfo_f(const char*, int, float);
    void fs_AddSavePointMetaInfo_d(const char*, int, double);
    void fs_AddSavePointMetaInfo_s(const char*, int, const char*, int);

    // Field registering
    void fs_RegisterField_i(const char* fieldName, int nameLength,
                            int iSize, int jSize, int KSize, int LSize,
                            int iMinusHalo, int iPlusHalo,
                            int jMinusHalo, int jPlusHalo,
                            int kMinusHalo, int kPlusHalo,
                            int lMinusHalo, int lPlusHalo);
    void fs_RegisterField_f(const char* fieldName, int nameLength,
                            int iSize, int jSize, int KSize, int LSize,
                            int iMinusHalo, int iPlusHalo,
                            int jMinusHalo, int jPlusHalo,
                            int kMinusHalo, int kPlusHalo,
                            int lMinusHalo, int lPlusHalo);
    void fs_RegisterField_d(const char* fieldName, int nameLength,
                            int iSize, int jSize, int KSize, int LSize,
                            int iMinusHalo, int iPlusHalo,
                            int jMinusHalo, int jPlusHalo,
                            int kMinusHalo, int kPlusHalo,
                            int lMinusHalo, int lPlusHalo);

    // Field dimension query
    void fs_GetFieldSize(const char*, int, int*, int*, int*, int*);

    // Field storing
    void fs_WriteData_i(const char* name, int namelength, const int* data);
    void fs_WriteData_f(const char* name, int namelength, const float* data);
    void fs_WriteData_d(const char* name, int namelength, const double* data);

    // Field loading
    void fs_ReadData_i(const char* name, int namelength, int* data);
    void fs_ReadData_f(const char* name, int namelength, float* data);
    void fs_ReadData_d(const char* name, int namelength, double* data);

    // debug
    void fs_PrintSerializerInfo();
#ifdef __cplusplus
}

Serializer& fs_Serializer();
SavePoint& fs_SavePoint();
#endif
