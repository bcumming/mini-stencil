// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#include "gtest/gtest.h"
#include "SerializationWrapper.h"
#include <vector>
#include <fstream>
#include <cstdio>
#include <cmath>

class ReadUnittest: public ::testing::Test {
protected:
    void SetUp() {
        IJKSize calcDomain3d, calcDomain2d;
        calcDomain3d.Init(33, 10, 60);
        calcDomain2d.Init(33, 1, 60);
        kboundary.Init(0, 1);

        double3d.Init("double3d", calcDomain3d, kboundary);
        int3d.Init("int3d", calcDomain3d, kboundary);
        double2d.Init("double2d", calcDomain2d, kboundary);
        int2d.Init("int2d", calcDomain2d, kboundary);

        size3d = double3d.storage().allocatedSize();
        size2d = double2d.storage().allocatedSize();

        int totsize3d = size3d.iSize() * size3d.jSize() * size3d.kSize();
        int totsize2d = size2d.iSize() * size2d.kSize();
        bufferDouble3d.resize(totsize3d);
        bufferInt3d.resize(totsize3d);
        bufferDouble2d.resize(totsize2d);
        bufferInt2d.resize(totsize2d);

        RemoveFiles();
    }

    void TearDown() {
        RemoveFiles();
    }

    void RemoveFiles() {
        std::remove("Field_double3d.json");
        std::remove("Field_int3d.json");
        std::remove("Field_double2d.json");
        std::remove("Field_int2d.json");
        std::remove("Field.json");

        std::remove("Field_double3d.dat");
        std::remove("Field_int3d.dat");
        std::remove("Field_double2d.dat");
        std::remove("Field_int2d.dat");
        std::remove("Field.dat");
    }

    void RegisterFields() {
        fs_RegisterField_d("double3d", 8, size3d.iSize(), size3d.jSize(),
                size3d.kSize(), 1, 3, 3, 3, 3, 0, 1, 0, 0);
        fs_RegisterField_i("int3d", 5, size3d.iSize(), size3d.jSize(),
                size3d.kSize(), 1, 3, 3, 3, 3, 0, 1, 0, 0);
        fs_RegisterField_d("double2d", 8, size2d.iSize(), 1, size2d.kSize(), 1,
                3, 3, 0, 0, 0, 1, 0, 0);
        fs_RegisterField_i("int2d", 5, size2d.iSize(), 1, size2d.kSize(), 1, 3,
                3, 0, 0, 0, 1, 0, 0);
    }

    void WriteFields(Serializer& ser, const SavePoint& sp) {
        ser.Save(double3d, sp);
        ser.Save(int3d, sp);
        ser.Save(double2d, sp);
        ser.Save(int2d, sp);
    }

    void FillFields() {
        for (int i_ = 0; i_ < size3d.iSize(); ++i_)
            for (int j_ = 0; j_ < size3d.jSize(); ++j_)
                for (int k = 0; k < size3d.kSize(); ++k) {
                    int i = i_ - cNumBoundaryLines;
                    int j = j_ - cNumBoundaryLines;
                    double3d(i, j, k) = std::sqrt(
                            static_cast<double>(i_ + j_ * k));
                    int3d(i, j, k) = i * i - j / (k + 4);
                }

        for (int i_ = 0; i_ < size2d.iSize(); ++i_)
            for (int k = 0; k < size2d.kSize(); ++k) {
                int i = i_ - cNumBoundaryLines;
                double2d(i, 0, k) = std::exp(static_cast<double>(i_ - k))
                        - 1.2 * i_;
                int2d(i, 0, k) = -k * k * (i + 2) / (1 + k);
            }
    }

    void CheckFields() {
        for (int i_ = 0; i_ < size3d.iSize(); ++i_)
            for (int j_ = 0; j_ < size3d.jSize(); ++j_)
                for (int k = 0; k < size3d.kSize(); ++k) {
                    int i = i_ - cNumBoundaryLines;
                    int j = j_ - cNumBoundaryLines;
                    int idx = i_ + size3d.iSize() * j_
                            + (size3d.iSize() * size3d.jSize()) * k;
                    EXPECT_EQ(double3d(i, j, k), bufferDouble3d[idx]);
                    EXPECT_EQ(int3d(i, j, k), bufferInt3d[idx]);
                }

        for (int i_ = 0; i_ < size2d.iSize(); ++i_)
            for (int k = 0; k < size2d.kSize(); ++k) {
                int i = i_ - cNumBoundaryLines;
                int idx = i_ + size2d.iSize() * k;
                EXPECT_EQ(double2d(i, 0, k), bufferDouble2d[idx]);
                EXPECT_EQ(int2d(i, 0, k), bufferInt2d[idx]);
            }
    }

    KBoundary kboundary;
    IJKSize size3d, size2d;
    IJKRealField double3d;
    IJKIntField int3d;
    IKRealField double2d;
    IKIntField int2d;

    std::vector<double> bufferDouble3d, bufferDouble2d;
    std::vector<int> bufferInt3d, bufferInt2d;
};

TEST_F(ReadUnittest, SameSerializer) {
    fs_Initialize(1, 1, ".", 1, "Field", 5);
    Serializer& ser = fs_Serializer();
    SavePoint& sp = fs_SavePoint();

    // Register fields
    RegisterFields();

    // Prepare savepoint
    fs_SetSavePointName("mySavePoint", 11);
    fs_AddSavePointMetaInfo_i("TimeStep", 8, -43);

    // Write and clear savepoint
    FillFields();
    WriteFields(ser, sp);
    fs_SetSavePointName("", 0);

    // Prepare savepoint for reading
    fs_SetSavePointName("mySavePoint", 11);
    fs_AddSavePointMetaInfo_i("TimeStep", 8, -43);

    // Load fields
    fs_ReadData_d("double3d", 8, &bufferDouble3d[0]);
    fs_ReadData_i("int3d", 5, &bufferInt3d[0]);
    fs_ReadData_d("double2d", 8, &bufferDouble2d[0]);
    fs_ReadData_i("int2d", 5, &bufferInt2d[0]);

    // Cleanup serializer
    fs_Finalize();

    // Check
    CheckFields();
}

TEST_F(ReadUnittest, DifferentSerializer)

{
    Serializer ser1;
    ser1.Init(false, true, ".", "Field");
    SavePoint sp1;
    sp1.set_Name("myNiceSavePoint");
    sp1.AddMetaInfo(meta_info("Pi", 3.14159));

    // Place fields into file
    FillFields();
    WriteFields(ser1, sp1);

    // Close serializer
    ser1.clear();


    // Initialize second serializer, set savepoint
    fs_Initialize(0, 0, ".", 1, "Field", 5);
    fs_SetSavePointName("myNiceSavePoint", 15);
    fs_AddSavePointMetaInfo_d("Pi", 2, 3.14159);

    // Register fields
    RegisterFields();

    // Load fields
    fs_ReadData_d("double3d", 8, &bufferDouble3d[0]);
    fs_ReadData_i("int3d", 5, &bufferInt3d[0]);
    fs_ReadData_d("double2d", 8, &bufferDouble2d[0]);
    fs_ReadData_i("int2d", 5, &bufferInt2d[0]);

    // Cleanup serializer
    fs_Finalize();

    // Check
    CheckFields();
}
