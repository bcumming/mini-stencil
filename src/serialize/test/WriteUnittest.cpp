// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#include "gtest/gtest.h"
#include "SerializationWrapper.h"
#include <vector>
#include <fstream>
#include <cstdio>

class WriteUnittest : public ::testing::Test
{
protected:
    void SetUp()
    {
        name1 = "myDoubleField";
        name2 = "mySingleField";
        fname1 = "Field_" + name1 + ".dat";
        fname2 = "Field_" + name2 + ".dat";
        fname = "Field.dat";
        json1 = "Field_" + name1 + ".json";
        json2 = "Field_" + name2 + ".json";
        json = "Field.json";

        RemoveFiles();

        doubleField.resize(39*16*61); doubleBuffer.resize(39*16*61);
        singleField.resize(39*60); singleBuffer.resize(39*60);
    }

    void TearDown()
    {
        RemoveFiles();
    }

    void RemoveFiles()
    {
        // No matter whether the files exist or not
        std::remove(fname1.c_str());
        std::remove(fname2.c_str());
        std::remove(fname.c_str());
        std::remove(json1.c_str());
        std::remove(json2.c_str());
        std::remove(json.c_str());
    }

    void FillFields()
    {
        for (int i = 0; i < 39; ++i)
            for (int j = 0; j < 16; ++j)
                for (int k = 0; k < 61; ++k)
                {
                    int idx = i + 39*j + (39*16)*k;
                    doubleField[idx] = idx + 0.3;
                }

        for (int i = 0; i < 39; ++i)
            for (int k = 0; k < 60; ++k)
            {
                int idx = i + 39*k;
                singleField[idx] = idx + 0.7;
            }
    }

    void IncrementFields(double val)
    {

        for (std::vector<double>::iterator iter = doubleField.begin(); iter != doubleField.end(); ++iter)
            (*iter) += val;
        for (std::vector<float>::iterator iter = singleField.begin(); iter != singleField.end(); ++iter)
            (*iter) += val;
    }

    std::string name1, name2, fname1, fname2, fname, json1, json2, json;
    std::vector<double> doubleField, doubleBuffer;
    std::vector<float> singleField, singleBuffer;
};


TEST_F(WriteUnittest, WriteMultiFile)
{
    fs_Initialize(0, 1, ".", 1, "Field", 5);
    Serializer& ser = fs_Serializer();

    std::ifstream fs;
    int fileSize;

    FillFields();

    // Register two fields
    fs_RegisterField_d(name1.c_str(), name1.size(), 39, 16, 61, 1, 3, 3, 3, 3, 0, 1, 0, 0);
    fs_RegisterField_f(name2.c_str(), name2.size(), 39, 1, 60, 1, 3, 3, 0, 0, 0, 0, 0, 0);

    // Write fields
    fs_WriteData_d(name1.c_str(), name1.size(), &doubleField[0]);
    fs_WriteData_f(name2.c_str(), name2.size(), &singleField[0]);

    // Check first field
    fs.open(fname1.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(0, std::ios::beg);
    ASSERT_EQ(39*16*61*8, fileSize);

    fs.read(reinterpret_cast<char*>(&doubleBuffer[0]), 39*16*61*8);
    fs.close();

    for (int i = 0; i < 39; ++i)
        for (int j = 0; j < 16; ++j)
            for (int k = 0; k < 61; ++k)
            {
                int idx_field = i + 39*j + (39*16)*k;
                int idx_buffer = k + 61*j + (61*16)*i;
                ASSERT_EQ(doubleField[idx_field], doubleBuffer[idx_buffer]);
            }

    // Check second field
    fs.open(fname2.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(0, std::ios::beg);
    ASSERT_EQ(39*60*4, fileSize);

    fs.read(reinterpret_cast<char*>(&singleBuffer[0]), 39*60*4);
    fs.close();

    for (int i = 0; i < 39; ++i)
            for (int k = 0; k < 60; ++k)
            {
                int idx_field = i + 39*k;
                int idx_buffer = k + 60*i;
                ASSERT_EQ(singleField[idx_field], singleBuffer[idx_buffer]);
            }


    // Increment both fields by 0.8, serialize and redo the checks
    IncrementFields(0.8);

    fs_WriteData_d(name1.c_str(), name1.size(), &doubleField[0]);
    fs_WriteData_f(name2.c_str(), name2.size(), &singleField[0]);

    // Check serializer
    ASSERT_EQ(std::string("myDoubleField"), ser.indexTable().front().fields()[0]);
    ASSERT_EQ(std::string("mySingleField"), ser.indexTable().front().fields()[1]);
    ASSERT_EQ(std::string("myDoubleField"), ser.indexTable().front().fields()[2]);
    ASSERT_EQ(std::string("mySingleField"), ser.indexTable().front().fields()[3]);

    // Check first field
    fs.open(fname1.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(39*16*61*8, std::ios::beg);
    ASSERT_EQ(2*39*16*61*8, fileSize);

    fs.read(reinterpret_cast<char*>(&doubleBuffer[0]), 39*16*61*8);
    fs.close();

    for (int i = 0; i < 39; ++i)
        for (int j = 0; j < 16; ++j)
            for (int k = 0; k < 61; ++k)
            {
                int idx_field = i + 39*j + (39*16)*k;
                int idx_buffer = k + 61*j + (61*16)*i;
                ASSERT_EQ(doubleField[idx_field], doubleBuffer[idx_buffer]);
            }

    // Check second field
    fs.open(fname2.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(39*60*4, std::ios::beg);
    ASSERT_EQ(2*39*60*4, fileSize);

    fs.read(reinterpret_cast<char*>(&singleBuffer[0]), 39*60*4);
    fs.close();

    for (int i = 0; i < 39; ++i)
            for (int k = 0; k < 60; ++k)
            {
                int idx_field = i + 39*k;
                int idx_buffer = k + 60*i;
                ASSERT_EQ(singleField[idx_field], singleBuffer[idx_buffer]);
            }

    // Cleanup
    fs_Finalize();
}


TEST_F(WriteUnittest, WriteSingleFile)
{
    fs_Initialize(1, 1, ".", 1, "Field", 5);
    Serializer& ser = fs_Serializer();

    std::ifstream fs;
    int fileSize, fieldsSize = 39*16*61*8 + 39*60*4, expectedFSize = 0;

    FillFields();

    // Register two fields
    fs_RegisterField_d(name1.c_str(), name1.size(), 39, 16, 61, 1, 3, 3, 3, 3, 0, 1, 0, 0);
    fs_RegisterField_f(name2.c_str(), name2.size(), 39, 1, 60, 1, 3, 3, 0, 0, 0, 0, 0, 0);

    // Write fields
    fs_WriteData_d(name1.c_str(), name1.size(), &doubleField[0]);
    fs_WriteData_f(name2.c_str(), name2.size(), &singleField[0]);
    expectedFSize += fieldsSize;

    // Check first field
    fs.open(fname.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(0, std::ios::beg);
    ASSERT_EQ(expectedFSize, fileSize);

    fs.read(reinterpret_cast<char*>(&doubleBuffer[0]), 39*16*61*8);
    fs.close();

    for (int i = 0; i < 39; ++i)
        for (int j = 0; j < 16; ++j)
            for (int k = 0; k < 61; ++k)
            {
                int idx_field = i + 39*j + (39*16)*k;
                int idx_buffer = k + 61*j + (61*16)*i;
                ASSERT_EQ(doubleField[idx_field], doubleBuffer[idx_buffer]);
            }

    // Check second field
    fs.open(fname.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(39*16*61*8, std::ios::beg);
    ASSERT_EQ(expectedFSize, fileSize);

    fs.read(reinterpret_cast<char*>(&singleBuffer[0]), 39*60*4);
    fs.close();

    for (int i = 0; i < 39; ++i)
            for (int k = 0; k < 60; ++k)
            {
                int idx_field = i + 39*k;
                int idx_buffer = k + 60*i;
                ASSERT_EQ(singleField[idx_field], singleBuffer[idx_buffer]);
            }


    // Increment both fields by 0.8, serialize and redo the checks
    IncrementFields(0.8);

    fs_WriteData_d(name1.c_str(), name1.size(), &doubleField[0]);
    fs_WriteData_f(name2.c_str(), name2.size(), &singleField[0]);
    expectedFSize += fieldsSize;

    // Check serializer
    ASSERT_EQ(std::string("myDoubleField"), ser.indexTable().front().fields()[0]);
    ASSERT_EQ(std::string("mySingleField"), ser.indexTable().front().fields()[1]);
    ASSERT_EQ(std::string("myDoubleField"), ser.indexTable().front().fields()[2]);
    ASSERT_EQ(std::string("mySingleField"), ser.indexTable().front().fields()[3]);

    // Check first field
    fs.open(fname.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(fieldsSize, std::ios::beg);
    ASSERT_EQ(expectedFSize, fileSize);

    fs.read(reinterpret_cast<char*>(&doubleBuffer[0]), 39*16*61*8);
    fs.close();

    for (int i = 0; i < 39; ++i)
        for (int j = 0; j < 16; ++j)
            for (int k = 0; k < 61; ++k)
            {
                int idx_field = i + 39*j + (39*16)*k;
                int idx_buffer = k + 61*j + (61*16)*i;
                ASSERT_EQ(doubleField[idx_field], doubleBuffer[idx_buffer]);
            }

    // Check second field
    fs.open(fname.c_str(), std::ios::binary);
    fs.seekg(0, std::ios::end);
    fileSize = fs.tellg();
    fs.seekg(fieldsSize + 39*16*61*8, std::ios::beg);
    ASSERT_EQ(expectedFSize, fileSize);

    fs.read(reinterpret_cast<char*>(&singleBuffer[0]), 39*60*4);
    fs.close();

    for (int i = 0; i < 39; ++i)
            for (int k = 0; k < 60; ++k)
            {
                int idx_field = i + 39*k;
                int idx_buffer = k + 60*i;
                ASSERT_EQ(singleField[idx_field], singleBuffer[idx_buffer]);
            }

    // Cleanup
    fs_Finalize();
}


