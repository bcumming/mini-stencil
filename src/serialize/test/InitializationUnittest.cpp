// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#include "gtest/gtest.h"
#include "SerializationFramework.h"
#include "SerializationWrapper.h"

TEST(InitializationUnittest, Initialization)
{
    Serializer& ser = fs_Serializer();
    ASSERT_FALSE(ser.initialized());

    // Initialize with both parameters
    fs_Initialize(1, 0, "myDir", 5, "FileName", 8);
    ASSERT_TRUE(ser.initialized());

    // Check values
    ASSERT_TRUE(ser.singleFile());
    ASSERT_FALSE(ser.readWrite());
    ASSERT_EQ(std::string("myDir/"), ser.basepath());
    ASSERT_EQ(std::string("FileName"), ser.basename());

    // Clean and check
    fs_Finalize();
    ASSERT_FALSE(ser.initialized());


    // Initialize with directory only
    fs_Initialize(1, 1, "myDir", 5, "", 0);
    ASSERT_TRUE(ser.initialized());

    // Check values
    ASSERT_TRUE(ser.singleFile());
    ASSERT_TRUE(ser.readWrite());
    ASSERT_EQ(std::string("myDir/"), ser.basepath());
    ASSERT_EQ(Serializer::defaultBasename(), ser.basename());

    // Clean and check
    fs_Finalize();
    ASSERT_FALSE(ser.initialized());


    // Initialize with filename only
    fs_Initialize(0, 0, "", 0, "FieldName", 9);
    ASSERT_TRUE(ser.initialized());

    // Check values
    ASSERT_FALSE(ser.singleFile());
    ASSERT_FALSE(ser.readWrite());
    ASSERT_EQ(Serializer::defaultBasepath(), ser.basepath());
    ASSERT_EQ(std::string("FieldName"), ser.basename());

    // Clean and check
    fs_Finalize();
    ASSERT_FALSE(ser.initialized());


    // Initialize without optional parameters
    fs_Initialize(0, 0, "", 0, "", 0);
    ASSERT_TRUE(ser.initialized());

    // Check values
    ASSERT_FALSE(ser.singleFile());
    ASSERT_FALSE(ser.readWrite());
    ASSERT_EQ(Serializer::defaultBasepath(), ser.basepath());
    ASSERT_EQ(Serializer::defaultBasename(), ser.basename());

    // Clean and check
    fs_Finalize();
    ASSERT_FALSE(ser.initialized());
}
