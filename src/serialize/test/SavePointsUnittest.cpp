// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#include "gtest/gtest.h"
#include "SerializationFramework.h"
#include "SerializationWrapper.h"

class SavePointsUnittest : public ::testing::Test
{
protected:
    void SetUp()
    {
        fs_Initialize(0, 0, "", 0, "", 0);
    }

    void TearDown()
    {
        fs_Finalize();
    }
};


TEST_F(SavePointsUnittest, SavePoints)
{
    SavePoint& sp = fs_SavePoint();
    int miCheckInt;
    double miCheckDouble;
    std::string miCheckString;

    ASSERT_TRUE(sp.name().empty());
    ASSERT_TRUE(sp.metaInfo().empty());

    // Set name
    fs_SetSavePointName("mySavePoint", 11);
    ASSERT_EQ(std::string("mySavePoint"), sp.name());
    ASSERT_TRUE(sp.metaInfo().empty());

    // Add some meta info
    fs_AddSavePointMetaInfo_i("CurrentYear", 11, 1865);
    fs_AddSavePointMetaInfo_d("Pi", 2, 3.14159);

    ASSERT_EQ(2, sp.metaInfo().size());
    ASSERT_EQ(cInteger, sp.metaInfoType("CurrentYear"));
    ASSERT_EQ(cFloatDouble, sp.metaInfoType("Pi"));

    sp.metaInfoValue("CurrentYear", miCheckInt);
    ASSERT_EQ(1865, miCheckInt);

    sp.metaInfoValue("Pi", miCheckDouble);
    ASSERT_EQ(3.14159, miCheckDouble);


    // Clean and rename save point
    fs_SetSavePointName("mySecondSavePoint", 17);
    ASSERT_EQ(std::string("mySecondSavePoint"), sp.name());
    ASSERT_TRUE(sp.metaInfo().empty());

    // Add some metainfo
    fs_AddSavePointMetaInfo_s("LinuxMascot", 11, "Tux", 3);
    fs_AddSavePointMetaInfo_s("WorstOS", 7, "Window$", 7);

    ASSERT_EQ(2, sp.metaInfo().size());
    ASSERT_EQ(cString, sp.metaInfoType("LinuxMascot"));
    ASSERT_EQ(cString, sp.metaInfoType("WorstOS"));

    sp.metaInfoValue("WorstOS", miCheckString);
    ASSERT_EQ(std::string("Window$"), miCheckString);

    sp.metaInfoValue("LinuxMascot", miCheckString);
    ASSERT_EQ(std::string("Tux"), miCheckString);
}
