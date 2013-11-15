// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#include "gtest/gtest.h"
#include "SerializationWrapper.h"


TEST(RegistrationUnittest, Registration)
{
    fs_Initialize(0, 0, "", 0, "", 0);
    int checkI, checkJ, checkK, checkL;

    Serializer& ser = fs_Serializer();
    ASSERT_TRUE(ser.fieldInfos().empty());

    fs_RegisterField_d("myField", 7, 39, 16, 61, 1, 3, 3, 3, 3, 0, 1, 0, 0);
    fs_RegisterField_f("mySecondField", 13, 39, 1, 61, 1, 3, 3, 0, 0, 0, 1, 0, 0);
    fs_RegisterField_i("myThirdField", 12, 1, 1, 60, 1, 0, 0, 0, 0, 0, 0, 0, 0);
    ASSERT_EQ(3, ser.fieldInfos().size());

    // Check first field
    const DataFieldInfo& field1 = ser.fieldInfos().find("myField")->second;
    ASSERT_EQ(std::string("myField"), field1.name());
    ASSERT_EQ(std::string("double"), field1.type());
    ASSERT_EQ(8, field1.bytesPerElement());
    ASSERT_EQ(3, field1.rank());
    ASSERT_EQ(39, field1.iSize());
    ASSERT_EQ(16, field1.jSize());
    ASSERT_EQ(61, field1.kSize());
    ASSERT_EQ(1, field1.lSize());
    ASSERT_EQ(33, field1.calculationDomain().iSize());
    ASSERT_EQ(10, field1.calculationDomain().jSize());
    ASSERT_EQ(60, field1.calculationDomain().kSize());
    ASSERT_EQ(3, field1.iMinusHaloSize());
    ASSERT_EQ(3, field1.jMinusHaloSize());
    ASSERT_EQ(0, field1.kMinusHaloSize());
    ASSERT_EQ(0, field1.lMinusHaloSize());
    ASSERT_EQ(3, field1.iPlusHaloSize());
    ASSERT_EQ(3, field1.jPlusHaloSize());
    ASSERT_EQ(1, field1.kPlusHaloSize());
    ASSERT_EQ(0, field1.lPlusHaloSize());
    ASSERT_EQ(39*16*61*8, field1.fieldLength());

    fs_GetFieldSize("myField", 7, &checkI, &checkJ, &checkK, &checkL);
    ASSERT_EQ(39, checkI);
    ASSERT_EQ(16, checkJ);
    ASSERT_EQ(61, checkK);
    ASSERT_EQ( 1, checkL);

    // Check second field
    const DataFieldInfo& field2 = ser.fieldInfos().find("mySecondField")->second;
    ASSERT_EQ(std::string("mySecondField"), field2.name());
    ASSERT_EQ(std::string("float"), field2.type());
    ASSERT_EQ(4, field2.bytesPerElement());
    ASSERT_EQ(2, field2.rank());
    ASSERT_EQ(39, field2.iSize());
    ASSERT_EQ(1, field2.jSize());
    ASSERT_EQ(61, field2.kSize());
    ASSERT_EQ(1, field2.lSize());
    ASSERT_EQ(33, field2.calculationDomain().iSize());
    ASSERT_EQ(1, field2.calculationDomain().jSize());
    ASSERT_EQ(60, field2.calculationDomain().kSize());
    ASSERT_EQ(3, field2.iMinusHaloSize());
    ASSERT_EQ(0, field2.jMinusHaloSize());
    ASSERT_EQ(0, field2.kMinusHaloSize());
    ASSERT_EQ(0, field2.lMinusHaloSize());
    ASSERT_EQ(3, field2.iPlusHaloSize());
    ASSERT_EQ(0, field2.jPlusHaloSize());
    ASSERT_EQ(1, field2.kPlusHaloSize());
    ASSERT_EQ(0, field2.lPlusHaloSize());
    ASSERT_EQ(39*61*4, field2.fieldLength());

    fs_GetFieldSize("mySecondField", 13, &checkI, &checkJ, &checkK, &checkL);
    ASSERT_EQ(39, checkI);
    ASSERT_EQ( 1, checkJ);
    ASSERT_EQ(61, checkK);
    ASSERT_EQ( 1, checkL);

    // Check third field
    const DataFieldInfo& field3 = ser.fieldInfos().find("myThirdField")->second;
    ASSERT_EQ(std::string("myThirdField"), field3.name());
    ASSERT_EQ(std::string("int"), field3.type());
    ASSERT_EQ(sizeof(int), field3.bytesPerElement());
    ASSERT_EQ(1, field3.rank());
    ASSERT_EQ(1, field3.iSize());
    ASSERT_EQ(1, field3.jSize());
    ASSERT_EQ(60, field3.kSize());
    ASSERT_EQ(1, field3.lSize());
    ASSERT_EQ(1, field3.calculationDomain().iSize());
    ASSERT_EQ(1, field3.calculationDomain().jSize());
    ASSERT_EQ(60, field3.calculationDomain().kSize());
    ASSERT_EQ(0, field3.iMinusHaloSize());
    ASSERT_EQ(0, field3.jMinusHaloSize());
    ASSERT_EQ(0, field3.kMinusHaloSize());
    ASSERT_EQ(0, field3.lMinusHaloSize());
    ASSERT_EQ(0, field3.iPlusHaloSize());
    ASSERT_EQ(0, field3.jPlusHaloSize());
    ASSERT_EQ(0, field3.kPlusHaloSize());
    ASSERT_EQ(0, field3.lPlusHaloSize());
    ASSERT_EQ(60*sizeof(int), field3.fieldLength());

    fs_GetFieldSize("myThirdField", 12, &checkI, &checkJ, &checkK, &checkL);
    ASSERT_EQ( 1, checkI);
    ASSERT_EQ( 1, checkJ);
    ASSERT_EQ(60, checkK);
    ASSERT_EQ( 1, checkL);

    // Close
    fs_Finalize();
    ASSERT_TRUE(ser.fieldInfos().empty());
}
