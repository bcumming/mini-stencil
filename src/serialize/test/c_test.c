/*
 * simple application that tests the c-wrapper in a c code
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <SerializationWrapper.h>

int TEST(int condition, const char *string){
    if(condition)
        printf("[TEST] PASS: %s\n", string);
    else
        printf("[TEST] FAIL: %s\n", string);

    return condition ? 1 : 0;
}

void register_fields(int iDim, int jDim, int kDim, int iHalo, int jHalo, int kHalo){
    // register one double field
    fs_RegisterField_d( "dField", 6,
                        iDim, jDim, kDim, 1, // i, j, k, l dimensions
                        iHalo, iHalo,  // i halo
                        jHalo, jHalo,  // j halo
                        kHalo, kHalo,  // k halo
                        0, 0); // l halo
    fs_RegisterField_i( "iField", 6,
                        iDim, jDim, kDim, 1, // i, j, k, l dimensions
                        iHalo, iHalo,  // i halo
                        jHalo, jHalo,  // j halo
                        kHalo, kHalo,  // k halo
                        0, 0); // l halo
}

void update_savepoint(const char *name, int value){
    fs_SetSavePointName(name, strlen(name));
    fs_AddSavePointMetaInfo_i("timestep", 8, value);
}

// test that initialization works
// returns 0 on success
// returns number of failed tests on failure
int test_init(void){
    printf("=================================\n");
    printf("test init\n");
    printf("=================================\n");
    int tests_passed = 0;
    int tests = 0;
    int checkI, checkJ, checkK, checkL;

    // initialize framwork
    fs_Initialize(1, 1, "", 0, "test", 4);

    // register 3 fields
    fs_RegisterField_d("myField", 7, 39, 16, 61, 1, 3, 3, 3, 3, 0, 1, 0, 0);
    fs_RegisterField_f("mySecondField", 13, 39, 1, 61, 1, 3, 3, 0, 0, 0, 1, 0, 0);
    fs_RegisterField_i("myThirdField", 12, 1, 1, 60, 1, 0, 0, 0, 0, 0, 0, 0, 0);

    // validate that correct dimensions are returned
    fs_GetFieldSize("myField", 7, &checkI, &checkJ, &checkK, &checkL);
    tests_passed += TEST(checkI==39, "i dimension"); tests++;
    tests_passed += TEST(checkJ==16, "j dimension"); tests++;
    tests_passed += TEST(checkK==61, "k dimension"); tests++;
    tests_passed += TEST(checkL==1,  "l dimension"); tests++;

    // finalize framework
    fs_Finalize();

    return (tests-tests_passed);
}

// test that writing to file works
// returns 0 on success
// returns number of failed tests on failure
int test_write(void){
    printf("=================================\n");
    printf("test write\n");
    printf("=================================\n");
    int tests_passed = 0;
    int tests = 0;
    int iDim=8, jDim=8, kDim=2;
    int iHalo=2, jHalo=2, kHalo=0;
    int iStride=iDim+2*iHalo;
    int jStride=jDim+2*jHalo;
    int len=iDim*jDim*kDim;
    int i;

    double *data_d = malloc(sizeof(double)*len);
    int    *data_i = malloc(sizeof(int)*len);

    // initialize framwork
    fs_Initialize(0, 1, "./", 2, "ctest", 5);
    register_fields(iDim,jDim,kDim,iHalo,jHalo,kHalo);

    // initialize data fields
    for(i=0; i<len; i++){
        data_d[i] = (double)i;
        data_i[i] = -i;
    }

    // Prepare savepoint
    update_savepoint("first",1);

    // write data fields
    fs_WriteData_d("dField", 6, data_d);
    fs_WriteData_i("iField", 6, data_i);

    // update data fields
    for(i=0; i<len; i++){
        data_d[i] += (double)i;
        data_i[i] += -i;
    }

    // Prepare savepoint
    update_savepoint("second",2);

    // write data fields
    fs_WriteData_d("dField", 6, data_d);
    fs_WriteData_i("iField", 6, data_i);

    // finalize framework
    fs_Finalize();

    return tests-tests_passed;
}

// test that reading from file works (requires that )
// returns 0 on success
// returns number of failed tests on failure
int test_read(void){
    printf("=================================\n");
    printf("test read\n");
    printf("=================================\n");
    int tests_passed = 0;
    int tests = 0;
    int iDim=8, jDim=8, kDim=2;
    int iHalo=2, jHalo=2, kHalo=0;
    int iStride=iDim+2*iHalo;
    int jStride=jDim+2*jHalo;
    int len=iDim*jDim*kDim;
    int i;

    double *data_d = malloc(sizeof(double)*len);
    int    *data_i = malloc(sizeof(int)*len);

    // initialize framwork
    fs_Initialize(0, 0, "./", 2, "ctest", 5);
    register_fields(iDim,jDim,kDim,iHalo,jHalo,kHalo);

    // Prepare savepoint
    update_savepoint("first",1);

    // read data fields
    fs_ReadData_d("dField", 6, data_d);
    fs_ReadData_i("iField", 6, data_i);

    // validate data fields
    int found_d=0, found_i=0;
    for(i=0; i<len; i++){
        if( data_d[i]!=(double)i )
            found_d++;
        if( data_i[i] != -i )
            found_i++;
    }
    tests_passed += TEST(found_d==0,"Read d field step 1"); tests++;
    tests_passed += TEST(found_i==0,"Read i field step 1"); tests++;

    // Prepare savepoint
    update_savepoint("second",2);

    // write data fields
    fs_ReadData_d("dField", 6, data_d);
    fs_ReadData_i("iField", 6, data_i);

    found_d=0, found_i=0;
    for(i=0; i<len; i++){
        if( data_d[i]!=(double)(2*i) )
            found_d++;
        if( data_i[i] != -(2*i) )
            found_i++;
    }
    tests_passed += TEST(found_d==0,"Read d field step 2"); tests++;
    tests_passed += TEST(found_i==0,"Read i field step 2"); tests++;

    // finalize framework
    fs_Finalize();

    return tests-tests_passed;
}

int main(void){
    int result=0;
    int rval=0;
    if( result=test_init() ){
        printf("[SUMMARY]  : FAILED %d tests in init test\n", result);
    }
    rval += result;
    if( result=test_write() ){
        printf("[SUMMARY]  : FAILED %d tests in write test\n", result);
    }
    rval += result;
    if( result=test_read() ){
        printf("[SUMMARY]  : FAILED %d tests in read test\n", result);
    }
    rval += result;

    return rval;
}

