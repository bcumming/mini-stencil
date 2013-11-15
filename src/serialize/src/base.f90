!+ Module for serialization on hard-disk
!------------------------------------------------------------------------------

MODULE m_serializer

#ifdef _SERIALIZE

!------------------------------------------------------------------------------
!
! Description:
!   This module contains subroutines which allow the outputting of data on
!   external files. The binary data are written to DAT files, while the
!   metadata (Name, dimensions, size of halo, ...) are stored in JSON files.
!
!   These routines are implemented in C++. this is a wrapper module.
!
! Current Code Owner: MeteoSwiss, Andrea Arteaga
!  email:  andyspiros@gmail.com
!==============================================================================


! Modules used:

    use iso_c_binding


!==============================================================================

IMPLICIT NONE

!==============================================================================

!------------------------------------------------------------------------------
! Variables
!------------------------------------------------------------------------------

    integer(c_int), dimension(:), allocatable :: fs_buffer_i
    real(c_float), dimension(:), allocatable  :: fs_buffer_f
    real(c_double), dimension(:), allocatable :: fs_buffer_d

    integer :: fs_buffersize_i, fs_buffersize_f, fs_buffersize_d

!==============================================================================

!------------------------------------------------------------------------------
! Interfaces
!------------------------------------------------------------------------------


!+ Interface for functions that add metainformation to the save point
!------------------------------------------------------------------------------
    interface fs_AddSavePointInfo
        module procedure fs_AddSavePointInfo_b, &
                         fs_AddSavePointInfo_i, &
                         fs_AddSavePointInfo_f, &
                         fs_AddSavePointInfo_d, &
                         fs_AddSavePointInfo_s
    end interface fs_AddSavePointInfo



!+ Interface for functions that write data for a field into the external files
!------------------------------------------------------------------------------
    interface fs_WriteData
        module procedure &
                         fs_WriteData_b_0d, &
                         fs_WriteData_i_0d, &
                         fs_WriteData_i_1d, &
                         fs_WriteData_i_2d, &
                         fs_WriteData_i_3d, &
                         fs_WriteData_i_4d, &
                         fs_WriteData_f_0d, &
                         fs_WriteData_f_1d, &
                         fs_WriteData_f_2d, &
                         fs_WriteData_f_3d, &
                         fs_WriteData_f_4d, &
                         fs_WriteData_d_0d, &
                         fs_WriteData_d_1d, &
                         fs_WriteData_d_2d, &
                         fs_WriteData_d_3d, &
                         fs_WriteData_d_4d
    end interface fs_WriteData



!+ Interface for functions that load data for a field from the external files
!------------------------------------------------------------------------------
    interface fs_ReadData
        module procedure &
                         fs_ReadData_i_0d, &
                         fs_ReadData_i_1d, &
                         fs_ReadData_i_2d, &
                         fs_ReadData_i_3d, &
                         fs_ReadData_i_4d, &
                         fs_ReadData_f_0d, &
                         fs_ReadData_f_1d, &
                         fs_ReadData_f_2d, &
                         fs_ReadData_f_3d, &
                         fs_ReadData_f_4d, &
                         fs_ReadData_d_0d, &
                         fs_ReadData_d_1d, &
                         fs_ReadData_d_2d, &
                         fs_ReadData_d_3d, &
                         fs_ReadData_d_4d
    end interface fs_ReadData



!+ Interfaces for C++ / Fortran cooperation
!------------------------------------------------------------------------------
    interface



        !======================================================================
        ! INITIALIZATION / FINALIZATION
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------


        !+ Function for initializing the serialization framework
        !----------------------------------------------------------------------
        subroutine fs_Initialize_wrapper(singlefile, readwrite, &
                                         basepath, basepathlength, &
                                         basename, basenamelength) &
        bind(c, name='fs_Initialize')
            use, intrinsic :: iso_c_binding
            integer(c_int), value, intent(in) :: singlefile
            integer(c_int), value, intent(in) :: readwrite
            character(c_char), intent(in)     :: basepath
            integer(c_int), value, intent(in) :: basepathlength
            character(c_char), intent(in)     :: basename
            integer(c_int), value, intent(in) :: basenamelength
        end subroutine fs_Initialize_wrapper


        !+ Function for finalizing the serialization framework
        !----------------------------------------------------------------------
        subroutine fs_Finalize_wrapper() &
        bind(c, name='fs_Finalize')
            use, intrinsic :: iso_c_binding
        end subroutine fs_Finalize_wrapper



        !======================================================================
        ! SAVEPOINT MANAGEMENT
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------


        !+ Function for setting a new savepoint with a name
        !----------------------------------------------------------------------
        subroutine fs_SetSavePointName_wrapper(name, namelength) &
        bind(c, name='fs_SetSavePointName')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)     :: name
            integer(c_int), value, intent(in) :: namelength
        end subroutine fs_SetSavePointName_wrapper


        !+ Function for printing the current savepoint
        !----------------------------------------------------------------------
        subroutine fs_PrintSavePoint() &
        bind(c, name='fs_PrintSavePoint')
            use, intrinsic :: iso_c_binding

        end subroutine fs_PrintSavePoint


        !+ Internal usage (binding with C++ function for integer metainformation)
        !----------------------------------------------------------------------
        subroutine fs_AddSavePointInfo_i_wrapper(key, keylength, value) &
        bind(c, name='fs_AddSavePointMetaInfo_i')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)     :: key
            integer(c_int), value, intent(in) :: keylength
            integer(c_int), value, intent(in) :: value
        end subroutine fs_AddSavePointInfo_i_wrapper


        !+ Internal usage (binding with C++ function for single precision real metainformation)
        !----------------------------------------------------------------------
        subroutine fs_AddSavePointInfo_f_wrapper(key, keylength, value) &
        bind(c, name='fs_AddSavePointMetaInfo_f')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)     :: key
            integer(c_int), value, intent(in) :: keylength
            real(c_float),  value, intent(in) :: value
        end subroutine fs_AddSavePointInfo_f_wrapper


        !+ Internal usage (binding with C++ function for double precision real metainformation)
        !----------------------------------------------------------------------
        subroutine fs_AddSavePointInfo_d_wrapper(key, keylength, value) &
        bind(c, name='fs_AddSavePointMetaInfo_d')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)     :: key
            integer(c_int), value, intent(in) :: keylength
            real(c_double), value, intent(in) :: value
        end subroutine fs_AddSavePointInfo_d_wrapper


        !+ Internal usage (binding with C++ function for string metainformation)
        !----------------------------------------------------------------------
        subroutine fs_AddSavePointInfo_s_wrapper(key, keylen, val, vallen) &
        bind(c, name='fs_AddSavePointMetaInfo_s')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)     :: key
            integer(c_int), value, intent(in) :: keylen
            character(c_char), intent(in)     :: val
            integer(c_int), value, intent(in) :: vallen
        end subroutine fs_AddSavePointInfo_s_wrapper



        !======================================================================
        ! FIELD REGISTERING
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------


        !+ Internal usage (binding with C++ function for integer field registering)
        !----------------------------------------------------------------------
        subroutine fs_RegisterField_i_wrapper(fieldname, namelength, &
                   isize, jsize, ksize, lsize, &
                   iminushalo, iplushalo, jminushalo, jplushalo, &
                   kminushalo, kplushalo, lminushalo, lplushalo) &
        bind (c, name='fs_RegisterField_i')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)        :: fieldname
            integer(c_int), value, intent(in)    :: namelength
            integer(c_int), value, intent(in)    :: isize
            integer(c_int), value, intent(in)    :: jsize
            integer(c_int), value, intent(in)    :: ksize
            integer(c_int), value, intent(in)    :: lsize
            integer(c_int), value, intent(in)    :: iminushalo
            integer(c_int), value, intent(in)    :: iplushalo
            integer(c_int), value, intent(in)    :: jminushalo
            integer(c_int), value, intent(in)    :: jplushalo
            integer(c_int), value, intent(in)    :: kminushalo
            integer(c_int), value, intent(in)    :: kplushalo
            integer(c_int), value, intent(in)    :: lminushalo
            integer(c_int), value, intent(in)    :: lplushalo
        end subroutine fs_RegisterField_i_wrapper


        !+ Internal usage (binding with C++ function for single precision real field registering)
        !----------------------------------------------------------------------
        subroutine fs_RegisterField_f_wrapper(fieldname, namelength, &
                   isize, jsize, ksize, lsize, &
                   iminushalo, iplushalo, jminushalo, jplushalo, &
                   kminushalo, kplushalo, lminushalo, lplushalo) &
        bind (c, name='fs_RegisterField_f')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)        :: fieldname
            integer(c_int), value, intent(in)    :: namelength
            integer(c_int), value, intent(in)    :: isize
            integer(c_int), value, intent(in)    :: jsize
            integer(c_int), value, intent(in)    :: ksize
            integer(c_int), value, intent(in)    :: lsize
            integer(c_int), value, intent(in)    :: iminushalo
            integer(c_int), value, intent(in)    :: iplushalo
            integer(c_int), value, intent(in)    :: jminushalo
            integer(c_int), value, intent(in)    :: jplushalo
            integer(c_int), value, intent(in)    :: kminushalo
            integer(c_int), value, intent(in)    :: kplushalo
            integer(c_int), value, intent(in)    :: lminushalo
            integer(c_int), value, intent(in)    :: lplushalo
        end subroutine fs_RegisterField_f_wrapper


        !+ Internal usage (binding with C++ function for double precision real field registering)
        !----------------------------------------------------------------------
        subroutine fs_RegisterField_d_wrapper(fieldname, namelength, &
                   isize, jsize, ksize, lsize, &
                   iminushalo, iplushalo, jminushalo, jplushalo, &
                   kminushalo, kplushalo, lminushalo, lplushalo) &
        bind (c, name='fs_RegisterField_d')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)        :: fieldname
            integer(c_int), value, intent(in)    :: namelength
            integer(c_int), value, intent(in)    :: isize
            integer(c_int), value, intent(in)    :: jsize
            integer(c_int), value, intent(in)    :: ksize
            integer(c_int), value, intent(in)    :: lsize
            integer(c_int), value, intent(in)    :: iminushalo
            integer(c_int), value, intent(in)    :: iplushalo
            integer(c_int), value, intent(in)    :: jminushalo
            integer(c_int), value, intent(in)    :: jplushalo
            integer(c_int), value, intent(in)    :: kminushalo
            integer(c_int), value, intent(in)    :: kplushalo
            integer(c_int), value, intent(in)    :: lminushalo
            integer(c_int), value, intent(in)    :: lplushalo
        end subroutine fs_RegisterField_d_wrapper




        !======================================================================
        ! FIELD STORAGE
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------


        !+ Internal usage (binding with C++ function for integer field storage)
        !----------------------------------------------------------------------
        subroutine fs_WriteData_i_wrapper(fieldname, namelength, data) &
        bind (c, name='fs_WriteData_i')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)      :: fieldname
            integer(c_int), value, intent(in)  :: namelength
            integer(c_int), intent(in)         :: data
        end subroutine fs_WriteData_i_wrapper


        !+ Internal usage (binding with C++ function for single precision real field storage)
        !----------------------------------------------------------------------
        subroutine fs_WriteData_f_wrapper(fieldname, namelength, data) &
        bind (c, name='fs_WriteData_f')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)        :: fieldname
            integer(c_int), value, intent(in)    :: namelength
            real(c_float), intent(in)            :: data
        end subroutine fs_WriteData_f_wrapper


        !+ Internal usage (binding with C++ function for double precision real field storage)
        !----------------------------------------------------------------------
        subroutine fs_WriteData_d_wrapper(fieldname, namelength, data) &
        bind (c, name='fs_WriteData_d')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)         :: fieldname
            integer(c_int), value, intent(in)     :: namelength
            real(c_double), intent(in)            :: data
        end subroutine fs_WriteData_d_wrapper




        !======================================================================
        ! FIELD LOADING
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------


        !+ Internal usage (binding with C++ function for integer field storage)
        !----------------------------------------------------------------------
        subroutine fs_ReadData_i_wrapper(fieldname, namelength, data) &
        bind (c, name='fs_ReadData_i')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)      :: fieldname
            integer(c_int), value, intent(in)  :: namelength
            integer(c_int), intent(out)         :: data
        end subroutine fs_ReadData_i_wrapper


        !+ Internal usage (binding with C++ function for single precision real field storage)
        !----------------------------------------------------------------------
        subroutine fs_ReadData_f_wrapper(fieldname, namelength, data) &
        bind (c, name='fs_ReadData_f')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)        :: fieldname
            integer(c_int), value, intent(in)    :: namelength
            real(c_float), intent(out)            :: data
        end subroutine fs_ReadData_f_wrapper


        !+ Internal usage (binding with C++ function for double precision real field storage)
        !----------------------------------------------------------------------
        subroutine fs_ReadData_d_wrapper(fieldname, namelength, data) &
        bind (c, name='fs_ReadData_d')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)         :: fieldname
            integer(c_int), value, intent(in)     :: namelength
            real(c_double), intent(out)            :: data
        end subroutine fs_ReadData_d_wrapper



        !======================================================================
        ! DEBUG ROUTINES
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------

        subroutine fs_PrintSerializerInfo() bind(c, name='fs_PrintSerializerInfo')
            use, intrinsic :: iso_c_binding
        end subroutine fs_PrintSerializerInfo




        !======================================================================
        ! BINDINGS FOR INTERNAL UTILITIES
        !----------------------------------------------------------------------
        !----------------------------------------------------------------------


        !+ Internal usage (binding with C++ function for retrieving field size)
        !----------------------------------------------------------------------
        subroutine fs_GetFieldSize(fieldname, namelength, &
                   isize, jsize, ksize, lsize) &
        bind (c, name='fs_GetFieldSize')
            use, intrinsic :: iso_c_binding

            character(c_char), intent(in)        :: fieldname
            integer(c_int), value, intent(in)    :: namelength
            integer(c_int), intent(out)           :: isize
            integer(c_int), intent(out)           :: jsize
            integer(c_int), intent(out)           :: ksize
            integer(c_int), intent(out)           :: lsize
        end subroutine fs_GetFieldSize

    end interface



!------------------------------------------------------------------------------
! Subroutines
!------------------------------------------------------------------------------

contains



    !===========================================================================
    !===========================================================================
    ! Initialization / Finalization
    !---------------------------------------------------------------------------


    !+ Initialization of serialization framework
    !  This function must be called before any other function in this module is called
    !----------------------------------------------------------------------
    subroutine fs_Initialize(singlefile, readwrite, basepath, basename)
        implicit none
        logical, intent(in)                    :: singlefile, readwrite
        character(len=*), optional, intent(in) :: basepath, basename
        integer                                :: singlefile_i, readwrite_i
        character(len=256)                     :: path, name
        integer                                :: pathlength, namelength
        
        
        if (present(basepath)) then
            path = basepath
        else
            path = ' '
        endif
        if (present(basename)) then
            name = basename
        else
            name = ' '
        endif
        

        ! Allocate initial buffers
        fs_buffersize_i = 1
        allocate ( fs_buffer_i(fs_buffersize_i) )
        fs_buffersize_f = 1
        allocate ( fs_buffer_f(fs_buffersize_f) )
        fs_buffersize_d = 1
        allocate ( fs_buffer_d(fs_buffersize_d) )

        ! Convert logical values to integers
        if (singlefile) then
            singlefile_i = 1
        else
            singlefile_i = 0
        end if

        if (readwrite) then
            readwrite_i = 1
        else
            readwrite_i = 0
        end if

        ! Call the C function
        call fs_Initialize_wrapper(singlefile_i, readwrite_i, basepath, &
                                   len_trim(basepath), basename, len_trim(basename))

    end subroutine fs_Initialize



    !+ Finalization of serialization framework
    !  This function must be called at the end of the execution
    !----------------------------------------------------------------------
    subroutine fs_Finalize()
        implicit none

        ! Release the buffers
        deallocate ( fs_buffer_i )
        deallocate ( fs_buffer_f )
        deallocate ( fs_buffer_d )

        ! Call the C function
        call fs_Finalize_wrapper()

    end subroutine fs_Finalize



    !======================================================================
    ! SAVEPOINT MANAGEMENT
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------


    !+ Function that clears the savepoint and sets its name
    !----------------------------------------------------------------------
    subroutine fs_SetSavePointName(name)
        implicit none

        character, intent(in)         :: name*(*)

        call fs_SetSavePointName_wrapper(name, len(name))
    end subroutine fs_SetSavePointName



    !+ Function that clears the savepoint and sets its name and the three usual key-value pairs
    !  The name is a required argument, while the LargeTimeStep, RKStageNumber and SmallTimeStep
    !  are optional; in case one of them is not present, the corresponding information is not
    !  saved into the file. There is no default value.
    !----------------------------------------------------------------------
    subroutine fs_SetupSavePoint(name, LargeTimeStep, RKStageNumber, SmallTimeStep)
        implicit none

        character, intent(in)         :: name*(*)
        integer, optional, intent(in) :: LargeTimeStep
        integer, optional, intent(in) :: RKStageNumber
        integer, optional, intent(in) :: SmallTimeStep

        call fs_SetSavePointName(name)

        if (present(LargeTimeStep)) then
            call fs_AddSavePointInfo('LargeTimeStep', LargeTimeStep)
        end if

        if (present(RKStageNumber)) then
            call fs_AddSavePointInfo('RKStageNumber', RKStageNumber)
        end if

        if (present(LargeTimeStep)) then
            call fs_AddSavePointInfo('SmallTimeStep', SmallTimeStep)
        end if

    end subroutine fs_SetupSavePoint


    !----------------------------------------------------------------------
    ! The following functions have the same interface and can be called by using the generic
    ! overloaded interface fs_AddSavePointInfo
    !
    ! The first argument must be a string; the second argument can be an integer,
    ! a single precision or double precision real or a string
    !----------------------------------------------------------------------


    !+ Integer value
    !----------------------------------------------------------------------
    subroutine fs_AddSavePointInfo_i(key, value)
        use, intrinsic :: iso_c_binding
        implicit none

        character, intent(in)         :: key*(*)
        integer(c_int), intent(in)    :: value

        call fs_AddSavePointInfo_i_wrapper(key, len(key), value)

    end subroutine fs_AddSavePointInfo_i

    !+ Logical value
    !----------------------------------------------------------------------
    subroutine fs_AddSavePointInfo_b(key, value)
        use, intrinsic :: iso_c_binding
        implicit none

        character, intent(in)         :: key*(*)
        logical, intent(in)    :: value
        integer(c_int)         :: value_

        if(value .eqv. .true.) then
          value_ = 1
        else
          value_ = 0
        endif
        call fs_AddSavePointInfo_i_wrapper(key, len(key), value_)

    end subroutine fs_AddSavePointInfo_b


    !+ Single precision real value
    !----------------------------------------------------------------------
    subroutine fs_AddSavePointInfo_f(key, value)
        use, intrinsic :: iso_c_binding
        implicit none

        character, intent(in)     :: key*(*)
        real(c_float), intent(in) :: value

        call fs_AddSavePointInfo_f_wrapper(key, len(key), value)

    end subroutine fs_AddSavePointInfo_f


    !+ Double precision real value
    !----------------------------------------------------------------------
    subroutine fs_AddSavePointInfo_d(key, value)
        use, intrinsic :: iso_c_binding
        implicit none

        character, intent(in)         :: key*(*)
        real(c_double), intent(in)    :: value

        call fs_AddSavePointInfo_d_wrapper(key, len(key), value)

    end subroutine fs_AddSavePointInfo_d


    !+ String value
    !----------------------------------------------------------------------
    subroutine fs_AddSavePointInfo_s(key, value)
        use, intrinsic :: iso_c_binding
        implicit none

        character, intent(in)  :: key*(*)
        character , intent(in) :: value*(*)

        call fs_AddSavePointInfo_s_wrapper(key, len(key), value, len(value))

    end subroutine fs_AddSavePointInfo_s





    !======================================================================
    ! FIELD REGISTERING
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------


    !+ Generic function for registering a field
    !  The field name is the only required argument.
    !  Data type the any of "integer", "float" and "double"; if not given, "double" is assumed
    !  Assumed size in case of lacking argument is 1
    !  Assumed halo size in case of lacking argument is 0
    !----------------------------------------------------------------------
    subroutine fs_RegisterField (FieldName, DataType, ISize, JSize, KSize, LSize, &
                                 IMinusHalo, IPlusHalo, JMinusHalo, JPlusHalo,&
                                 KMinusHalo, KPlusHalo, LMinusHalo, LPlusHalo &
                                )
        use, intrinsic :: iso_c_binding

        implicit none

        character, intent(in)                  :: FieldName*(*)
        character(len=*), optional, intent(in) :: DataType
        integer(c_int), optional, intent(in)   :: ISize
        integer(c_int), optional, intent(in)   :: JSize
        integer(c_int), optional, intent(in)   :: KSize
        integer(c_int), optional, intent(in)   :: LSize
        integer(c_int), optional, intent(in)   :: IMinusHalo
        integer(c_int), optional, intent(in)   :: IPlusHalo
        integer(c_int), optional, intent(in)   :: JMinusHalo
        integer(c_int), optional, intent(in)   :: JPlusHalo
        integer(c_int), optional, intent(in)   :: KMinusHalo
        integer(c_int), optional, intent(in)   :: KPlusHalo
        integer(c_int), optional, intent(in)   :: LMinusHalo
        integer(c_int), optional, intent(in)   :: LPlusHalo

        character(len=7)           :: DT

        integer(c_int)             :: ISize_, JSize_, KSize_, LSize_, &
                                    IMH, IPH, JMH, JPH, KMH, KPH, LMH, LPH

        ! Intepret sizes and halo sizes
        IF ( present(ISize) ) THEN
            ISize_ = ISize
        ELSE
            ISize_ = 1
        END IF
        IF ( present(JSize) ) THEN
            JSize_ = JSize
        ELSE
            JSize_ = 1
        END IF
        IF ( present(KSize) ) THEN
            KSize_ = KSize
        ELSE
            KSize_ = 1
        END IF
        IF ( present(LSize) ) THEN
            LSize_ = LSize
        ELSE
            LSize_ = 1
        END IF

        IF ( present(IMinusHalo) ) THEN
            IMH = IMinusHalo
        ELSE
            IMH = 0
        END IF
        IF ( present(JMinusHalo) ) THEN
            JMH = JMinusHalo
        ELSE
            JMH = 0
        END IF
        IF ( present(KMinusHalo) ) THEN
            KMH = KMinusHalo
        ELSE
            KMH = 0
        END IF
        IF ( present(LMinusHalo) ) THEN
            LMH = LMinusHalo
        ELSE
            LMH = 0
        END IF

        IF ( present(IPlusHalo) ) THEN
            IPH = IPlusHalo
        ELSE
            IPH = 0
        END IF
        IF ( present(JPlusHalo) ) THEN
            JPH = JPlusHalo
        ELSE
            JPH = 0
        END IF
        IF ( present(KPlusHalo) ) THEN
            KPH = KPlusHalo
        ELSE
            KPH = 0
        END IF
        IF ( present(LPlusHalo) ) THEN
            LPH = LPlusHalo
        ELSE
            LPH = 0
        END IF


        ! Set default data type to double
        IF ( present(DataType) ) THEN
            DT = DataType
        ELSE
            DT = "double"
        END IF

        ! Call the correct subroutine, depending on the data type
        IF ( DT .EQ. "integer") THEN
            call fs_RegisterField_i_wrapper(FieldName, len(FieldName), &
                                        ISize_, JSize_, KSize_, LSize_, &
                                        IMH, IPH, JMH, JPH, KMH, KPH, LMH, LPH)
        ELSEIF ( DT .EQ. "float") THEN
            call fs_RegisterField_f_wrapper(FieldName, len(FieldName), &
                                        ISize_, JSize_, KSize_, LSize_, &
                                        IMH, IPH, JMH, JPH, KMH, KPH, LMH, LPH)
        ELSEIF ( DT .EQ. "double") THEN
            call fs_RegisterField_d_wrapper(FieldName, len(FieldName), &
                                        ISize_, JSize_, KSize_, LSize_, &
                                        IMH, IPH, JMH, JPH, KMH, KPH, LMH, LPH)
        ELSE
            write (*,*) "Error: data type ", DataType, " not recognized"
        END IF

    end subroutine fs_RegisterField




    !----------------------------------------------------------------------
    ! The following functions are provided for convenience and allow the registrations
    ! of the most common types of fields. In all of the it is assumed for rank-1 fields
    ! that the index i is used, for rank-2 fields that the indices i and j are used
    ! and for rank-3 fields that the indices i, j and k are used; it is therefore
    ! impossible to register e.g. a j-k field or a k-field using these subroutines: instead,
    ! the generic subroutine fs_RegisterField must be called.
    !
    ! In the following subroutines, all parameters are required. The first parameter is
    ! always the name of the field; then, the sizes must be provided, followed by
    ! the size of the halo in each direction. Notice that the halo can be non-symmetric.
    ! Also notice that for each rank three functions are provided: one for integer fields,
    ! one for single-precision real fields and one for double-precision real fields.
    !----------------------------------------------------------------------


    !+Registration of 1D integer fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_i_1d(fieldname, isize, iminushalo, iplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo

        call fs_RegisterField_i_wrapper(fieldname, len(fieldname), &
                                        isize, 1, 1, 1, &
                                        iminushalo, iplushalo, 0, 0, 0, 0, 0, 0)
    end subroutine fs_RegisterField_i_1d


    !+Registration of 1D single-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_f_1d(fieldname, isize, iminushalo, iplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo

        call fs_RegisterField_f_wrapper(fieldname, len(fieldname), &
                                        isize, 1, 1, 1, &
                                        iminushalo, iplushalo, 0, 0, 0, 0, 0, 0)
    end subroutine fs_RegisterField_f_1d


    !+Registration of 1D double-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_d_1d(fieldname, isize, iminushalo, iplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo

        call fs_RegisterField_d_wrapper(fieldname, len(fieldname), &
                                        isize, 1, 1, 1, &
                                        iminushalo, iplushalo, 0, 0, 0, 0, 0, 0)
    end subroutine fs_RegisterField_d_1d



    !+Registration of 2D integer fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_i_2d(fieldname, isize, jsize, &
                                   iminushalo, iplushalo, jminushalo, jplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo

        call fs_RegisterField_i_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, 1, 1, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, 0, 0, 0, 0)
    end subroutine fs_RegisterField_i_2d


    !+Registration of 2D single-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_f_2d(fieldname, isize, jsize, &
                                   iminushalo, iplushalo, jminushalo, jplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo

        call fs_RegisterField_f_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, 1, 1, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, 0, 0, 0, 0)
    end subroutine fs_RegisterField_f_2d


    !+Registration of 2D double-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_d_2d(fieldname, isize, jsize, &
                                   iminushalo, iplushalo, jminushalo, jplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo

        call fs_RegisterField_d_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, 1, 1, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, 0, 0, 0, 0)
    end subroutine fs_RegisterField_d_2d



    !+Registration of 3D integer fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_i_3d(fieldname, isize, jsize, ksize, &
            iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: ksize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo
        integer(c_int) :: kminushalo
        integer(c_int) :: kplushalo

        call fs_RegisterField_i_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, ksize, 1, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, &
                                        kminushalo, kplushalo, 0, 0)
    end subroutine fs_RegisterField_i_3d


    !+Registration of 3D single-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_f_3d(fieldname, isize, jsize, ksize, &
            iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: ksize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo
        integer(c_int) :: kminushalo
        integer(c_int) :: kplushalo

        call fs_RegisterField_f_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, ksize, 1, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, &
                                        kminushalo, kplushalo, 0, 0)
    end subroutine fs_RegisterField_f_3d


    !+Registration of 3D double-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_d_3d(fieldname, isize, jsize, ksize, &
            iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: ksize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo
        integer(c_int) :: kminushalo
        integer(c_int) :: kplushalo

        call fs_RegisterField_d_wrapper(fieldname, len(fieldname), &
                                            isize, jsize, ksize, 1, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, &
                                        kminushalo, kplushalo, 0, 0)
    end subroutine fs_RegisterField_d_3d



    !+Registration of 4D integer fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_i_4d(fieldname, isize, jsize, ksize, lsize, &
                                 iminushalo, iplushalo, jminushalo, jplushalo, &
                                 kminushalo, kplushalo, lminushalo, lplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: ksize
        integer(c_int) :: lsize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo
        integer(c_int) :: kminushalo
        integer(c_int) :: kplushalo
        integer(c_int) :: lminushalo
        integer(c_int) :: lplushalo

        call fs_RegisterField_i_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, ksize, lsize, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, &
                                        kminushalo, kplushalo, &
                                        lminushalo, lplushalo)
    end subroutine fs_RegisterField_i_4d


    !+Registration of 4D single-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_f_4d(fieldname, isize, jsize, ksize, lsize, &
                                 iminushalo, iplushalo, jminushalo, jplushalo, &
                                 kminushalo, kplushalo, lminushalo, lplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: ksize
        integer(c_int) :: lsize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo
        integer(c_int) :: kminushalo
        integer(c_int) :: kplushalo
        integer(c_int) :: lminushalo
        integer(c_int) :: lplushalo

        call fs_RegisterField_f_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, ksize, lsize, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, &
                                        kminushalo, kplushalo, &
                                        lminushalo, lplushalo)
    end subroutine fs_RegisterField_f_4d


    !+Registration of 4D double-precision fields
    !----------------------------------------------------------------------
    subroutine fs_RegisterField_d_4d(fieldname, isize, jsize, ksize, lsize, &
                                 iminushalo, iplushalo, jminushalo, jplushalo, &
                                 kminushalo, kplushalo, lminushalo, lplushalo)
        use, intrinsic :: iso_c_binding

        implicit none
        character      :: fieldname*(*)
        integer(c_int) :: isize
        integer(c_int) :: jsize
        integer(c_int) :: ksize
        integer(c_int) :: lsize
        integer(c_int) :: iminushalo
        integer(c_int) :: iplushalo
        integer(c_int) :: jminushalo
        integer(c_int) :: jplushalo
        integer(c_int) :: kminushalo
        integer(c_int) :: kplushalo
        integer(c_int) :: lminushalo
        integer(c_int) :: lplushalo

        call fs_RegisterField_d_wrapper(fieldname, len(fieldname), &
                                        isize, jsize, ksize, lsize, &
                                        iminushalo, iplushalo, &
                                        jminushalo, jplushalo, &
                                        kminushalo, kplushalo, &
                                        lminushalo, lplushalo)
    end subroutine fs_RegisterField_d_4d






    !======================================================================
    ! FIELD STORING
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------


    !----------------------------------------------------------------------
    ! The following functions share the same interface and represent the implementation
    ! of the algorithm that writes the raw binary data to the disk by passing them
    ! to the C++ serialization framework. Before passing them, the data is copied into
    ! the temporary buffer.
    !
    ! Implementation details
    ! In order to be able to pass data from a selection (e.g. u(:, 3, :, nnow) ),
    ! the data is copied in a fortran buffer that then is passed to the corresponding
    ! C function. A single buffer for each data type exist in this module and is
    ! reallocated with a larger size if it is too small to contain all the elements
    ! in the passed field.
    !----------------------------------------------------------------------



    !+Serialization of integer values
    !----------------------------------------------------------------------
    subroutine fs_WriteData_b(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        logical,dimension(*), intent(in) :: data
        character, intent(in)                    :: fieldname*(*)
        integer                                  :: isize, jsize, ksize, lsize, totsize
        integer                                  :: iter
        integer(c_int)                           :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_i) then
            deallocate ( fs_buffer_i )
            fs_buffersize_i = totsize
            allocate ( fs_buffer_i(fs_buffersize_i) )
        end if

        ! Copy data into buffer
        do iter = 1,totsize
            if(data(iter) .eqv. .true.) then
              fs_buffer_i(iter) = 1
            else
              fs_buffer_i(iter) = 0
            endif
        end do

        ! Call C function
        call fs_WriteData_i_wrapper(fieldname, nsize, fs_buffer_i(1))

    end subroutine fs_WriteData_b


    !+Serialization of integer values
    !----------------------------------------------------------------------
    subroutine fs_WriteData_i(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(*), intent(in) :: data
        character, intent(in)                    :: fieldname*(*)
        integer                                  :: isize, jsize, ksize, lsize, totsize
        integer                                  :: iter
        integer(c_int)                           :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_i) then
            deallocate ( fs_buffer_i )
            fs_buffersize_i = totsize
            allocate ( fs_buffer_i(fs_buffersize_i) )
        end if

        ! Copy data into buffer
        do iter = 1,totsize
            fs_buffer_i(iter) = data(iter)
        end do

        ! Call C function
        call fs_WriteData_i_wrapper(fieldname, nsize, fs_buffer_i(1))

    end subroutine fs_WriteData_i



    !+Serialization of single-precision real values
    !----------------------------------------------------------------------
    subroutine fs_WriteData_f(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(*), intent(in) :: data
        character, intent(in)                   :: fieldname*(*)
        integer                                 :: isize, jsize, ksize, lsize, totsize
        integer                                 :: iter
        integer(c_int)                          :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_f) then
            deallocate ( fs_buffer_f )
            fs_buffersize_f = totsize
            allocate ( fs_buffer_f(fs_buffersize_f) )
        end if

        ! Copy data into buffer
        do iter = 1,totsize
            fs_buffer_f(iter) = data(iter)
        end do

        ! Call C function
        call fs_WriteData_f_wrapper(fieldname, nsize, fs_buffer_f(1))

    end subroutine fs_WriteData_f



    !+Serialization of double-precision real values
    !----------------------------------------------------------------------
    subroutine fs_WriteData_d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(*), intent(in) :: data
        character, intent(in)                    :: fieldname*(*)
        integer                                  :: isize, jsize, ksize, lsize, totsize
        integer                                  :: iter
        integer(c_int)                           :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_d) then
            deallocate ( fs_buffer_d )
            fs_buffersize_d = totsize
            allocate ( fs_buffer_d(fs_buffersize_d) )
        end if

        ! Copy data into buffer
        do iter = 1,totsize
            fs_buffer_d(iter) = data(iter)
        end do

        ! Call C function
        call fs_WriteData_d_wrapper(fieldname, nsize, fs_buffer_d(1))

    end subroutine fs_WriteData_d




    !+Deserialization of integer values
    !----------------------------------------------------------------------
    subroutine fs_ReadData_i(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(*), intent(out) :: data
        character, intent(in)                     :: fieldname*(*)
        integer                                   :: isize, jsize, ksize, lsize, totsize
        integer                                   :: iter
        integer(c_int)                            :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_i) then
            deallocate ( fs_buffer_i )
            fs_buffersize_i = totsize
            allocate ( fs_buffer_i(fs_buffersize_i) )
        end if

        ! Call C function
        call fs_ReadData_i_wrapper(fieldname, nsize, fs_buffer_i(1))

        ! Copy data from  buffer
        do iter = 1,totsize
            data(iter) = fs_buffer_i(iter)
        end do

    end subroutine fs_ReadData_i



    !+Deserialization of single-precision real values
    !----------------------------------------------------------------------
    subroutine fs_ReadData_f(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(*), intent(out) :: data
        character, intent(in)                    :: fieldname*(*)
        integer                                  :: isize, jsize, ksize, lsize, totsize
        integer                                  :: iter
        integer(c_int)                           :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_f) then
            deallocate ( fs_buffer_f )
            fs_buffersize_f = totsize
            allocate ( fs_buffer_f(fs_buffersize_f) )
        end if

        ! Call C function
        call fs_ReadData_f_wrapper(fieldname, nsize, fs_buffer_f(1))

        ! Copy data from  buffer
        do iter = 1,totsize
            data(iter) = fs_buffer_f(iter)
        end do

    end subroutine fs_ReadData_f



    !+Deserialization of double-precision real values
    !----------------------------------------------------------------------
    subroutine fs_ReadData_d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(*), intent(out) :: data
        character, intent(in)                     :: fieldname*(*)
        integer                                   :: isize, jsize, ksize, lsize, totsize
        integer                                   :: iter
        integer(c_int)                            :: nsize

        ! Name length
        nsize = len(fieldname)

        ! Get dimensions
        call fs_GetFieldSize(fieldname, nsize, isize, jsize, ksize, lsize)
        totsize = isize * jsize * ksize * lsize

        ! Ensure buffer has enough space
        if (totsize .GT. fs_buffersize_d) then
            deallocate ( fs_buffer_d )
            fs_buffersize_d = totsize
            allocate ( fs_buffer_d(fs_buffersize_d) )
        end if

        ! Call C function
        call fs_ReadData_d_wrapper(fieldname, nsize, fs_buffer_d(1))

        ! Copy data from  buffer
        do iter = 1,totsize
            data(iter) = fs_buffer_d(iter)
        end do

    end subroutine fs_ReadData_d



    !======================================================================
    ! FIELD STORING WRAPPERS
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------


    !----------------------------------------------------------------------
    ! The following functions are thin wrappers for the rank-agnostic functions
    ! provided just above. They get acually called when the overloaded interface
    ! fs_WriteData is called.
    !----------------------------------------------------------------------



    !+Serialization of boolean 1D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_b_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        logical, intent(in) :: data
        character, intent(in)                     :: fieldname*(*)
        logical, dimension(1)             :: data_

        data_(1) = data
        ! Call rank-agnostic function
        call fs_WriteData_b(fieldname, data_)

    end subroutine fs_WriteData_b_0d


    !+Serialization of integer scalars
    !----------------------------------------------------------------------
    subroutine fs_WriteData_i_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), intent(in)               :: data
        integer(c_int), dimension(1)             :: data_
        character, intent(in)                    :: fieldname*(*)

        ! Call rank-agnostic function
        data_(1) = data
        call fs_WriteData_i(fieldname, data_)

    end subroutine fs_WriteData_i_0d


    !+Serialization of integer 1D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_i_1d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:), intent(in) :: data
        character, intent(in)                     :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_i(fieldname, data)

    end subroutine fs_WriteData_i_1d


    !+Serialization of integer 2D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_i_2d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:,1:), intent(in) :: data
        character, intent(in)                        :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_i(fieldname, data)

    end subroutine fs_WriteData_i_2d


    !+Serialization of integer 3D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_i_3d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:,1:,1:), intent(in) :: data
        character, intent(in)                           :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_i(fieldname, data)

    end subroutine fs_WriteData_i_3d


    !+Serialization of integer 4D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_i_4d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:,1:,1:,1:), intent(in) :: data
        character, intent(in)                              :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_i(fieldname, data)

    end subroutine fs_WriteData_i_4d




    !+Serialization of single-precision real scalars
    !----------------------------------------------------------------------
    subroutine fs_WriteData_f_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), intent(in)               :: data
        real(c_float), dimension(1)             :: data_
        character, intent(in)                   :: fieldname*(*)

        ! Call rank-agnostic function
        data_(1) = data
        call fs_WriteData_f(fieldname, data_)

    end subroutine fs_WriteData_f_0d


    !+Serialization of single-precision real 1D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_f_1d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:), intent(in) :: data
        character, intent(in)                    :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_f(fieldname, data)

    end subroutine fs_WriteData_f_1d


    !+Serialization of single-precision real 2D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_f_2d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:,1:), intent(in) :: data
        character, intent(in)                       :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_f(fieldname, data)

    end subroutine fs_WriteData_f_2d


    !+Serialization of single-precision real 3D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_f_3d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:,1:,1:), intent(in) :: data
        character, intent(in)                          :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_f(fieldname, data)

    end subroutine fs_WriteData_f_3d


    !+Serialization of single-precision real 4D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_f_4d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:,1:,1:,1:), intent(in) :: data
        character, intent(in)                             :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_f(fieldname, data)

    end subroutine fs_WriteData_f_4d




    !+Serialization of double-precision real scalars
    !----------------------------------------------------------------------
    subroutine fs_WriteData_d_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), intent(in)               :: data
        real(c_double), dimension(1)             :: data_
        character, intent(in)                    :: fieldname*(*)

        ! Call rank-agnostic function
        data_(1) = data
        call fs_WriteData_d(fieldname, data_)

    end subroutine fs_WriteData_d_0d


    !+Serialization of double-precision real 1D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_d_1d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:), intent(in) :: data
        character, intent(in)                     :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_d(fieldname, data)

    end subroutine fs_WriteData_d_1d


    !+Serialization of double-precision real 2D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_d_2d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:,1:), intent(in) :: data
        character, intent(in)                        :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_d(fieldname, data)

    end subroutine fs_WriteData_d_2d


    !+Serialization of double-precision real 3D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_d_3d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:,1:,1:), intent(in) :: data
        character, intent(in)                           :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_d(fieldname, data)

    end subroutine fs_WriteData_d_3d


    !+Serialization of double-precision real 4D fields
    !----------------------------------------------------------------------
    subroutine fs_WriteData_d_4d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:,1:,1:,1:), intent(in) :: data
        character, intent(in)                              :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_WriteData_d(fieldname, data)

    end subroutine fs_WriteData_d_4d






    !======================================================================
    ! FIELD LOADING WRAPPERS
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------


    !----------------------------------------------------------------------
    ! The following functions are thin wrappers for the rank-agnostic functions
    ! provided above. They get acually called when the overloaded interface
    ! fs_ReadData is called.
    !----------------------------------------------------------------------



    !+Deserialization of integer scalars
    !----------------------------------------------------------------------
    subroutine fs_ReadData_i_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), intent(out)               :: data
        integer(c_int), dimension(1)              :: data_
        character, intent(in)                     :: fieldname*(*)

        ! Call rank-agnostic function
        data_(1) = data
        call fs_ReadData_i(fieldname, data_)

    end subroutine fs_ReadData_i_0d


    !+Deserialization of integer 1D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_i_1d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:), intent(out) :: data
        character, intent(in)                      :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_i(fieldname, data)

    end subroutine fs_ReadData_i_1d


    !+Deserialization of integer 2D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_i_2d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:,1:), intent(out) :: data
        character, intent(in)                         :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_i(fieldname, data)

    end subroutine fs_ReadData_i_2d


    !+Deserialization of integer 3D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_i_3d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:,1:,1:), intent(out) :: data
        character, intent(in)                            :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_i(fieldname, data)

    end subroutine fs_ReadData_i_3d


    !+Deserialization of integer 4D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_i_4d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), dimension(1:,1:,1:,1:), intent(out) :: data
        character, intent(in)                               :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_i(fieldname, data)

    end subroutine fs_ReadData_i_4d



    !+Deserialization of single-precision real scalars
    !----------------------------------------------------------------------
    subroutine fs_ReadData_f_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), intent(out)               :: data
        real(c_float), dimension(1)              :: data_
        character, intent(in)                    :: fieldname*(*)

        ! Call rank-agnostic function
        data_(1) = data
        call fs_ReadData_f(fieldname, data_)

    end subroutine fs_ReadData_f_0d


    !+Deserialization of single-precision real 1D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_f_1d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:), intent(out) :: data
        character, intent(in)                     :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_f(fieldname, data)

    end subroutine fs_ReadData_f_1d


    !+Deserialization of single-precision real 2D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_f_2d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:,1:), intent(out) :: data
        character, intent(in)                        :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_f(fieldname, data)

    end subroutine fs_ReadData_f_2d


    !+Deserialization of single-precision real 3D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_f_3d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:,1:,1:), intent(out) :: data
        character, intent(in)                           :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_f(fieldname, data)

    end subroutine fs_ReadData_f_3d


    !+Deserialization of single-precision real 4D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_f_4d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_float), dimension(1:,1:,1:,1:), intent(out) :: data
        character, intent(in)                              :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_f(fieldname, data)

    end subroutine fs_ReadData_f_4d



    !+Deserialization of double-precision real scalars
    !----------------------------------------------------------------------
    subroutine fs_ReadData_d_0d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), intent(out)               :: data
        real(c_double), dimension(1)              :: data_
        character, intent(in)                     :: fieldname*(*)

        ! Call rank-agnostic function
        data_(1) = data
        call fs_ReadData_d(fieldname, data_)

    end subroutine fs_ReadData_d_0d


    !+Deserialization of double-precision real 1D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_d_1d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:), intent(out) :: data
        character, intent(in)                      :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_d(fieldname, data)

    end subroutine fs_ReadData_d_1d


    !+Deserialization of double-precision real 2D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_d_2d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:,1:), intent(out) :: data
        character, intent(in)                         :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_d(fieldname, data)

    end subroutine fs_ReadData_d_2d


    !+Deserialization of double-precision real 3D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_d_3d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:,1:,1:), intent(out) :: data
        character, intent(in)                            :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_d(fieldname, data)

    end subroutine fs_ReadData_d_3d


    !+Deserialization of double-precision real 4D fields
    !----------------------------------------------------------------------
    subroutine fs_ReadData_d_4d(fieldname, data)
        use, intrinsic :: iso_c_binding
        implicit none

        real(c_double), dimension(1:,1:,1:,1:), intent(out) :: data
        character, intent(in)                               :: fieldname*(*)

        ! Call rank-agnostic function
        call fs_ReadData_d(fieldname, data)

    end subroutine fs_ReadData_d_4d

#endif
END MODULE m_serializer
