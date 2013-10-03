! File: m_serializer.f90
! Description: utility module which can be used to serialize variables into a file
! Author: Oliver Fuhrer, MeteoSwiss, 2011

module m_serializer

#ifdef _SERIALIZE

  implicit none

  private

  !!! CONSTANTS

  ! debug
  logical, parameter         :: IsDebugOn = .false.

  ! on/off-switch
  logical                    :: IsOn = .false.

  ! maximum number of fields to be serialized
  integer, parameter         :: max_NField  = 199

  ! flush output buffers after each write
  logical, parameter         :: DoIOFlush = .true.

  ! file prefix/postfix
  character(*), parameter    :: FileNamePrefix = 'Field_'
  character(*), parameter    :: FileNamePostfix = '.dat'

  ! string lengths
  integer, parameter         :: sl_Error    = 255
  integer, parameter         :: sl_Name     = 50
  integer, parameter         :: sl_FileName = sl_Name + 10
  integer, parameter         :: sl_Type     = 32

  ! null character
  character(len=1), parameter :: Char0 = CHAR(0)
  character(len=1), parameter :: Char10 = CHAR(10)

	!!! TYPE DECLARATIONS

  ! field information
  type t_FieldInfo
    character(len=sl_Name)   :: Name
    character(len=sl_Type)   :: ElementType
    integer                  :: BytesPerElement
    integer                  :: FieldRank
    integer                  :: ISize, JSize, KSize
    integer                  :: IHaloSize, JHaloSize, KPlusHaloSize
    logical                  :: IsInUse
    integer                  :: FileUnitNumber
  end type t_FieldInfo

  ! information about current save point
  type t_SavePointInfo
    character(len=sl_Name)   :: Name
    integer                  :: LargeTimeStep
    integer                  :: RKStageNumber
    integer                  :: SmallTimeStep
    integer                  :: GlobalCounter
  end type t_SavePointInfo

  !!! PRIVATE DECLARATIONS

  ! save point setup?
  logical                    :: IsSavePointOk = .false.

  ! File unit number used for I/O
  ! (increased for each field, initialized with start value)
  integer                    :: FileUnitNumber = 210

  ! the current save point
  type(t_SavePointInfo)      :: SavePointInfo

  ! store field information
  type(t_FieldInfo)          :: FieldInfo( max_NField )

  ! field information counter
  integer                    :: FieldInfoCount = 0

  !!! INTERFACE BLOCKS

  interface fs_WriteData
    module procedure           &
      fs_WriteData_0d_real,    &
      fs_WriteData_1d_real,    &
      fs_WriteData_2d_real,    &
      fs_WriteData_3d_real,    &
      fs_WriteData_0d_dble,    &
      fs_WriteData_1d_dble,    &
      fs_WriteData_2d_dble,    &
      fs_WriteData_3d_dble,    &
      fs_WriteData_0d_int, &
      fs_WriteData_1d_int, &
      fs_WriteData_2d_int, &
      fs_WriteData_3d_int
  end interface

  !!! PUBLIC DECLARATIONS

  public :: &
    fs_Initialize,             &
    fs_WriteFieldInfo,         &
    fs_SetupSavePoint,         &
    fs_WriteData,              &
    fs_Finalize

contains


  ! Module Procedure: fs_Initialize
  ! Description: Initialize module at startup
  subroutine fs_Initialize( )
    implicit none

    ! debug log
    call fs_Debug('fs_Initialize: ')

    ! switch on
    IsOn = .true.

    ! setup SavePoint with default values
    SavePointInfo%Name           = ''
    SavePointInfo%LargeTimeStep  = -1
    SavePointInfo%RKStageNumber  = -1
    SavePointInfo%SmallTimeStep  = -1
    SavePointInfo%GlobalCounter  = -1

    ! setup FieldInfo with default values
    FieldInfo(:)%Name            = ''
    FieldInfo(:)%ElementType     = ''
    FieldInfo(:)%BytesPerElement = -1
    FieldInfo(:)%FieldRank       = -1
    FieldInfo(:)%ISize           = -1
    FieldInfo(:)%JSize           = -1
    FieldInfo(:)%KSize           = -1
    FieldInfo(:)%IHaloSize       = -1
    FieldInfo(:)%JHaloSize       = -1
    FieldInfo(:)%KPlusHaloSize   = -1
    FieldInfo(:)%IsInUse         = .false.
    FieldInfo(:)%FileUnitNumber  = -1

    ! debug log
    call fs_Debug('fs_Initialize: EXIT')

  end subroutine fs_Initialize


  ! Module procedure: fs_WriteFieldInfo
  ! Description: This function opens a file for serializing a field which is
  !   specified by its name and writes a header into the file consisting of
  !   FieldInfo as a JSON string, terminating it with a null terminator.
  subroutine fs_WriteFieldInfo( FieldName, ElementType, BytesPerElement, &
    FieldRank, ISize, JSize, KSize, IHaloSize, JHaloSize, KPlusHaloSize );
    implicit none

    ! arguments (in)
    character(len=*), intent(in)           :: FieldName
    character(len=*), intent(in)           :: ElementType
    integer,          intent(in)           :: BytesPerElement
    integer,          intent(in)           :: FieldRank
    integer,          intent(in), optional :: ISize, JSize, KSize
    integer,          intent(in), optional :: IHaloSize, JHaloSize, KPlusHaloSize

    ! local variables
    integer                      :: i
    logical                      :: zIsUnitOk, zIsUnitOpen
    integer                      :: zStatus
    integer                      :: zISize, zJSize, zKSize
    integer                      :: zIHaloSize, zJHaloSize, zKPlusHaloSize
    character(len=sl_FileName)   :: zFileName
    character(len=1024)          :: zString
    character(len=30)            :: zS1, zS2, zS3, zS4, zS5, zS6, zS7, zS8

    ! check if on
    if (.not. IsOn) return

    ! debug log
    call fs_Debug('fs_WriteFieldInfo: '//trim(FieldName))

    ! check arguments
    if ( len_trim( FieldName ) > sl_Name ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1231)
    end if
    if ( len_trim( ElementType ) > sl_Type ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1233)
    end if
    if ( BytesPerElement < 1 .or. BytesPerElement > 32 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1441)
    end if
    if ( count( (/ 0, 1, 2, 3 /) == FieldRank ) /= 1 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1451)
    end if

    ! check if sizes are specified and ok
    if ( present( ISize ) ) then
      zISize = ISize
      if ( zISize < -1 .or. zISize == 0 ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1261)
      end if
    else
      zISize = -1
    end if
    if ( present( JSize ) ) then
      zJSize = JSize
      if ( zJSize < -1 .or. zJSize == 0 ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1261)
      end if
    else
      zJSize = -1
    end if
    if ( present( KSize ) ) then
      zKSize = KSize
      if ( zKSize < -1 .or. zKSize == 0 ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1261)
      end if
    else
      zKSize = -1
    end if
    if ( count( (/zISize,zJSize,zKSize/) /= -1 ) /= FieldRank ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1251)
    end if

    ! check if halo sizes are specified and ok
    if ( present( IHaloSize ) ) then
      zIHaloSize = IHaloSize
      if ( zIHaloSize < -1 .or. zIHaloSize > 20 ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1262)
      end if
    else
      zIHaloSize = -1
    end if
    if ( present( JHaloSize ) ) then
      zJHaloSize = JHaloSize
      if ( zJHaloSize < -1 .or. zJHaloSize > 20 ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1262)
      end if
    else
      zJHaloSize = -1
    end if
    if ( present( KPlusHaloSize ) ) then
      zKPlusHaloSize = KPlusHaloSize
      if ( zKPlusHaloSize < -1 .or. zKPlusHaloSize > 20 ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1262)
      end if
    else
      zKPlusHaloSize = -1
    end if
    if ( zIHaloSize /= -1 .and. zISize == -1 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1291)
    end if
    if ( zJHaloSize /= -1 .and. zJSize == -1 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1291)
    end if
    if ( zKPlusHaloSize /= -1 .and. zKSize == -1 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1291)
    end if
    if ( zIHaloSize /= -1 .and. 2*zIHaloSize >= zISize ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1255)
    end if
    if ( zJHaloSize /= -1 .and. 2*zJHaloSize >= zJSize ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1255)
    end if
    if ( zKPlusHaloSize /= -1 .and. zKPlusHaloSize >= zKSize ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1255)
    end if

    ! check if field with same name already registered
    do i = 1, FieldInfoCount
      if ( trim(FieldName) == trim(FieldInfo(i)%Name) ) then
        write(0,*) 'field ', FieldName
        call fs_Error(1263)
      end if
    end do

    ! increase field information counter
    FieldInfoCount = FieldInfoCount + 1

    ! get next free File unit number for I/O
    do while ( FileUnitNumber < 999 )
      FileUnitNumber = FileUnitNumber + 1
      inquire( unit=FileUnitNumber, exist=zIsUnitOk, opened=zIsUnitOpen )
      if ( zIsUnitOk .and. .not. zIsUnitOpen ) exit
    end do
    if ( FileUnitNumber > 999 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1101)
    end if

    ! set field information
    FieldInfo(FieldInfoCount)%IsInUse         = .true.
    FieldInfo(FieldInfoCount)%Name            = trim( FieldName )
    FieldInfo(FieldInfoCount)%ElementType     = trim( ElementType )
    FieldInfo(FieldInfoCount)%BytesPerElement = BytesPerElement
    FieldInfo(FieldInfoCount)%FieldRank       = FieldRank
    FieldInfo(FieldInfoCount)%ISize           = zISize
    FieldInfo(FieldInfoCount)%JSize           = zJSize
    FieldInfo(FieldInfoCount)%KSize           = zKSize
    FieldInfo(FieldInfoCount)%IHaloSize       = zIHaloSize
    FieldInfo(FieldInfoCount)%JHaloSize       = zJHaloSize
    FieldInfo(FieldInfoCount)%KPlusHaloSize   = zKPlusHaloSize
    FieldInfo(FieldInfoCount)%FileUnitNumber  = FileUnitNumber

    ! open file
    zFileName = trim( FileNamePrefix ) // trim( FieldName ) &
              // trim( FileNamePostfix)
    open( unit=FileUnitNumber, file=trim(zFileName), status='unknown', &
          form='unformatted', access='stream', iostat=zStatus )
    if ( zStatus /= 0 ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1201)
    end if

    ! convert numerical values to strings
    write(zS1,*) BytesPerElement
    write(zS2,*) FieldRank
    write(zS3,*) zISize
    write(zS4,*) zJSize
    write(zS5,*) zKSize
    write(zS6,*) zIHaloSize
    write(zS7,*) zJHaloSize
    write(zS8,*) zKPlusHaloSize

    ! construct JSON output
    ! changed by florian scheidegger at 14.01.2011
    ! fixed bug at 14.02.2011	    
    zString = Char10
    zString = trim(zString) // '{' // Char10
    zString = trim(zString) // '  "DataFieldInfo": {' // Char10
    zString = trim(zString) // '    "Name": ' // '"' // trim(FieldName) // '"' // ',' // Char10
    zString = trim(zString) // '    "ElementType": ' // '"' // trim(ElementType) // '"' // ',' // Char10
    zString = trim(zString) // '    "BytesPerElement": ' // trim(adjustl(zS1)) // ',' // Char10
    zString = trim(zString) // '    "Rank": ' // trim(adjustl(zS2)) ! deleted this: // ',' // Char10
    if (zISize /= -1) &
    zString = trim(zString) // ',' // Char10 // '    "ISize": ' // trim(adjustl(zS3)) ! deleted this:// ',' // Char10
    if (zJSize /= -1) &
    zString = trim(zString) // ',' // Char10 // '    "JSize": ' // trim(adjustl(zS4)) ! deleted this:// ',' // Char10
    if (zKSize /= -1) &
    zString = trim(zString) // ',' // Char10 // '    "KSize": ' // trim(adjustl(zS5)) ! deleted this:// ',' // Char10
    if (zIHaloSize /= -1) &
    zString = trim(zString) // ',' // Char10 // '    "IHaloSize": ' // trim(adjustl(zS6)) ! deleted this:// ',' // Char10
    if (zJHaloSize /= -1) &
    zString = trim(zString) // ',' // Char10 // '    "JHaloSize": ' // trim(adjustl(zS7)) ! deleted this:// ',' // Char10
    if (zKPlusHaloSize /= -1) &
    zString = trim(zString) // ',' // Char10 // '    "KPlusHaloSize": ' // trim(adjustl(zS8)) ! deleted this:// ',' // Char10
    zString = trim(zString) // Char10 // '  }' // Char10
    zString = trim(zString) // '}' // Char10
    zString = trim(zString) // Char10
    zString = trim(zString) // Char0

    ! debug log
    write(zS1,*) FileUnitNumber
    call fs_Debug('fs_WriteFieldInfo: writing JSON string to unit '//trim(adjustl(zS1)))

    ! write JSON output to file unit number
    write( unit=FileUnitNumber ) trim(zString)

    ! debug log
    call fs_Debug('fs_WriteFieldInfo: EXIT')

  end subroutine fs_WriteFieldInfo


  ! Module procedure: fs_SetupSavePoint
  ! Description: This function sets the information of the private global
  !   struct SavePointInfo, which is used for consecutive writing of fields
  !   by the fs_WriteData method. This method is typically called everything
  !   fields require dumping at a certain location in the code.
  subroutine fs_SetupSavePoint( SavePointName, LargeTimeStep, RKStageNumber, SmallTimeStep );
    implicit none

    ! arguments (in)
    character(len=*), intent(in)           :: SavePointName
    integer,          intent(in), optional :: LargeTimeStep
    integer,          intent(in), optional :: RKStageNumber
    integer,          intent(in), optional :: SmallTimeStep

    ! check if on
    if (.not. IsOn) return

    ! debug log
    call fs_Debug('fs_SetupSavePoint: '//trim(SavePointName))

    ! check arguments
    if ( len_trim( SavePointName ) > sl_Name ) &
      call fs_Error(1341)

    ! check if this is the first time round
    if (.not. IsSavePointOk) then
      IsSavePointOk = .true.
    end if

    ! setup save point
    ! non comment - else part has to be enabled (15.02.2011)
    SavePointInfo%Name = trim( SavePointName )
    if ( present( LargeTimeStep ) ) then
      SavePointInfo%LargeTimeStep = LargeTimeStep
    else
      SavePointInfo%LargeTimeStep = -1
    end if
    if ( present( RKStageNumber ) ) then
      SavePointInfo%RKStageNumber = RKStageNumber
    else
      SavePointInfo%RKStageNumber = -1
    end if
    if ( present( SmallTimeStep ) ) then
      SavePointInfo%SmallTimeStep = SmallTimeStep
    else
      SavePointInfo%SmallTimeStep = -1
    end if
    SavePointInfo%GlobalCounter = SavePointInfo%GlobalCounter + 1

    ! debug log
    call fs_Debug('fs_SetupSavePoint: EXIT')

  end subroutine fs_SetupSavePoint


  ! Module Procedure: GetMatchingFileUnitNumber
  ! Description: Search field by name in FieldInfo structure, check if arguments
  !   passed in are consistent, if yes return file unit number
  function GetMatchingFileUnitNumber( FieldName, FieldDataSize )
    implicit none

    ! arguments
    character(len=*), intent(in)  :: FieldName
    integer,          intent(in)  :: FieldDataSize(:)

    ! return value
    integer                       :: GetMatchingFileUnitNumber

    ! local variables
    integer        :: i
    integer        :: zFieldIndex, zDataSize

    ! check arguments
    if ( len_trim( FieldName ) > sl_Name ) &
      call fs_Error(1231)

    ! abort if no save point has been setup
    if (.not. IsSavePointOk) &
      call fs_Error(1001)

    ! abort if not FieldInfo records are around
    if ( FieldInfoCount < 1 ) &
      call fs_Error(1071)

    ! find matching field inde FieldInfo
    do i = 1, FieldInfoCount
      if ( trim(FieldInfo(i)%Name) == trim(FieldName) ) exit
    end do

    ! abort if no matching field is found
    if ( i > FieldInfoCount ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1073)
    end if

    ! found it!
    zFieldIndex = i

    ! check if total size of passed field match
    zDataSize = 1
    if ( FieldInfo(zFieldIndex)%ISize /= -1 ) &
      zDataSize=zDataSize*FieldInfo(zFieldIndex)%ISize
    if ( FieldInfo(zFieldIndex)%JSize /= -1 ) &
      zDataSize=zDataSize*FieldInfo(zFieldIndex)%JSize
    if ( FieldInfo(zFieldIndex)%KSize /= -1 ) &
      zDataSize=zDataSize*FieldInfo(zFieldIndex)%KSize

    if ( zDataSize /= product( FieldDataSize ) ) then
      write(0,*) 'field ', FieldName
      call fs_Error(1075)
    end if

    ! check if individual sizes match
    do i = 1, size(FieldDataSize)
      if ( .not. any( (/ FieldInfo(zFieldIndex)%ISize, &
                         FieldInfo(zFieldIndex)%JSize, &
                         FieldInfo(zFieldIndex)%KSize /) &
                      == FieldDataSize(i) ) ) &
        call fs_Error(1077)
    end do

    ! return found field index
    GetMatchingFileUnitNumber = FieldInfo(zFieldIndex)%FileUnitNumber

  end function GetMatchingFileUnitNumber


  ! Module Procedure: WriteSavePoint
  ! Description: Write current point in simulation in JSON format to
  !   a specified file unit number
  subroutine WriteSavePoint( FieldName, FileUnitNumber )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    integer,          intent(in) :: FileUnitNumber

    ! local variables
    character(len=1024)          :: zString
    character(len=30)            :: zS1, zS2, zS3, zS4

    ! check if on
    if (.not. IsOn) return

    ! convert numerical values to strings
    write(zS1,*) SavePointInfo%LargeTimeStep
    write(zS2,*) SavePointInfo%RKStageNumber
    write(zS3,*) SavePointInfo%SmallTimeStep
    write(zS4,*) SavePointInfo%GlobalCounter

    ! construct JSON output
    zString = Char10
    zString = trim(zString) // '{' // Char10
    ! bug fixed, tag is called SavePointInfo, not just SavePoint !!!
    zString = trim(zString) // '  "SavePointInfo": {' // Char10
    zString = trim(zString) // '    "Name": ' // '"' // trim(SavePointInfo%Name) // '"' !// ',' // Char10
    if ( SavePointInfo%LargeTimeStep /= -1 ) &
    zString = trim(zString) // ',' // Char10 // '    "LargeTimeStep": ' // trim(adjustl(zS1)) !// ',' // Char10
    if ( SavePointInfo%RKStageNumber /= -1 ) &
    zString = trim(zString) // ',' // Char10 // '    "RKStageNumber": ' // trim(adjustl(zS2)) ! // ',' // Char10
    if ( SavePointInfo%SmallTimeStep /= -1 ) &
    zString = trim(zString) // ',' // Char10 // '    "SmallTimeStep": ' // trim(adjustl(zS3)) !// ',' // Char10
    zString = trim(zString) // ',' // Char10 // '    "Id": ' // trim(adjustl(zS4)) // Char10
    zString = trim(zString) // '  }' // Char10
    zString = trim(zString) // '}' // Char10
    zString = trim(zString) // Char10
    zString = trim(zString) // Char0

    ! debug log
    write(zS1,*) FileUnitNumber
    call fs_Debug('fs_WriteData: writing JSON string to unit '//trim(adjustl(zS1)))

    ! write JSON output to file unit number
    write( unit=FileUnitNumber ) trim(zString)

  end subroutine WriteSavePoint


  ! Module Procedure: FinishWrite
  ! Description: Finish up IO after the binary field data has been written
  subroutine FinishWrite( FieldName, FileUnitNumber )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    integer,          intent(in) :: FileUnitNumber

    ! local variables
    character(len=30)            :: zS1

    ! check if on
    if (.not. IsOn) return

    ! debug log
    write(zS1,*) FileUnitNumber
    call fs_Debug('fs_WriteData: flushing I/O unit '//trim(adjustl(zS1)))

    ! flush output buffer if requested
    if (DoIOFlush) call flush( FileUnitNumber )

  end subroutine FinishWrite


  ! Module Procedure: fs_WriteData_0d_int
  ! Description: Serialization of rank 0 int values
  subroutine fs_WriteData_0d_int( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    integer         , intent(in) :: FieldData

    ! local variables
    integer     :: zFileUnitNumber
    integer     :: zEmptyIntArray(0)

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 0d int')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, zEmptyIntArray )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_0d_int


  ! Module Procedure: fs_WriteData_1d_int
  ! Description: Serialization of rank 1 int values
  subroutine fs_WriteData_1d_int( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    integer         , intent(in) :: FieldData(:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 1d int')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_1d_int


  ! Module Procedure: fs_WriteData_2d_int
  ! Description: Serialization of rank 2 int values
  subroutine fs_WriteData_2d_int( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    integer         , intent(in) :: FieldData(:,:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 2d int')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_2d_int


  ! Module Procedure: fs_WriteData_3d_int
  ! Description: Serialization of rank 3 int values
  subroutine fs_WriteData_3d_int( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    integer         , intent(in) :: FieldData(:,:,:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 3d int')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_3d_int


  ! Module Procedure: fs_WriteData_0d_real
  ! Description: Serialization of rank 0 real values
  subroutine fs_WriteData_0d_real( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    real            , intent(in) :: FieldData

    ! local variables
    integer     :: zFileUnitNumber
    integer     :: zEmptyIntArray(0)

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 0d real')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, zEmptyIntArray )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_0d_real


  ! Module Procedure: fs_WriteData_1d_real
  ! Description: Serialization of rank 1 real values
  subroutine fs_WriteData_1d_real( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    real            , intent(in) :: FieldData(:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 1d real')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_1d_real


  ! Module Procedure: fs_WriteData_2d_real
  ! Description: Serialization of rank 2 real values
  subroutine fs_WriteData_2d_real( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    real            , intent(in) :: FieldData(:,:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 2d real')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_2d_real


  ! Module Procedure: fs_WriteData_3d_real
  ! Description: Serialization of rank 3 real values
  subroutine fs_WriteData_3d_real( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    real            , intent(in) :: FieldData(:,:,:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 3d real')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_3d_real


  ! Module Procedure: fs_WriteData_0d_dble
  ! Description: Serialization of rank 0 dble values
  subroutine fs_WriteData_0d_dble( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    double precision, intent(in) :: FieldData

    ! local variables
    integer     :: zFileUnitNumber
    integer     :: zEmptyIntArray(0)

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 0d double')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, zEmptyIntArray )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_0d_dble


  ! Module Procedure: fs_WriteData_1d_dble
  ! Description: Serialization of rank 1 dble values
  subroutine fs_WriteData_1d_dble( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    double precision, intent(in) :: FieldData(:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 1d double')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_1d_dble


  ! Module Procedure: fs_WriteData_2d_dble
  ! Description: Serialization of rank 2 dble values
  subroutine fs_WriteData_2d_dble( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    double precision, intent(in) :: FieldData(:,:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 2d double')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_2d_dble


  ! Module Procedure: fs_WriteData_3d_dble
  ! Description: Serialization of rank 3 dble values
  subroutine fs_WriteData_3d_dble( FieldName, FieldData )
    implicit none

    ! arguments
    character(len=*), intent(in) :: FieldName
    double precision, intent(in) :: FieldData(:,:,:)

    ! local variables
    integer     :: zFileUnitNumber

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_WriteData: '//trim(FieldName)//' 3d double')

    ! implementation
    zFileUnitNumber = GetMatchingFileUnitNumber( FieldName, shape(FieldData) )
    call WriteSavePoint( FieldName, zFileUnitNumber )
    write( unit=zFileUnitNumber ) FieldData
    call FinishWrite( FieldName, zFileUnitNumber )

    ! debug
    call fs_Debug('fs_WriteData: EXIT')

  end subroutine fs_WriteData_3d_dble


  ! Module Procedure: fs_Finalize
  ! Description: Finalize module before shutdown
  subroutine fs_Finalize( )
    implicit none

    ! local variables
    integer :: i

    ! check if on
    if (.not. IsOn) return

    ! debug
    call fs_Debug('fs_Finalize: ')

    ! close all open files
    i = 1
    do while ( i <= max_NField .and. FieldInfo(i)%IsInUse )
      close( unit=FieldInfo(i)%FileUnitNumber )
      i = i + 1
    end do

    ! switch off
    IsOn = .false.

    ! debug
    call fs_Debug('fs_Finalize: EXIT')

  end subroutine fs_Finalize


  ! Module Procedure: fs_Error
  ! Description: Handle module errors in a very primitve way
  subroutine fs_Error( ErrorNumber )
    implicit none

    ! arguments
    integer, intent(in)     :: ErrorNumber

    ! local variables
    character(len=sl_Error) :: zErrorString

    ! immediate return if no error condition
    if ( ErrorNumber == 0 ) return

    ! what happened?
    select case ( ErrorNumber )
      case (1001)
        zErrorString = 'fs_WriteData called before setting up a save point'
      case (1071)
        zErrorString = 'fs_WriteData called but not fields registered'
      case (1073)
        zErrorString = 'fs_WriteData called but no matching field name registered'
      case (1075)
        zErrorString = 'total size of field passed to fs_WriteData does not match registered size'
      case (1077)
        zErrorString = 'sizes of field passed to fs_WriteData don''t match registered sizes'
      case (1101)
        zErrorString = 'no more available unit numbers'
      case (1201)
        zErrorString = 'problem opening output file'
      case (1231)
        zErrorString = 'field name string length exceeds allowed length'
      case (1233)
        zErrorString = 'type string length exceeds allowed length'
      case (1251)
        zErrorString = 'number of non -1 dimensions is not equal rank'
      case (1255)
        zErrorString = 'halo size is equal or greater and half of dimension size'
      case (1261)
        zErrorString = 'unreasonable size specified'
      case (1262)
        zErrorString = 'unreasonable halo size specified'
      case (1263)
        zErrorString = 'field with same name already registered'
      case (1291)
        zErrorString = 'halo size specified for unspecified dimension size'
      case (1341)
        zErrorString = 'save point name string length exceeds allowed length'
      case (1441)
        zErrorString = 'bytes per field is not within reasonable range'
      case (1451)
        zErrorString = 'rank must be one of 0, 1, 2, 3'
      case default
        zErrorString = 'Unknown Error'
    end select

    ! tell user
    write(0,*) 'ERROR in serialization module:'
    write(0,*) '  ErrorNumber: ', ErrorNumber
    write(0,*) '  Description: ' // trim( zErrorString )

    ! abort
    stop 1

  end subroutine fs_Error

  
  ! Module Procedure: fs_Debug
  ! Description: Handle module errors in a very primitve way
  subroutine fs_Debug( DebugString )
    implicit none

    ! arguments
    character(len=*), intent(in)  :: DebugString

    ! check if debugging is on
    if (.not. IsDebugOn) return

    ! write debug message
    write(0,*) 'fs(dbg): '//trim(DebugString)

  end subroutine fs_Debug

#endif

end module m_serializer
