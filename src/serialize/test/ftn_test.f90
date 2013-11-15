!+ test the fortran interface to the serialization framework
!------------------------------------------------------------------------------

program ftn_test

!------------------------------------------------------------------------------
! modules
!------------------------------------------------------------------------------
    use iso_c_binding
    use m_serializer

    implicit none
!------------------------------------------------------------------------------
! variables
!------------------------------------------------------------------------------

    real(c_double), dimension(:),     allocatable :: data_1d
    real(c_double), dimension(:,:),   allocatable :: data_2d
    real(c_double), dimension(:,:,:), allocatable :: data_3d

    ! i, j, k and halo dimensions of fields
    integer :: iDim, jDim, kDim, hDim
    integer :: ierr
    integer :: i, j, k

!------------------------------------------------------------------------------
! main
!------------------------------------------------------------------------------
    print *, 'Fortran wrapper test'

    ! initialize dimensions
    iDim = 8
    jDim = 8
    kDim = 2
    hDim = 2

    ! allocate memory
    allocate(data_1d(iDim), data_2d(iDim,jDim), data_3d(iDim,jDim,kDim), stat=ierr)
    call error(ierr /= 0, 'Problem allocating memory')

    ! initialize memory
    do i=1, iDim
        data_1d(i) = i;
    end do
    do j=1, jDim
        do i=1, iDim
            data_2d(i,j) = i+j;
        end do
    end do
    do k=1, kDim
        do j=1, jDim
            do i=1, iDim
                data_3d(i,j,k) = i+j+k;
            end do
        end do
    end do

    ! initialize serialization framework
    call fs_Initialize(.true., .true., '.', 'ftest')

    ! register fields for output
    call fs_RegisterField ('1D', 'double', idim, 1, 1, 1, -hdim, hdim, 0, 0, 0, 0, 0, 0)
    call fs_RegisterField ('2D', 'double', idim, jdim, 1, 1, -hdim, hdim, -hdim, hdim, 0, 0, 0, 0)
    call fs_RegisterField ('3D', 'double', idim, jdim, kdim, 1, -hdim, hdim, -hdim, hdim, 0, 0, 0, 0)

    ! set up savepoint
    call fs_SetSavePointName('first')
    call fs_AddSavePointInfo_i('timestep', 1)

    ! save data
    call fs_WriteData_d('1D', data_1d)
    call fs_WriteData_d('2D', data_2d)
    call fs_WriteData_d('3D', data_3d)

    ! set up savepoint
    call fs_SetSavePointName('second')
    call fs_AddSavePointInfo_i('timestep', 2)

    ! save data
    call fs_WriteData_d('1D', data_1d)
    call fs_WriteData_d('2D', data_2d)
    call fs_WriteData_d('3D', data_3d)

    ! finalize serialization
    call fs_Finalize()

    ! free memory
    deallocate(data_1d, data_2d, data_3d)
!------------------------------------------------------------------------------
! subroutines
!------------------------------------------------------------------------------
contains

! write error message and terminate
subroutine error(yes, msg)
  implicit none

  ! arguments
  logical, intent(in) :: yes
  character(len=*), intent(in) :: msg

  ! local
  integer, external :: lnblnk

  if (yes) then
    write(0,*) 'FATAL PROGRAM ERROR!'
    write(0,*) msg
    write(0,*) 'Execution aborted...'
    stop
  end if

end subroutine error
end program ftn_test
