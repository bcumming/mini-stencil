!******************************************
! extended_examples
! Author: O. Fuhrer, 15.02.2011, MeteoSwiss
! Email: oliver.fuhrer@meteosiwss.ch
! *****************************************

! Description: A small test program solving diffusion equation using
!   simple finite differences.

! Syntax:
!   extended_examples nx ny nz nt nb

program extended_examples

  ! modules

  use constants, only: ir

  use operators, only: bc_zerograd, adv_upwind5_x, adv_upwind5_y, &
                       lap_2, lap_4, diff_impl_z, bc_zerovalue

#ifdef _NETCDF
  use cdf
#endif

#ifdef _SERIALIZE
  use m_serializer, only: fs_Initialize, fs_WriteFieldInfo, fs_SetupSavePoint, &
                          fs_WriteData, fs_Finalize
#endif

  implicit none

  ! variables

  integer :: nx, ny, nz, nt, nb
  real (kind=ir), allocatable :: data_in(:,:,:), data_out(:,:,:)
  real (kind=ir), allocatable :: cflx(:,:,:), cfly(:,:,:)
  real (kind=ir), allocatable :: dcoeff, kcoeff

  integer :: ierr
  integer :: i, j, k, it
  integer :: istart, iend, jstart, jend, kstart, kend
  integer :: nincout
  integer :: iseed(80), nseed

#ifdef _NETCDF
  character(len=255) :: fname = 'out.nc'
#endif

  write(*,'(a)') 'Welcome to stencil3d!'

  ! ****************** read command line arguments ******************

  ! read command line arguments
  call readcmdline(nx, ny, nz, nt, nb)

  ! add halo points to total size
  nx = nx + 2*nb
  ny = ny + 2*nb

  ! ****************** setup compute domain ******************

  ! determine compute region (without halo) on global grid
  istart = 1 + nb
  iend   = nx - nb
  jstart = 1 + nb
  jend   = ny - nb
  kstart = 1
  kend   = nz

  ! ****************** allocate memory ******************

  ! allocate global fields
  allocate(data_in(nx,ny,nz), data_out(nx,ny,nz), &
           cflx(nx,ny,nz), cfly(nx,ny,nz), stat=ierr)
  call error(ierr /= 0, 'Problem allocating memory')

  ! initialize to zero
  data_in = 0.0_ir
  data_out = 0.0_ir
  cflx = 0.0_ir
  cfly = 0.0_ir

  ! ****************** initialization ******************

  ! initialize random numbers
  call random_seed(size=nseed)
  call error(nseed>80, 'Size of random number seed exceeds 20')
  iseed(:) = 42
  call random_seed(put=iseed(1:nseed))

  ! initialize with random numbers in halo
  call random_number(data_in)

  ! initialize initial condition in calculation domain
  do k = kstart, kend
    do j = jstart, jend
      do i = istart, iend
        if ((i-nx/2)**2 + (j-ny/2)**2 + (k-nz/2)**2 < 15**2) then
          data_in(i,j,k) = 1.0_ir
        else
          data_in(i,j,k) = 0.0_ir
        end if
      end do
    end do
  end do

  ! setup vertical diffusion coefficient
  kcoeff = 0.1_ir

  ! setup horizontal diffusion coefficient
  dcoeff = 0.01_ir

  ! setup Courant number for x- and y-advection
!  call random_number(cflx)
!  cflx = 0.2_ir*cflx - 0.1_ir
!  call random_number(cfly)
!  cfly = 0.2_ir*cfly - 0.1_ir
  cflx = 0.09_ir
  cfly = 0.11_ir

#ifdef _SERIALIZE
  ! initialize serialization framework and serialize constant fields
  call fs_Initialize();
  call fs_WriteFieldInfo('data', 'double', 8, 3, nx, ny, nz, nb, nb, 0)
  call fs_WriteFieldInfo('cflx', 'double', 8, 3, nx, ny, nz, nb, nb, 0)
  call fs_WriteFieldInfo('cfly', 'double', 8, 3, nx, ny, nz, nb, nb, 0)
  call fs_WriteFieldInfo('dcoeff', 'double', 8, 0)
  call fs_WriteFieldInfo('kcoeff', 'double', 8, 0)
  call fs_SetupSavePoint('Init')
  call fs_WriteData('cflx', cflx)
  call fs_WriteData('cfly', cfly)
  call fs_WriteData('dcoeff', dcoeff)
  call fs_WriteData('kcoeff', kcoeff)
#endif

#ifdef _NETCDF
  ! open file and write constant data and initial condition
  call cdfbegin(trim(fname), 'extended_examples', create=.true., overwrite=.true.)
  call cdftime('time','timesteps')
  call cdfdimput('nx', nx)
  call cdfdimput('ny', ny)
  call cdfdimput('nz', nz)
!  call cdfvarput('cflx', cflx, (/'nx','ny','nz'/), units='1', &
!                 flip=.true.)
!  call cdfvarput('cfly', cfly, (/'nx','ny','nz'/), units='1', &
!                 flip=.true.)
!  call cdfvarput('dcoeff', dcoeff, (/''/), units='1', flip=.true.)
!  call cdfvarput('kcoeff', kcoeff, (/''/), units='1', flip=.true.)
  call cdfvarput('data', data_in, (/'nx','ny','nz'/), units='1', &
                 time=0.0_ir, flip=.true.)
#endif

  ! ****************** serial reference version ******************

  ! main timeloop
  do it = 1, nt

    write(*,'(a,i5)') 'Step', it

#ifdef _SERIALIZE
    if (it == 1) then
      call fs_SetupSavePoint('AdvectionX.in', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
    end if
#endif

    ! x-advection
    call bc_zerograd( data_in, 3, .true., .true., .false., .false., &
                      nx, ny, nz, istart, iend, jstart, jend, kstart, kend )
    call adv_upwind5_x( data_in, data_out, cflx, &
                        nx, ny, nz, istart, iend, jstart, jend, kstart, kend )
    call swap_data()

#ifdef _SERIALIZE
    if (it == 1) then
      call fs_SetupSavePoint('AdvectionX.out', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
      call fs_SetupSavePoint('AdvectionY.in', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
    end if
#endif

    ! y-advection
    call bc_zerograd( data_in, 3, .false., .false., .true., .true., &
                      nx, ny, nz, istart, iend, jstart, jend, kstart, kend )
    call adv_upwind5_y( data_in, data_out, cfly, &
                        nx, ny, nz, istart, iend, jstart, jend, kstart, kend )
    call swap_data()

#ifdef _SERIALIZE
    if (it == 1) then
      call fs_SetupSavePoint('AdvectionY.out', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
      call fs_SetupSavePoint('HorizontalDiffusion.in', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
    end if
#endif

    ! xy-diffusion
    call bc_zerograd( data_in, 2, .true., .true., .true., .true., &
                      nx, ny, nz, istart, iend, jstart, jend, kstart, kend )
    call lap_4( data_in, data_out, dcoeff, &
                nx, ny, nz, istart, iend, jstart, jend, kstart, kend )
    call swap_data()

#ifdef _SERIALIZE
    if (it == 1) then
      call fs_SetupSavePoint('HorizontalDiffusion.out', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
      call fs_SetupSavePoint('VerticalDiffusion.in', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
    end if
#endif

    ! z-diffusion (implicit)
    call diff_impl_z( data_in, data_out, kcoeff, &
                      nx, ny, nz, istart, iend, jstart, jend )
    call swap_data()

#ifdef _SERIALIZE
    if (it == 1) then
      call fs_SetupSavePoint('VerticalDiffusion.out', LargeTimeStep=it)
      call fs_WriteData('data', data_in)
    end if
#endif

#ifdef _NETCDF
  ! write NetCDF output for reference solution
  if (mod(it,nincout) == 0) then
    write(*,'(a)') 'NetCDF output to '//trim(fname)
    call cdfvarput('data', data_in, (/'nx','ny','nz'/), units='1', &
                   time=real(it,ir), flip=.true.)
  end if
#endif

  end do

  ! ****************** cleanup ******************

#ifdef _NETCDF
  ! close file
  call cdfend()
#endif

#ifdef _SERIALIZE
  ! finalize the serializing system
  call fs_Finalize()
#endif

  ! deallocate global fields
  deallocate(data_in, data_out, cflx, cfly)

  write(*,'(a)') 'Goodbye!'

contains

!==============================================================================

! swap data_in and data_out fields
! note: this is achieved without copying by using the Fortran move_alloc intrinsic
subroutine swap_data()
  implicit none

  real (kind=ir), allocatable :: tmp(:,:,:)

  call move_alloc(data_out, tmp)
  call move_alloc(data_in, data_out)
  call move_alloc(tmp, data_in)

end subroutine swap_data

!==============================================================================

! read command line arguments
subroutine readcmdline(nx, ny, nz, nt, nb)
  implicit none

  ! arguments
  integer, intent(out) :: nx, ny, nz, nt, nb

  ! local
  character(len=256) :: sarg

  if (command_argument_count() /= 5) then
    write(*,*) 'Usage: extended_examples nx ny nz nt nb'
    write(*,*) '  nx  number of gridpoints in x-direction'
    write(*,*) '  ny  number of gridpoints in y-direction'
    write(*,*) '  nz  number of gridpoints in z-direction'
    write(*,*) '  nt  number of timesteps'
    write(*,*) '  nb  width of halo region'
    stop
  end if

  ! read nx
  call get_command_argument(1, sarg)
  nx = -1
  read(sarg,*) nx
  call error(nx<1, 'nx must be positive integer')

  ! read ny
  call get_command_argument(2, sarg)
  ny = -1
  read(sarg,*) ny
  call error(ny<1, 'ny must be positive integer')

  ! read nz
  call get_command_argument(3, sarg)
  nz = -1
  read(sarg,*) nz
  call error(nz<1, 'nz must be positive integer')

  ! read nt
  call get_command_argument(4, sarg)
  nt = -1
  read(sarg,*) nt
  call error(nt<1, 'nt must be positive integer')

  ! read nb
  call get_command_argument(5, sarg)
  nb = -1
  read(sarg,*) nb
  call error(nb<1, 'nb must be positive integer')

  ! compute NetCDF output interval
  nincout = max(1, nt/20)

end subroutine readcmdline

!==============================================================================

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

!==============================================================================

end program extended_examples