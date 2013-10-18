!******************************************
! m_operators.f90
! based on min-app code written by Oliver Fuhrer, MeteoSwiss
! modified by Ben Cumming, CSCS
! *****************************************

! Description: Contains simple operators which can be used on 3d-meshes

module operators

use constants, only: ir
use stats,     only: flops_advx, flops_advy, flops_lap4, flops_bc, flops_difz

implicit none

! global variables
real (kind=ir), allocatable :: ccol(:,:,:)
real (kind=ir), allocatable :: dcol(:,:,:)

contains

!==============================================================================

subroutine init_operators( nx, ny, nz, ierr )
    integer, intent(in) :: nx, ny, nz
    integer, intent(out) :: ierr

    ! allocate temporary fields for tridiagonal solve
    allocate(ccol(nx,ny,nz), dcol(nx,ny,nz), stat=ierr)
    !call error(ierr /= 0, 'Problem allocating memory')

end subroutine init_operators

!==============================================================================

subroutine adv_upwind5_x( s, s_out, cfl, nx, ny, nz, &
                          istart, iend, jstart, jend, kstart, kend )

!------------------------------------------------------------------------------
!
! Description:
!   Calculate the advection   - u * ds / dx  ( u = dx / dt )
! 
! Method:
!   Upwind formula of 5th order
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(in)  :: s(nx, ny, nz)
  real (kind=ir), intent(out) :: s_out(nx, ny, nz)
  real (kind=ir), intent(in)  :: cfl(nx, ny, nz)
  integer, intent(in) :: istart, iend, jstart, jend, kstart, kend

! local variables
  integer :: i, j, k
  real (kind=ir) :: const1, cd_6order, diff_6order

  const1 = 1.0_ir / 60.0_ir

  do k = kstart, kend
    do j = jstart, jend
      do i = istart, iend

        cd_6order = cfl(i, j, k) *   ( ( s(i+3, j, k) - s(i-3, j, k) )     &
          &                -  9.0_ir * ( s(i+2, j, k) - s(i-2, j, k) )     &
          &                + 45.0_ir * ( s(i+1, j, k) - s(i-1, j, k) ) )

        diff_6order = abs( cfl(i, j, k) ) * ( ( s(i+3, j, k) + s(i-3, j, k) ) &
          &                       -  6.0_ir * ( s(i+2, j, k) + s(i-2, j, k) ) &
          &                       + 15.0_ir * ( s(i+1, j, k) + s(i-1, j, k) ) &
          &                       - 20.0_ir *   s(i  , j, k)    )

        s_out(i, j, k) = s(i, j, k) - const1 * ( cd_6order - diff_6order )

      end do
    end do
  end do

  ! update the flop counts
  ! the three operations in the stencil kernel can be abbreviated as follows
  ! we don't count the call to abs() as a floating point operation

  ! 8  ops   cd_6order = x * ((x-x) - x*(x-x) + x*(x-x))
  ! 11 ops   diff_6order = abs(x) * ( (x+x) - x * (x+x) + x * (x+x) - x * x)
  ! 3  ops   s_out(i, j, k) = x - x * (x-x)
  ! 21 ops total
  flops_advx =  flops_advx + 21 * (iend-istart+1)*(jend-jstart+1)*(kend-kstart+1)

end subroutine adv_upwind5_x

!==============================================================================

subroutine adv_upwind5_y(s, s_out, cfl, nx, ny, nz, &
                         istart, iend, jstart, jend, kstart, kend)

!------------------------------------------------------------------------------
!
! Description:
!   Calculate the advection   - v * ds / dy  (v = dy / dt)
! 
! Method:
!   Upwind formula of 5th order
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(in)  :: s(nx, ny, nz)
  real (kind=ir), intent(out) :: s_out(nx, ny, nz)
  real (kind=ir), intent(in)  :: cfl(nx, ny, nz)
  integer, intent(in) :: istart, iend, jstart, jend, kstart, kend

! local variables
  integer :: i, j, k
  real (kind=ir) :: const1, cd_6order, diff_6order

  const1 = 1.0_ir / 60.0_ir

  do k = kstart, kend
    do j = jstart, jend
      do i = istart, iend

        cd_6order = cfl(i, j, k) *   ( ( s(i, j+3, k) - s(i, j-3, k) )     &
          &                -  9.0_ir * ( s(i, j+2, k) - s(i, j-2, k) )     &
          &                + 45.0_ir * ( s(i, j+1, k) - s(i, j-1, k) ) )

        diff_6order = abs( cfl(i, j, k) ) * ( ( s(i, j+3, k) + s(i, j-3, k) ) & 
          &                       -  6.0_ir * ( s(i, j+2, k) + s(i, j-2, k) ) & 
          &                       + 15.0_ir * ( s(i, j+1, k) + s(i, j-1, k) ) &
          &                       - 20.0_ir *   s(i, j,   k)    )

        s_out(i, j, k) = s(i, j, k) - const1 * ( cd_6order - diff_6order )

     end do
    end do
  end do
  ! update the flop counts
  ! the three operations in the stencil kernel can be abbreviated as follows
  ! we don't count the call to abs() as a floating point operation

  ! 8  ops   cd_6order = x * ((x-x) - x*(x-x) + x*(x-x))
  ! 11 ops   diff_6order = abs(x) * ( (x+x) - x * (x+x) + x * (x+x) - x * x)
  ! 3  ops   s_out(i, j, k) = x - x * (x-x)
  ! 21 ops total
  flops_advy =  flops_advy + 21 * (iend-istart+1)*(jend-jstart+1)*(kend-kstart+1)

end subroutine adv_upwind5_y

!==============================================================================

subroutine d_to_a(s_out, u_in, v_in, w_in, cx, cy, cz, nx, ny, nz,             &
                  istart, iend, jstart, jend, kstart, kend)

!------------------------------------------------------------------------------
!
! Description:
!   Apply a 3D 6 point stencil operating on a D-grid vector field to obtain
!   a scalar field on an A-grid.  Divergence is the typical operation.
!   
! 
! Method:
!   Standard split finite difference operator (second order), which assumes 
!   equal but opposite coefficients on the upper and lower points
!
!------------------------------------------------------------------------------

! arguments
  real (kind=ir), intent(out) :: s_out(nx,  ny,   nz  )
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(in)  :: u_in(nx+1, ny,   nz  )
  real (kind=ir), intent(in)  :: v_in(nx,   ny+1, nz  )
  real (kind=ir), intent(in)  :: w_in(nx,   ny,   nz+1)
  real (kind=ir), intent(in)  :: cx(2), cy(2), cz(2)

  integer, intent(in) :: istart, iend, jstart, jend, kstart, kend

! local variables
  integer :: i, j, k

  const1 = 1.0_ir / 60.0_ir

  do k = kstart, kend
    do j = jstart, jend
      do i = istart, iend

        s_out(i,j,k)  =   cx(1)*u_in(i+1, j,   k  ) + cx(2)*u_in(i,   j,   k  ) +   &
          &               cy(1)*v_in(i,   j+1, k  ) + cy(2)*v_in(i,   j,   k  ) +   &
          &               cz(1)*w_in(i,   j,   k+1) + cz(2)*w_in(i,   j,   k  )

     end do
    end do
  end do

end subroutine d_to_a

!==============================================================================

subroutine lap_2(s, s_out, dcoeff, nx, ny, nz, &
                 istart, iend, jstart, jend, kstart, kend)

!------------------------------------------------------------------------------
!
! Description:
!   This subroutine caculates the Laplacian (lap) of an input array (s),
!   normalized by the grid spacing.
!
! Method:
!   The 2nd order centered differences. 
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(in) :: s(nx, ny, nz), dcoeff
  real (kind=ir), intent(out) :: s_out(nx, ny, nz)
  integer, intent(in) ::  istart, iend, jstart, jend, kstart, kend

! local variables
  integer :: i, j, k

!------------------------------------------------------------------------------

  do k = kstart, kend
    do j = jstart, jend
      do i = istart, iend
        s_out(i, j, k) = s(i, j, k) - dcoeff * (  &
             + s(i+1, j  , k) + s(i-1, j  , k)    &
             + s(i  , j+1, k) + s(i  , j-1, k)    &
             - 4.0_ir*s(i, j, k)                )
      end do
    end do
  end do
  
end subroutine lap_2

!==============================================================================

subroutine lap_4(s, s_out, dcoeff, nx, ny, nz, &
                 istart, iend, jstart, jend, kstart, kend)

!------------------------------------------------------------------------------
!
! Description:
!   This subroutine caculates the linear forth order horizontal diffusion
!   operator - the Laplacian of the Laplacian - of an input array 's' 
!
! Method:
!   The 2nd order centered differences.
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(in) :: s(nx, ny, nz), dcoeff
  real (kind=ir), intent(out) :: s_out(nx, ny, nz)
  integer, intent(in) :: istart, iend, jstart, jend, kstart, kend

! local variables
  real (kind=ir), allocatable :: work(:,:)
  integer :: i, j, k

!------------------------------------------------------------------------------

  allocate(work(nx, ny))

  do k = kstart, kend
    do j = jstart-1, jend+1
      do i = istart-1, iend+1
        work(i, j) = + s(i+1, j  , k) + s(i-1, j  , k)  &
                     + s(i  , j+1, k) + s(i  , j-1, k)  &
                    - 4.0_ir*s(i, j, k)
      end do
    end do
    do j = jstart, jend
      do i = istart, iend
        s_out(i, j, k) = s(i, j, k) - dcoeff * (  &
            + work(i+1, j  ) + work(i-1, j  )     &
            + work(i  , j+1) + work(i  , j-1)     &
            - 4.0_ir*work(i, j) )
      end do
    end do
  end do

  deallocate(work)

  ! update the flop counts
  ! the two operations in the stencil kernels above can be abbreviated as follows
  ! lap2 kernel : range k, i-1:i+1, j-1:j+1
  !     5 ops   work(i,j) = x + x + x + x - x*x
  ! lap4 kernel : range k, i, j
  !     7 ops   s_out(i,j) = x - x*(x+x+x+x-x*x)

  flops_lap4 =  flops_lap4 + 5 * (iend-istart+3)*(jend-jstart+3)*(kend-kstart+1) &
                           + 7 * (iend-istart+1)*(jend-jstart+1)*(kend-kstart+1)

end subroutine lap_4

!==============================================================================
subroutine diff_impl_z_lu(s_in, s_out, kbar, nx, ny, nz, istart, iend, jstart, jend)
!------------------------------------------------------------------------------
! Description:
!   This subroutine caculates the implicit vertical diffusion operator
!   of an input array 's' using constant vertical grid spacing and diffusion
!   coefficient
!
! Method:
!   Tridiagonal solve using Thomas' algorithm
!------------------------------------------------------------------------------

    ! arguments
    integer, intent(in) :: nx, ny, nz
    real (kind=ir), intent(in)  :: s_in(nx, ny, nz)
    real (kind=ir), intent(out) :: s_out(nx, ny, nz)
    real (kind=ir), intent(in)  :: kbar
    integer, intent(in) :: istart, iend, jstart, jend

    ! local variables
    integer :: i, j, k
    real (kind=ir) :: btmp, dtmp
    real (kind=ir) :: update

    !======================================
    ! forward substitution phase
    !======================================
    ! do first step : k=1
    do j = jstart, jend
        do i = istart, iend
            btmp = kbar + 1.0_ir
            ccol(i,j,1) = -1.0_ir*kbar/btmp
            dcol(i,j,1) = s_in(i,j,1)/btmp
        end do
    end do
    ! do middle steps : k=2:N-1
    do k = 2, nz-1
        do j = jstart, jend
            do i = istart, iend
                btmp = 2.0_ir*kbar + 1.0_ir + kbar*ccol(i,j,k-1)
                dtmp = s_in(i,j,k) + kbar*dcol(i,j,k-1)
                ccol(i,j,k) = -1.0_ir * kbar / btmp
                dcol(i,j,k) = dtmp / btmp
            end do
        end do
    end do
    ! do last step : k=N
    do j = jstart, jend
        do i = istart, iend
            btmp = Kbar + 1.0_ir + Kbar*ccol(i,j,nz-1)
            dtmp = s_in(i,j,nz) + Kbar*dcol(i,j,nz-1)
            dcol(i,j,nz) = dtmp / btmp
            s_out(i,j,nz) = dcol(i,j,nz)
        end do
    end do

    !======================================
    ! backward substitution phase
    !======================================
    ! do steps : k=N-1:-1:1
    do k = nz-1, 1, -1
        do j = jstart, jend
            do i = istart, iend
                ! for some reason the following has to be done in two steps
                ! (with temporary variable) to produce correct results for
                ! the cray compiler. If a more experience Fortran programmer
                ! can fix this, please do! The original one line statment is
                ! commented out below
                update = ccol(i,j,k)*s_out(i,j,k+1)
                s_out(i,j,k) = dcol(i,j,k) - update
                !s_out(i,j,k) = dcol(i,j,k) - ccol(i,j,k)*s_out(i,j,k+1)
            end do
        end do
    end do

    !4 * i * j
    !9 * i * j * k-2
    !6 * i * j
    !2 * i * j * k-1
    ! =>
    ! 12 * i * j
    ! 11 * i * j * k-2
  flops_difz =  flops_difz + 12*(iend-istart+1)*(jend-jstart+1) &
                           + 11*(iend-istart+1)*(jend-jstart+1)*(nz-2)


end subroutine diff_impl_z_lu

subroutine diff_impl_z_ul(s, s_out, kcoeff, nx, ny, nz, &
                       istart, iend, jstart, jend)

!------------------------------------------------------------------------------
!
! Description:
!   This subroutine caculates the implicit vertical diffusion operator
!   of an input array 's' using constant vertical grid spacing and diffusion
!   coefficient
!
! Method:
!   Tridiagonal solve using Thomas' algorithm
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(in) :: s(nx, ny, nz)
  real (kind=ir), intent(out) :: s_out(nx, ny, nz)
  real (kind=ir), intent(in) :: kcoeff
  integer, intent(in) :: istart, iend, jstart, jend

! local variables
  real (kind=ir), allocatable :: a(:,:,:), b(:,:,:), c(:,:,:), d(:,:,:)
  real (kind=ir)  :: kneg, kplus1, kby2plus1
  integer :: i, j, k

!------------------------------------------------------------------------------

  allocate(a(nx, ny, nz),b(nx, ny, nz),c(nx, ny, nz),d(nx, ny, nz))
  kneg   = -kcoeff
  kplus1 = 1.0_ir+kcoeff
  kby2plus1 = 1.0_ir+2.0_ir*kcoeff

  ! compute tridiagonal matrix elements (a,b,c) and RHS (d)
  do j = jstart, jend
    do i = istart, iend
      !b(i,j,1) = 1.0_ir+kcoeff
      !c(i,j,1) = -kcoeff
      b(i,j,1) = kplus1
      c(i,j,1) = kneg
      d(i,j,1) = s(i,j,1)
    end do
  end do
  do k = 2, nz-1
    do j = jstart, jend
      do i = istart, iend
        !a(i,j,k) = -kcoeff
        !b(i,j,k) = 1.0_ir+2.0_ir*kcoeff
        !c(i,j,k) = -kcoeff
        a(i,j,k) = kneg
        b(i,j,k) = kby2plus1
        c(i,j,k) = kneg
        d(i,j,k) = s(i,j,k)
      end do
    end do
  end do
  do j = jstart, jend
    do i = istart, iend
      !a(i,j,nz) = -kcoeff
      !b(i,j,nz) = 1.0_ir+kcoeff
      a(i,j,nz) = kneg
      b(i,j,nz) = kplus1
      d(i,j,nz) = s(i,j,nz)
    end do
  end do

  ! UL-decomposition
  do j = jstart, jend
    do i = istart, iend
      b(i,j,nz) = 1.0_ir/b(i,j,nz)
      a(i,j,nz) = a(i,j,nz)*b(i,j,nz)
    end do
  end do
  do k = nz-1, 1, -1
    do j = jstart, jend
      do i = istart, iend
        b(i,j,k) = 1.0_ir/(b(i,j,k) - c(i,j,k)*a(i,j,k+1))
        a(i,j,k) = a(i,j,k)*b(i,j,k)
      end do
    end do
  end do

  ! forward elimination
  do j = jstart, jend
    do i = istart, iend
      d(i,j,nz) = d(i,j,nz)*b(i,j,nz)
    end do
  end do
  do k = nz-1, 1, -1
    do j = jstart, jend
      do i = istart, iend
        d(i,j,k) = b(i,j,k)*( d(i,j,k) - d(i,j,k+1)*c(i,j,k) )
      end do
    end do
  end do

  ! back substitution
  do j = jstart, jend
    do i = istart, iend
      s_out(i,j,1) = d(i,j,1)
    end do
  end do
  do k = 2, nz
    do j = jstart, jend
      do i = istart, iend
        s_out(i,j,k) = -a(i,j,k)*s_out(i,j,k-1) + d(i,j,k)
      end do
    end do
  end do

  deallocate(a,b,c,d)

  ! update the flop counts
  ! the two operations in the stencil kernels above can be abbreviated as follows
  !  UL - decomposition
  !      2 i*j
  !      4 i*j*(k-1)
  ! forward substitution
  !      1 i*j
  !      3 i*j*(k-1)
  ! backward substitution
  !      2 i*j*(k-1)

  flops_difz =  flops_difz + 3*(iend-istart+1)*(jend-jstart+1) &
                           + 9*(iend-istart+1)*(jend-jstart+1)*(nz-1)

end subroutine diff_impl_z_ul

!==============================================================================

subroutine bc_zerograd( s, nb, bc_west, bc_east, bc_south, bc_north, &
                        nx, ny, nz, istart, iend, jstart, jend, kstart, kend )

!------------------------------------------------------------------------------
!
! Description:
!   Applies zero-gradient boundary condition to s-field of halo-width nb
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(inout)  :: s(nx, ny, nz)
  integer, intent(in) :: nb
  logical, intent(in) :: bc_west, bc_east, bc_south, bc_north
  integer, intent(in) :: istart, iend, jstart, jend, kstart, kend

! local variables
  integer :: i, j, k
  integer :: gis, gie, gjs, gje

  ! setup indices
  if ( bc_west ) then
    gis = istart-nb
  else
    gis = istart
  end if
  if ( bc_east ) then
    gie = iend+nb
  else
    gie = iend
  end if
  if ( bc_south ) then
    gjs = jstart-nb
  else
    gjs = jstart
  end if
  if ( bc_north ) then
    gje = jend+nb
  else
    gje = jend
  end if

  ! apply boundary condition
  if ( bc_west ) then
    do k = kstart, kend
      do j = gjs, gje
        do i = istart-nb, istart-1
          s(i,j,k) = s(istart,j,k)
        end do
      end do
    end do
  end if
  if ( bc_east ) then
    do k = kstart, kend
      do j = gjs, gje
        do i = iend+1, iend+nb
          s(i,j,k) = s(iend,j,k)
        end do
      end do
    end do
  end if
  if ( bc_south ) then
    do k = kstart, kend
      do j = jstart-nb, jstart-1
        do i = gis, gie
          s(i,j,k) = s(i,jstart,k)
        end do
      end do
    end do
  end if
  if ( bc_north ) then
    do k = kstart, kend
      do j = jend+1, jend+nb
        do i = gis, gie
          s(i,j,k) = s(i,jend,k)
        end do
      end do
    end do
  end if

end subroutine bc_zerograd

!==============================================================================

subroutine bc_zerovalue( s, nb, bc_west, bc_east, bc_south, bc_north, &
                         nx, ny, nz, istart, iend, jstart, jend, kstart, kend )

!------------------------------------------------------------------------------
!
! Description:
!   Applies zero-value boundary condition to s-field of halo-width nb
!
!------------------------------------------------------------------------------

! arguments
  integer, intent(in) :: nx, ny, nz
  real (kind=ir), intent(inout)  :: s(nx, ny, nz)
  integer, intent(in) :: nb
  logical, intent(in) :: bc_west, bc_east, bc_south, bc_north
  integer, intent(in) :: istart, iend, jstart, jend, kstart, kend

! local variables
  integer :: i, j, k
  integer :: gis, gie, gjs, gje

  ! setup indices
  if ( bc_west ) then
    gis = istart-nb
  else
    gis = istart
  end if
  if ( bc_east ) then
    gie = iend+nb
  else
    gie = iend
  end if
  if ( bc_south ) then
    gjs = jstart-nb
  else
    gjs = jstart
  end if
  if ( bc_north ) then
    gje = jend+nb
  else
    gje = jend
  end if

  ! apply boundary condition
  if ( bc_west ) then
    do k = kstart, kend
      do j = gjs, gje
        do i = istart-nb, istart-1
          s(i,j,k) = 0.0_ir
        end do
      end do
    end do
  end if
  if ( bc_east ) then
    do k = kstart, kend
      do j = gjs, gje
        do i = iend+1, iend+nb
          s(i,j,k) = 0.0_ir
        end do
      end do
    end do
  end if
  if ( bc_south ) then
    do k = kstart, kend
      do j = jstart-nb, jstart-1
        do i = gis, gie
          s(i,j,k) = 0.0_ir
        end do
      end do
    end do
  end if
  if ( bc_north ) then
    do k = kstart, kend
      do j = jend+1, jend+nb
        do i = gis, gie
          s(i,j,k) = 0.0_ir
        end do
      end do
    end do
  end if

end subroutine bc_zerovalue

!==============================================================================

end module operators
