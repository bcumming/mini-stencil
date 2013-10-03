!******************************************
! m_operators.f90
! Author: O. Fuhrer, 15.02.2011, MeteoSwiss
! Email: oliver.fuhrer@meteosiwss.ch
! *****************************************

! Description: Contains simple operators which can be used on 3d-meshes

module operators

use constants, only: ir

implicit none

contains

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

end subroutine adv_upwind5_y

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

end subroutine lap_4

!==============================================================================

subroutine diff_impl_z(s, s_out, kcoeff, nx, ny, nz, &
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
  integer :: i, j, k

!------------------------------------------------------------------------------

  allocate(a(nx, ny, nz),b(nx, ny, nz),c(nx, ny, nz),d(nx, ny, nz))

  ! compute tridiagonal matrix elements (a,b,c) and RHS (d)
  do j = jstart, jend
    do i = istart, iend
      b(i,j,1) = 1.0_ir+kcoeff
      c(i,j,1) = -kcoeff
      d(i,j,1) = s(i,j,1)
    end do
  end do
  do k = 2, nz-1
    do j = jstart, jend
      do i = istart, iend
        a(i,j,k) = -kcoeff
        b(i,j,k) = 1.0_ir+2.0_ir*kcoeff
        c(i,j,k) = -kcoeff
        d(i,j,k) = s(i,j,k)
      end do
    end do
  end do
  do j = jstart, jend
    do i = istart, iend
      a(i,j,nz) = -kcoeff
      b(i,j,nz) = 1.0_ir+kcoeff
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

end subroutine diff_impl_z

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
