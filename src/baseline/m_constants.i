# 1 "m_constants.f90"
! Module:
! constants
!
! Author:
! Oliver Fuhrer, Tue Jun 27 08:21:28 CEST 2006
! oliver.fuhrer@epfl.ch
!
! Description:
! This module contains mathematical and physical constants.

# 11
module constants
  implicit none

  ! numerical precision constants
  integer, parameter :: ir=selected_real_kind(12,200)

  ! fortran constants
  integer, parameter :: stderr=0
  integer, parameter :: stdin=5
  integer, parameter :: stdout=6

  ! mathematical constants
  real (kind=ir), parameter :: zero=0.0_ir
  real (kind=ir), parameter :: half=0.5_ir
  real (kind=ir), parameter :: one=1.0_ir
  real (kind=ir), parameter :: two=2.0_ir
  
end module constants
