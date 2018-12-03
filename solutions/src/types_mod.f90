!> this module contains the real and integer kind variables
module types_mod
  use, intrinsic :: iso_fortran_env

  implicit none
  private
  public :: SP, DP, SI, DI
  !> 32-bit real
  integer, parameter :: SP = REAL32
  !> 64 bit real
  integer, parameter :: DP = REAL64

  !> 32-bit integer
  integer, parameter :: SI = INT32
  !> 64-bit integer
  integer, parameter :: DI = INT64

contains
  
end module types_mod
