!> this module calculates the CFL number
module CFL_mod
  use Types_mod

  implicit none

  public :: fd1d_heat_explicit_cfl
contains

  !> calculates the CFL number
  !> \( \text{CFL} = \kappa\frac{\Delta t}{\Delta x^2} \)
  subroutine fd1d_heat_explicit_cfl( k, t_num, t_min, t_max, x_num, x_min, x_max, cfl )
    implicit none
    !> calculated CFL number
    real(KIND=DP), intent(inout) :: cfl
    real(KIND=DP) :: dx
    real(KIND=DP) :: dt
    !> the heat constant \( \kappa \)
    real(KIND=DP), intent(in) :: k
    !> t_max upper bound of t-axis
    real(KIND=DP), intent(in) :: t_max
    !> lower bound of t-axis
    real(KIND=DP), intent(in) :: t_min
    !> number of intervals in t-axis
    integer(KIND=SI), intent(in) :: t_num
    !> upper bound of x-axis
    real(KIND=DP), intent(in) :: x_max
    !> x_min lower bound of x-axis
    real(KIND=DP), intent(in) :: x_min
    !> number of intervals in x-axis
    integer(KIND=SI), intent(in) :: x_num

    dx = ( x_max - x_min ) / dble( x_num - 1 )
    dt = ( t_max - t_min ) / dble( t_num - 1 )

    cfl = k * dt / dx / dx
      
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  CFL stability criterion value = ', cfl

  end subroutine fd1d_heat_explicit_cfl
  
end module CFL_mod
