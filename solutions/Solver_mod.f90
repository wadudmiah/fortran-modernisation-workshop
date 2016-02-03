module Solver_mod
  use Types_mod
  use RHS_mod
  
  implicit none

  public :: fd1d_heat_explicit_solver

contains
  subroutine fd1d_heat_explicit_solver( x_num, x, t, dt, cfl, h, h_new )

    implicit none

    integer(KIND=SI), intent(in) :: x_num
    real(KIND=DP), intent(in)    :: cfl
    real(KIND=DP)                :: f(x_num)
    real(KIND=DP), intent(in)    :: dt
    real(KIND=DP), intent(in)    :: h(x_num)
    real(KIND=DP), intent(inout) :: h_new(x_num)
    integer(KIND=SI)             :: j
    real(KIND=DP), intent(in)    :: t
    real(KIND=DP), intent(in)    :: x(x_num)

    do j = 1, x_num
      f(j) = func( j, x_num, x )
    end do

    h_new(1) = 0.0_DP

    do j = 2, x_num - 1
      h_new(j) = h(j) + dt * f(j) + cfl * ( h(j-1) - 2.0D+00 * h(j) + h(j+1) )
    end do

    ! set the boundary conditions again
    h_new(1) = 90.0_DP
    h_new(x_num) = 70.0_DP
  end subroutine fd1d_heat_explicit_solver

end module Solver_mod
