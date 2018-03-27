!> this module solver the PDE
!>  \( \frac{\partial{\bf H}}{\partial t} - K\frac{\partial^{2}{\bf H}}{\partial x^{2}} = f(x) \)
module Solver_mod
  use Types_mod
  use RHS_mod
  
  implicit none

  public :: fd1d_heat_explicit_solver

contains
  !> solves the 1D heat equation
  subroutine fd1d_heat_explicit_solver( x, t, dt, cfl, h, h_new )

    implicit none

    integer(kind=SI)                           :: x_num
    !> the CFL number
    real(kind=DP), intent(in)                  :: cfl
    !> the time step
    real(kind=DP), intent(in)                  :: dt
    !> solution at time step n
    real(kind=DP), dimension(:), intent(in)    :: h
    !> the calculated solution at time step n + 1
    real(kind=DP), dimension(:), intent(inout) :: h_new
    integer(kind=SI)                           :: j
    !> time step
    real(kind=DP), intent(in)                  :: t
    !> spatial values
    real(kind=DP), dimension(:), intent(in)    :: x
    real(kind=DP)                              :: f(size( x ))

    x_num = size( x )
    
    do j = 1, x_num
      f(j) = func( j, x_num, x )
    end do

    h_new(1) = 0.0_DP

    do j = 2, x_num - 1
      != stencil readOnce, (reflexive(dim=1)) :: func
      != stencil readOnce, (reflexive(dim=1)) :: f
      != stencil (centered(depth=1, dim=1)) :: h
      h_new(j) = h(j) + dt * f(j) + cfl * ( h(j-1) - 2.0_DP * h(j) + h(j+1) )
    end do

    ! set the boundary conditions again
    h_new(1) = 90.0_DP
    h_new(x_num) = 70.0_DP
  end subroutine fd1d_heat_explicit_solver

end module Solver_mod
