module RHS_mod

  use Types_mod
  
  implicit none

  public :: func
  
contains
  
  function func( j, x_num, x ) result ( d )
    integer(KIND=SI), intent(in) :: j, x_num
    real(KIND=DP) :: d
    real(KIND=DP), intent(in) :: x(:)

    d = 0.0_DP
  end function func
  
end module RHS_mod
