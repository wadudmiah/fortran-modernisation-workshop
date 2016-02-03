module IO_mod
  use Types_mod

  implicit none

  public :: r8mat_write, r8vec_write, r8vec_linspace
contains
  subroutine r8mat_write( output_filename, m, n, table )

    implicit none

    integer(KIND=SI), intent(in) :: m
    integer(KIND=SI), intent(in) :: n

    integer(KIND=SI) :: j
    character(len=*), intent(in) :: output_filename
    integer(KIND=SI) :: output_unit
    character(len=30) :: string 
    real(KIND=DP), intent(in) :: table(m, n)
 
    output_unit = 10
    open( unit = output_unit, file = output_filename, status = 'replace' )

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'

    do j = 1, n
      write ( output_unit, string ) table(1:m, j)
    end do

    close( unit = output_unit )
  end subroutine r8mat_write

  subroutine r8vec_write ( output_filename, n, x )

    implicit none

    integer(KIND=SI)             :: m
    integer(KIND=SI), intent(in) :: n

    integer(KIND=SI)             :: j
    character(len=*), intent(in) :: output_filename
    integer(KIND=SI)             :: output_unit
    real(KIND=DP), intent(in)    :: x(n)

    output_unit = 11
    open( unit = output_unit, file = output_filename, status = 'replace' )

    do j = 1, n
      write ( output_unit, '(2x,g24.16)' ) x(j)
    end do

    close ( unit = output_unit )
  end subroutine r8vec_write

  subroutine r8vec_linspace( n, a_first, a_last, a )

    implicit none

    integer(KIND=SI), intent(in) :: n
    real(KIND=DP), intent(inout) :: a(n)
    real(KIND=DP), intent(in) :: a_first
    real(KIND=DP), intent(in) :: a_last
    integer(KIND=SI) :: i

    do i = 1, n
      a(i) = ( dble( n - i ) * a_first + dble( i - 1 ) * a_last ) / dble( n - 1 )
    end do

  end subroutine r8vec_linspace  
end module IO_mod
