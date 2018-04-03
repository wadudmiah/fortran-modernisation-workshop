!> this module deals with netcdf I/O

module IO_mod
  use Types_mod
  use netcdf
  
  implicit none
  private
  public :: r8mat_write, r8vec_write, r8vec_linspace
contains
  subroutine r8mat_write( output_filename, table, x, t )

    implicit none

    integer(KIND=SI) :: m ! x_num
    integer(KIND=SI) :: n ! t_num

    !> x-values (spatial direction)
    real(KIND=DP), intent(in)    :: x(:)
    !> t-values (time direction)
    real(kind=DP), intent(in)    :: t(:)
    
    integer(KIND=SI)             :: j
    !> the output file name 
    character(len=*), intent(in) :: output_filename
    !> the 2D array with the solution
    real(KIND=DP), intent(in)    :: table(:, :)

    ! netcdf parameters
    integer(KIND=SI), parameter :: NDIMS = 2
    integer(KIND=SI)            :: ncid, x_id, t_id, sol_id, dimids(1:NDIMS)
    integer(KIND=SI)            :: x_dimid, t_dimid, ierr

    ierr = NF90_CREATE( output_filename, NF90_CLOBBER, ncid )

    m = size( x )
    n = size( t )
    ! define the dimensions
    ierr = NF90_DEF_DIM( ncid, "x", m, x_dimid )
    ierr = NF90_DEF_DIM( ncid, "t", n, t_dimid ) 

    ! define the x- and t-range variables
    ierr = NF90_DEF_VAR( ncid, "x-range", NF90_DOUBLE, x_dimid, x_id )
    ierr = NF90_DEF_VAR( ncid, "t-range", NF90_DOUBLE, t_dimid, t_id )

    ! define global attributes
    ierr = NF90_PUT_ATT( ncid, NF90_GLOBAL, "purpose", "Fortran workshop" )
    ierr = NF90_PUT_ATT( ncid, NF90_GLOBAL, "name", "Wadud Miah" )
    ierr = NF90_PUT_ATT( ncid, NF90_GLOBAL, "institution", "NAG" )
    
    ! define dimension attributes
    ierr = NF90_PUT_ATT( ncid, x_id, "units", "metres" )
    ierr = NF90_PUT_ATT( ncid, t_id, "units", "seconds" )

    ! define the solution matrix
    dimids = [ x_dimid, t_dimid ]
    ierr = NF90_DEF_VAR( ncid, "solution", NF90_DOUBLE, dimids, sol_id )
    ierr = NF90_PUT_ATT( ncid, sol_id, "units", "Celsius" )
    
    ! end define mode
    ierr = NF90_ENDDEF( ncid )

    ! enter data mode    
    ierr = NF90_PUT_VAR( ncid, x_id, x(:) )
    ierr = NF90_PUT_VAR( ncid, t_id, t(:) )
    ierr = NF90_PUT_VAR( ncid, sol_id, table(:,:) )

    ! close the file
    ierr = NF90_CLOSE( ncid )
  end subroutine r8mat_write

  subroutine r8vec_write( output_filename, n, x )
    implicit none

    integer(KIND=SI)             :: n

    integer(KIND=SI)             :: j
    character(len=*), intent(in) :: output_filename
    integer(KIND=SI)             :: output_unit
    real(KIND=DP), dimension(:), intent(in)    :: x

    output_unit = 11
    open( unit = output_unit, file = output_filename, status = 'replace' )

    n = size( x )
    do j = 1, n
      write ( output_unit, '(2x,g24.16)' ) x(j)
    end do

    close ( unit = output_unit )
  end subroutine r8vec_write

  subroutine r8vec_linspace( a_first, a_last, a )

    implicit none

    real(KIND=DP), intent(inout) :: a(:)
    real(KIND=DP), intent(in) :: a_first
    real(KIND=DP), intent(in) :: a_last
    integer(KIND=SI) :: i, n

    n = size( a(:) )
    
    do i = 1, n
      a(i) = ( dble( n - i ) * a_first + dble( i - 1 ) * a_last ) / dble( n - 1 )
    end do

  end subroutine r8vec_linspace  
end module IO_mod
