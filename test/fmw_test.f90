! Fortran Modernisation Workshop test code. This is to check
! the correct compiler, NetCDF and PLplot dependencies
! Wadud Miah, Numerical Algorithms Group
! wadud.miah@nag.co.uk

program fmw_test
  use, intrinsic :: iso_fortran_env
  use plplot
  use netcdf
  
  implicit none

  integer, parameter :: N = 360
  real(kind=REAL64), dimension(1:N) :: x
  real(kind=REAL64), dimension(1:N) :: t
  integer :: i, ierr, x_dimid, t_dimid, x_id, t_id, ncid
  intrinsic :: sin
  
  do i = 1, N
    x(i) = real( i, kind = REAL64 ) 
    t(i) = sin( x(i) * 3.14159_REAL64 / real( N, REAL64 ))
  end do

  ierr = NF90_CREATE( 'fmw_test.nc', NF90_CLOBBER, ncid )

  ierr = NF90_DEF_DIM( ncid, "x", N, x_dimid )
  ierr = NF90_DEF_DIM( ncid, "t", N, t_dimid ) 

  ierr = NF90_DEF_VAR( ncid, "x-range", NF90_DOUBLE, x_dimid, x_id )
  ierr = NF90_DEF_VAR( ncid, "t-range", NF90_DOUBLE, t_dimid, t_id )

  ierr = NF90_ENDDEF( ncid )

  ierr = NF90_PUT_VAR( ncid, x_id, x(:) )
  ierr = NF90_PUT_VAR( ncid, t_id, t(:) )
  ierr = NF90_CLOSE( ncid )

  call PLSFNAM( 'fmw_test.png' )
  call PLSDEV( 'pngcairo' )
  call PLINIT( )

  call PLENV( 1.0_REAL64, real( N, REAL64), 0.0_REAL64, 1.0_REAL64, 0, 0 )
  call PLLAB( 'x', 'sin(x)', 'sin function' )
  call PLLINE( x(:), t(:) )
  call PLEND( )
end program fmw_test

