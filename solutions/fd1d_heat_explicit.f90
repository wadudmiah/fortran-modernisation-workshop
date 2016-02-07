!> @author
!> Wadud Miah, Numerical Algorithms Group
!> @brief 
!> Solves the one dimensional heat diffusion equation
!> \f$ \frac{\partial{\bf H}}{\partial t} 
!>     - K\frac{\partial^{2}{\bf H}}{\partial x^{2}} = f(x) \f$

program fd1d_heat_explicit
      use Types_mod
      use RHS_mod
      use CFL_mod
      use IO_mod
      use Solver_mod
      
      implicit none

      integer(KIND=SI), parameter :: t_num = 201
      integer(KIND=SI), parameter :: x_num = 21 
      
      real(KIND=DP) :: cfl
      real(KIND=DP) :: dt
      real(KIND=DP) :: h(x_num)
      real(KIND=DP) :: h_new(x_num)
      ! the "matrix" stores all x-values for all t-values
      ! remember Fortran is column major, meaning that rows are contiguous
      real(KIND=DP) :: hmat(x_num, t_num)
      integer(KIND=SI) :: i
      integer(KIND=SI) :: j
      real(KIND=DP) :: k

      real(KIND=DP) :: t(t_num)
      real(KIND=DP) :: t_max
      real(KIND=DP) :: t_min
      real(KIND=DP) :: x(x_num)
      real(KIND=DP) :: x_max
      real(KIND=DP) :: x_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  FORTRAN90 version.'
      write ( *, '(a)' ) '  Test the FD1D_HEAT_EXPLICIT library.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_TEST01:'
      write ( *, '(a)' ) '  Compute an approximate solution to the time-dependent'
      write ( *, '(a)' ) '  one dimensional heat equation:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    dH/dt - K * d2H/dx2 = f(x,t)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Run a simple test case.'

      ! heat coefficient
      k = 0.002_DP

      ! the x-range values
      x_min = 0.0_DP
      x_max = 1.0_DP
      ! x_num is the number of intervals in the x-direction
      call r8vec_linspace( x_num, x_min, x_max, x )

      ! the t-range values. integrate from t_min to t_max
      t_min = 0.0_DP
      t_max = 80.0_DP

      ! t_num is the number of intervals in the t-direction
      dt = ( t_max - t_min ) / dble( t_num - 1 )
      call r8vec_linspace( t_num, t_min, t_max, t )

      ! get the CFL coefficient
      call fd1d_heat_explicit_cfl( k, t_num, t_min, t_max, x_num, x_min, x_max, cfl )

      if ( 0.5_DP <= cfl ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_CFL - Fatal error!'
        write ( *, '(a)' ) '  CFL condition failed.'
        write ( *, '(a)' ) '  0.5 <= K * dT / dX / dX = CFL.'
        stop
      end if
  
      ! set the initial condition
      do j = 1, x_num
        h(j) = 50.0_DP
      end do

      ! set the bounday condition
      h(1) = 90.0_DP
      h(x_num) = 70.0_DP

      ! initialise the matrix to the initial condition
      do i = 1, x_num
        hmat(i, 1) = h(i)
      end do

      ! the main time integration loop 
      do j = 2, t_num
        call fd1d_heat_explicit_solver( x_num, x, t(j-1), dt, cfl, h, h_new )

        do i = 1, x_num
          hmat(i, j) = h_new(i)
          h(i) = h_new(i)
        end do
      end do

      ! write data to files
      call r8mat_write( 'h_test01.nc', x_num, t_num, hmat, x, t )
end program fd1d_heat_explicit

