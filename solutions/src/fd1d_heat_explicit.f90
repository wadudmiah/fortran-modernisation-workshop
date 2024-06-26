!> Wadud Miah, **Numerical Algorithms Group**
!> Solves the one dimensional heat diffusion equation:
!> \( \frac{\partial H}{\partial t} 
!>     - \kappa\frac{\partial^{2} H}{\partial x^{2}} = f(x) \)

program fd1d_heat_explicit
      use Types_mod
      use RHS_mod
      use CFL_mod
      use IO_mod
      use Solver_mod
      use plplot
      
      implicit none

      integer(kind=SI), parameter :: T_NUM = 201
      integer(kind=SI), parameter :: X_NUM = 21 
      
      real(kind=DP) :: cfl
      real(kind=DP) :: dt
      real(kind=DP), dimension(:), allocatable :: h
      real(kind=DP), dimension(:), allocatable :: h_new
      ! the "matrix" stores all x-values for all t-values
      ! remember Fortran is column major, meaning that rows are contiguous
      real(kind=DP), dimension(:,:), allocatable :: hmat
      integer(kind=SI) :: i, j, ierr
      character(len=10) :: vis_filename_num
      character(len=30) :: vis_filename
      character(len=60) :: errbuf
      real(kind=DP) :: k

      real(kind=DP), dimension(:), allocatable :: t
      real(kind=DP) :: t_max
      real(kind=DP) :: t_min
      real(kind=DP), dimension(:), allocatable :: x
      real(kind=DP) :: x_max
      real(kind=DP) :: x_min

      allocate( h(1:X_NUM), stat = ierr, errmsg = errbuf )
      allocate( h_new(1:X_NUM), stat = ierr, errmsg = errbuf )
      allocate( hmat(1:X_NUM,1:T_NUM), stat = ierr, errmsg = errbuf )
      allocate( t(1:T_NUM), stat = ierr, errmsg = errbuf )
      allocate( x(1:X_NUM), stat = ierr, errmsg = errbuf )

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
      call r8vec_linspace( x_min, x_max, x )

      ! the t-range values. integrate from t_min to t_max
      t_min = 0.0_DP
      t_max = 80.0_DP

      ! t_num is the number of intervals in the t-direction
      dt = ( t_max - t_min ) / dble( t_num - 1 )
      call r8vec_linspace( t_min, t_max, t )

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
      time_loop: do j = 2, t_num
        call fd1d_heat_explicit_solver( x, t(j-1), dt, cfl, h, h_new )

        ! visualise solution at every 10 time steps
        if ( mod( j, 10 ) == 0 ) then
          write ( vis_filename_num, '(I5.5)' ) j
          vis_filename = 'fd1d_heat_explicit_' // trim( vis_filename_num ) &
                                               // '.png'

          call PLSFNAM( vis_filename )
          call PLSDEV( "pngcairo" )
          call PLINIT( )
          call PLENV( minval( x(:) ), maxval( x(:) ), &
                      minval( h_new(:) ),  maxval( h_new(:) ), 0, 0 )
          call PLLAB( 'x (m)', 'temp (C)', &
                      'heat diffusion at n = ' // vis_filename_num )
          call PLLINE( x(:), h_new(:) )
          call PLEND( )
        end if 
        
        do i = 1, x_num
          hmat(i, j) = h_new(i)
          h(i) = h_new(i)
        end do
      end do time_loop

      ! write data to files
      call r8mat_write( 'h_test01.nc', hmat, x, t )

      deallocate( h, stat = ierr )
      deallocate( h_new, stat = ierr )
      deallocate( hmat, stat = ierr )
      deallocate( t, stat = ierr )
      deallocate( x, stat = ierr )
end program fd1d_heat_explicit

