      program main

c*********************************************************************72
c
cc MAIN is the main program for FD1D_HEAT_EXPLICIT_PRB.
c
c  Discussion:
c
c    FD1D_HEAT_EXPLICIT_PRB tests the FD1D_HEAT_EXPLICIT library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 January 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none
      integer t_num
      parameter ( t_num = 201 )
      integer x_num
      parameter ( x_num = 21 )
      
      double precision cfl
      double precision dt
      double precision h(x_num)
      double precision h_new(x_num)
      double precision hmat(x_num,t_num)
      integer i
      integer j
      double precision k

      double precision t(t_num)
      double precision t_max
      double precision t_min
      double precision x(x_num)
      double precision x_max
      double precision x_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the FD1D_HEAT_EXPLICIT library.'


c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '


c*********************************************************************72
c
cc FD1D_HEAT_EXPLICIT_TEST01 does a simple test problem
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 January 2012
c
c  Author:
c
c    John Burkardt
c

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FD1D_HEAT_EXPLICIT_TEST01:'
      write ( *, '(a)' ) 
     &  '  Compute an approximate solution to the time-dependent'
      write ( *, '(a)' ) '  one dimensional heat equation:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    dH/dt - K * d2H/dx2 = f(x,t)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Run a simple test case.'
c
c  Heat coefficient.
c
      k = 0.002D+00
c
c  X_NUM is the number of equally spaced nodes to use between 0 and 1.
c
      x_min = 0.0D+00
      x_max = 1.0D+00
      call r8vec_linspace ( x_num, x_min, x_max, x )
c
c  T_NUM is the number of equally spaced time points between 0 and 10.0.
c
      t_min = 0.0D+00
      t_max = 80.0D+00
      dt = ( t_max - t_min ) / dble ( t_num - 1 )
      call r8vec_linspace ( t_num, t_min, t_max, t )
c
c  Get the CFL coefficient.
c
      call fd1d_heat_explicit_cfl ( k, t_num, t_min, t_max, x_num, 
     &  x_min, x_max, cfl )
c
c  Running the code produces an array H of temperatures H(t,x),
c  and vectors x and t.
c
c     call ic_test01 ( x_num, x, t(1), h )
c     set initial condition
      do j = 1, x_num
        h(j) = 50.0D+00
      end do

c     set boundary condition
      h(1) = 90.0D+00
      h(x_num) = 70.0D+00
c      call bc_test01 ( x_num, x, t(1), h )

      j = 1
      do i = 1, x_num
        hmat(i,j) = h(i)
      end do

      do j = 2, t_num
        call fd1d_heat_explicit ( x_num, x, t(j-1), dt, cfl, 
     &    h, h_new )
        do i = 1, x_num
          hmat(i,j) = h_new(i)
          h(i) = h_new(i)
        end do
      end do
c
c  Write the data to files.
c
      call r8mat_write ( 'h_test01.txt', x_num, t_num, hmat )
      call r8vec_write ( 't_test01.txt', t_num, t )
      call r8vec_write ( 'x_test01.txt', x_num, x )


      end 

