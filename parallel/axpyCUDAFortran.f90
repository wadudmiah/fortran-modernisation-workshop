attributes(global) subroutine axpy(n, a, X, Y, Z)
use iso_fortran_env, only: real64
implicit none
integer, value :: n
real(real64), value :: a
real(real64) :: X(*), Y(*), Z(*)
integer :: i

  i = threadIdx%x + (blockIdx%x - 1) * blockDim%x
  if (i <= n)  Z(i) = a * X(i) + Y(i)

end subroutine axpy

program axpyProg
use iso_fortran_env, only: real64
use cudafor
implicit none
real(real64) :: a
real(real64), allocatable :: X(:), Y(:), Z(:)
real(real64), allocatable, device :: X_D(:), Y_D(:), Z_D(:)
integer :: n
type(dim3) :: block, grid
integer :: err

  n = 2**10

  allocate(X(n))
  allocate(Y(n))
  allocate(Z(n))

  call random_seed
  call random_number(a)
  call random_number(X)
  call random_number(Y)

  allocate(X_D(n))
  allocate(Y_D(n))
  allocate(Z_D(n))

  err = cudaMemCpy(X_D, X, n, cudaMemCpyHostToDevice)
  err = cudaMemCpy(Y_D, Y, n)

  block = dim3(128, 1, 1)
  grid = dim3(n / block%x, 1, 1)

  call axpy<<<grid, block>>>(%val(n), %val(a), X_D, Y_D, Z_D)

  deallocate(X_D)
  deallocate(Y_D)

  Z = Z_D

  deallocate(Z_D)

  print *, maxval(abs(Z)), maxval(abs(Z / (a * X + Y) - 1.0_real64))

  deallocate(X)
  deallocate(Y)
  deallocate(Z)

end program axpyProg
