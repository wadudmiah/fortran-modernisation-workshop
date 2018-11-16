subroutine axpy(n, a, X, Y, Z)
use iso_fortran_env, only: real64
implicit none
integer :: n
real(real64) :: a
real(real64) :: X(*), Y(*), Z(*)
integer :: i

  do i = 1, n
    Z(i) = a * X(i) + Y(i)
  end do

end subroutine axpy

program axpyProg
use iso_fortran_env, only: real64
implicit none
real(real64) :: a, a_I[*]
real(real64), allocatable :: X(:), Y(:), Z(:)
real(real64), allocatable :: X_I(:)[:], Y_I(:)[:], Z_I(:)[:]
integer :: n, n_I
integer :: i

  n = 2**10

  if (this_image() == 1) then
    allocate(X(n))
    allocate(Y(n))
    allocate(Z(n))

    call random_seed
    call random_number(a)
    call random_number(X)
    call random_number(Y)
  end if

  n_I = n / num_images()

  allocate(X_I(n_I)[*])
  allocate(Y_I(n_I)[*])
  allocate(Z_I(n_I)[*])

  if (this_image() == 1) then
    do i = 1, num_images()
      a_I[i] = a
      X_I(:)[i] = X((i-1)*n_I+1:i*n_I)
      Y_I(:)[i] = Y((i-1)*n_I+1:i*n_I)
    end do
  end if

  sync all

  call axpy(n_I, a_I, X_I, Y_I, Z_I)

  deallocate(X_I)
  deallocate(Y_I)

  if (this_image() == 1) then
    do i = 1, num_images()
      Z((i-1)*n_I+1:i*n_I) = Z_I(:)[i]
    end do

    print *, maxval(abs(Z)), maxval(abs(Z / (a * X + Y) - 1.0_real64))

    deallocate(X)
    deallocate(Y)
    deallocate(Z)
  end if

  sync all

  deallocate(Z_I)

end program axpyProg
