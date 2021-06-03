program main
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32):: ar(4:10)

    call f(ar)
contains
    subroutine f(ar)
        integer(int32):: ar(:)

        print*, lbound(ar,1), ubound(ar,1)
    end subroutine
end program main