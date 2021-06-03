module array_real32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    pure subroutine vec_append_array(ar)
        ! append array size to double
        real(real64), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        real(real64),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:2*r))
        tmp(l:r) = ar(l:r)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine vec_reduce_array(ar)
        ! reduce array size to half
        real(real64), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        real(real64),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:r/2))
        tmp(l:r/2) = ar(l:r/2)
        call move_alloc(tmp, ar)
    end subroutine
end module