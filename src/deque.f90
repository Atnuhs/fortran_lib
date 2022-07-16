module deque_int32_mod
    
end module


module deque_int64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int64),parameter:: val_err=10_int64**18+1
    private
    type,public:: deque
        integer(int64),allocatable:: array(:)
        integer(int32):: l, r
    contains
        procedure:: push_back => dq_push_back
        procedure:: push_front => dq_push_front
        procedure:: pop_back => dq_pop_back
        procedure:: pop_front => dq_pop_front
        procedure:: size => dq_size
        procedure:: remain => dq_remain
    end type

    interface deque
        module procedure:: init_deque
    end interface

contains
    function init_deque() result(dq)
        type(deque):: dq

        allocate(dq%array(0:1))
        dq%l = 0
        dq%r = 1
    end function


    function dq_size(dq) result(ret)
        ! 残りの要素数
        class(deque),intent(in):: dq
        integer(int32):: ret

        ret = modulo(dq%r-dq%l-1, size(dq%array))
    end function


    function dq_remain(dq) result(ret)
        ! 要素が残っているかどうか
        class(deque),intent(in):: dq
        logical:: ret

        ret = dq%size() > 0
    end function

    subroutine expand_dq_array(dq)
        type(deque),intent(inout):: dq
        integer(int64),allocatable:: tmp(:)
        integer(int64):: i,j

        call move_alloc(dq%array,tmp)
        allocate(dq%array(0:2*size(tmp)-1))
        do i=1,size(tmp)-1
            j = mod(i+dq%l, size(tmp, kind=int64))
            dq%array(i) = tmp(j)
        end do
        dq%l = 0
        dq%r = size(tmp)
        if(allocated(tmp)) deallocate(tmp)
    end subroutine


    subroutine dq_push_back(dq, v)
        class(deque),intent(inout):: dq
        integer(int64):: v

        if (dq%l == dq%r) call expand_dq_array(dq)
        dq%array(dq%r) = v
        dq%r = modulo(dq%r+1, size(dq%array))
    end subroutine


    subroutine dq_push_front(dq, v)
        class(deque),intent(inout):: dq
        integer(int64):: v

        if (dq%l == dq%r) call expand_dq_array(dq)
        dq%array(dq%l) = v
        dq%l = modulo(dq%l-1, size(dq%array))
    end subroutine


    function dq_pop_back(dq) result(ret)
        class(deque),intent(inout):: dq
        integer(int64):: ret

        ret = val_err
        if (dq%l == dq%r-1) return ! empty
        dq%r = modulo(dq%r-1, size(dq%array))
        ret = dq%array(dq%r)
    end function


    function dq_pop_front(dq) result(ret)
        class(deque),intent(inout):: dq
        integer(int64):: ret

        ret = val_err
        if (dq%l+1 == dq%r) return ! empty
        dq%l = modulo(dq%l+1, size(dq%array))
        ret = dq%array(dq%l)
    end function
end module


module deque_real64_mod


    
end module


module deque_mod
    ! use deque_int32_mod,deque_int32 => deque
    use deque_int64_mod,deque_int64 => deque
    ! use deque_real64_mod,deque_real64 => deque
end module
