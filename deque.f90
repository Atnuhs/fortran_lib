module deque_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type deque
        integer(int32),pointer:: v(:)
        integer(int32):: l,r
        integer(int32):: lmax, rmax
    contains
        procedure:: append => dq_append
        procedure:: appendleft => dq_appendleft
        procedure:: pop => dq_pop
        procedure:: popleft => dq_popleft
        procedure:: right => dq_right
        procedure:: left => dq_left
        procedure:: remaining_elements => dq_remaining_elements
        procedure:: remain => dq_remain
    end type

    interface deque
        procedure:: dq_init
    end interface
contains
    function dq_init() result(dq)
        ! 初期化
        type(deque):: dq
        dq%r=0; dq%rmax=1
        dq%l=1; dq%lmax=-1
        allocate(dq%v(dq%lmax:dq%rmax))
    end function


    subroutine dq_append(dq,num)
        ! 右端に挿入
        class(deque):: dq
        integer(int32):: num
        if (dq%r+1 > dq%rmax) call dq_add_(dq)
        dq%r=dq%r+1
        dq%v(dq%r) = num
    end subroutine

    subroutine dq_appendleft(dq,num)
        ! 左端に挿入
        class(deque):: dq
        integer(int32):: num
        if (dq%l-1 < dq%lmax) call dq_add_(dq)
        dq%l=dq%l-1
        dq%v(dq%l) = num
    end subroutine

    subroutine dq_add_(dq)
        ! arrayの延長
        class(deque):: dq
        integer(int32):: l
        integer(int32),pointer:: tmp(:)
        l = size(dq%v)
        allocate(tmp(l))
        tmp(:) = dq%v(:)
        deallocate(dq%v)
        allocate(dq%v(2*dq%lmax:2*dq%rmax))
        dq%v(dq%lmax:dq%rmax) = tmp(:)
        dq%lmax = 2*dq%lmax
        dq%rmax = 2*dq%rmax
    end subroutine


    function dq_pop(dq) result(ret)
        ! 右端から取り出し
        class(deque):: dq
        integer(int32):: ret
        ret = dq%v(dq%r)
        dq%r=dq%r-1
    end function

    function dq_popleft(dq) result(ret)
        ! 左端から取り出し
        class(deque):: dq
        integer(int32):: ret
        ret = dq%v(dq%l)
        dq%l=dq%l+1
    end function


    function dq_right(dq) result(ret)
        ! 右端を確認
        class(deque):: dq
        integer(int32):: ret
        ret = dq%v(dq%r)
    end function

    function dq_left(dq) result(ret)
        ! 左端を確認
        class(deque):: dq
        integer(int32):: ret
        ret = dq%v(dq%l)
    end function

    function dq_remaining_elements(dq) result(ret)
        ! 残りの要素数
        class(deque):: dq
        integer(int32):: ret 
        ret = dq%r - dq%l + 1
    end function

    function dq_remain(dq) result(ret)
        ! 要素が残っているかどうか
        class(deque):: dq
        logical:: ret
        ret = dq%remaining_elements() > 0
    end function
end module