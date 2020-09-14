module deque_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: dq_size, dq_remain
    public:: dq_to_array, dq_debug_print

    type,public:: deque
        integer(int32),allocatable,private:: array(:)
        integer(int32),private:: l=1,r=0
    contains
        procedure:: append, appendleft
        procedure:: pop, popleft
        procedure:: right, left
    end type
contains
    subroutine err_no_elem()

        print'(a)', "you try pop with empty deque"
        stop
    end subroutine


    pure function dq_size(dq) result(ret)
        ! 残りの要素数
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%r-dq%l + 1
    end function
    

    function lmax(dq) result(ret)
        type(deque):: dq
        integer(int32):: ret
        
        ret = lbound(dq%array,1)
    end function


    function rmax(dq) result(ret)
        type(deque):: dq
        integer(int32):: ret

        ret = ubound(dq%array,1)
    end function


    subroutine append_right_allocation(dq)
        type(deque):: dq
        integer(int32):: l
        integer(int32),allocatable:: tmp(:)

        l = dq_size(dq)
        print'("add r :",i0," => ",i0)',rmax(dq),rmax(dq)+l
        allocate(tmp(lmax(dq):rmax(dq)+l))
        tmp(lmax(dq):rmax(dq)) = dq%array(lmax(dq):rmax(dq))
        call move_alloc(tmp, dq%array)
    end subroutine


    subroutine append_left_allocation(dq)
        type(deque):: dq
        integer(int32):: l
        integer(int32),allocatable:: tmp(:)

        l = dq_size(dq)
        print'("add l :",i0," => ",i0)',lmax(dq),lmax(dq)-l
        allocate(tmp(lmax(dq)-l:rmax(dq)))
        tmp(lmax(dq):rmax(dq)) = dq%array(lmax(dq):rmax(dq))
        call move_alloc(tmp, dq%array)
    end subroutine


    subroutine reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::new_rmax
        integer(int32),allocatable:: tmp(:)


        new_rmax = max(dq%r+dq_size(dq)/2, dq%r+1)
        print'("reduce r :",i0," => ",i0)',rmax(dq),new_rmax
        allocate(tmp(lmax(dq):new_rmax))
        tmp(lmax(dq):new_rmax) = dq%array(lmax(dq):new_rmax)
        call move_alloc(tmp, dq%array)
    end subroutine


    subroutine reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::new_lmax
        integer(int32), allocatable:: tmp(:)

        new_lmax = min(dq%l-dq_size(dq)/2, dq%l-1)
        print'("reduce l :",i0," => ",i0)',lmax(dq),new_lmax
        allocate(tmp(new_lmax:rmax(dq)))
        tmp(new_lmax:rmax(dq)) = dq%array(new_lmax:rmax(dq))
        call move_alloc(tmp, dq%array)
    end subroutine


    function extra_right(dq) result(ret)
        type(deque):: dq
        integer(int32):: ret

        ret = rmax(dq) - dq%r
    end function


    function extra_left(dq) result(ret)
        type(deque):: dq
        integer(int32):: ret

        ret = dq%l - lmax(dq)
    end function


    subroutine check_allocate_size(dq)
        type(deque):: dq

        if (dq%r > rmax(dq)) call append_right_allocation(dq)
        if (dq%l < lmax(dq)) call append_left_allocation(dq)
        if (extra_right(dq) > dq_size(dq)) call reduce_right_allocation(dq)
        if (extra_left(dq) > dq_size(dq)) call reduce_left_allocation(dq)
    end subroutine


    subroutine append(dq,v)
        ! 右端に挿入
        class(deque):: dq
        integer(int32):: v

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%r=dq%r+1
        call check_allocate_size(dq)
        dq%array(dq%r) = v
    end subroutine


    subroutine appendleft(dq,v)
        ! 左端に挿入
        class(deque):: dq
        integer(int32):: v
        
        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%l=dq%l-1
        call check_allocate_size(dq)
        dq%array(dq%l) = v
    end subroutine


    function pop(dq) result(ret)
        ! 右端から取り出し
        class(deque):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call err_no_elem()
        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call check_allocate_size(dq)
    end function


    function popleft(dq) result(ret)
        ! 左端から取り出し
        class(deque):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call err_no_elem()
        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call check_allocate_size(dq)
    end function


    function right(dq) result(ret)
        ! 右端を確認
        class(deque):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call err_no_elem()
        ret = dq%array(dq%r)
    end function


    function left(dq) result(ret)
        ! 左端を確認
        class(deque):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call err_no_elem()
        ret = dq%array(dq%l)
    end function


    function dq_remain(dq) result(ret)
        ! 要素が残っているかどうか
        type(deque):: dq
        logical:: ret

        ret = dq_size(dq) > 0
    end function


    function dq_to_array(dq) result(ret)
        type(deque):: dq
        integer(int32):: ret(1:dq_size(dq))

        ret(1:dq_size(dq)) = dq%array(dq%l:dq%r)
    end function


    subroutine dq_debug_print(dq)
        type(deque):: dq
        integer(int32):: i

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))

        print'(a)', "This is 'dq_debug_print' => deque_mod"
        print'(a)', "Output the detailed status of the deque."
        print'("lmax: ", i5,3x, "rmax: ", i5,3x, "array size: ", i5)', lmax(dq), rmax(dq), size(dq%array)
        print'("lptr: ", i5,3x, "rptr: ", i5,3x, "element num: ", i5)', dq%l, dq%r, dq_size(dq)
        print'("lext: ", i5,3x, "rext: ", i5,3x)', extra_left(dq), extra_right(dq)
        print'(a)', "===== elements ====="
        print'(*(i0,1x))', (merge(dq%array(i), 0, dq%l<=i .and. i<=dq%r), i=lmax(dq),rmax(dq))
    end subroutine
end module



program main
    use,intrinsic :: iso_fortran_env
    use:: deque_mod
    implicit none
    integer(int32):: i,n,tmp
    type(deque):: dq

    n = 10000000
    ! call dq_debug_print(dq)
    do i=1,2*n
        call dq%appendleft(i)
        ! call dq_debug_print(dq)
    end do

    do i=1,2*n
        tmp = dq%pop()
        ! call dq_debug_print(dq)
    end do
    
end program main