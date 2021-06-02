module deque_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: dq_debug_print

    type,public:: deque
        integer(int32),allocatable,private:: array(:)
        integer(int32),private:: l=1,r=0
    contains
        procedure:: push_back => dq_push_back, push_front => dq_push_front
        procedure:: pop_back => dq_pop_back, pop_front => dq_pop_front
        procedure:: vr => dq_right, vl => dq_left
        procedure:: at => dq_at
        procedure:: ir => dq_ir, il => dq_il
        procedure:: size => dq_size, remain => dq_remain
        procedure:: to_array => dq_to_array
        procedure:: update => dq_update
    end type
contains
    subroutine dq_err_no_elem()

        print'(a)', "you try dq_pop_back with empty deque"
        stop
    end subroutine


    pure function dq_ir(dq) result(ret)
        class(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%r
    end function


    pure function dq_il(dq) result(ret)
        class(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%l
    end function

    subroutine dq_update(dq,i,x)
        class(deque),intent(inout):: dq
        integer(int32),intent(in):: i,x

        if (.not.allocated(dq%array)) call dq_err_no_elem
        dq%array(i) = x
    end subroutine

    pure function dq_size(dq) result(ret)
        ! 残りの要素数
        class(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%r-dq%l + 1
    end function


    pure function dq_remain(dq) result(ret)
        ! 要素が残っているかどうか
        class(deque),intent(in):: dq
        logical:: ret

        ret = dq_size(dq) > 0
    end function


    pure subroutine dq_append_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr
        integer(int32),allocatable:: tmp(:)

        ! print'("add r :",i0," => ",i0)',dq_rmax(dq),dq_rmax(dq)+l
        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        allocate(tmp(ll:rr+l))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_append_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr
        integer(int32),allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        ! print'("add l :",i0," => ",i0)',dq_lmax(dq),dq_lmax(dq)-l
        allocate(tmp(ll-l:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::rr,l,ll
        integer(int32),allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = max(dq%r+l/2, dq%r+1)
        ! print'("reduce r :",i0," => ",i0)',dq_rmax(dq),rr
        allocate(tmp(ll:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::l,ll,rr
        integer(int32), allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = min(dq%l-l/2, dq%l-1)
        rr = ubound(dq%array,1)
        allocate(tmp(ll:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure function dq_extra_right(dq) result(ret)
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = ubound(dq%array,1) - dq%r
    end function


    pure function dq_extra_left(dq) result(ret)
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%l - lbound(dq%array,1)
    end function


    pure subroutine dq_check_allocate_size(dq)
        type(deque),intent(inout):: dq

        if (dq%r > ubound(dq%array,1)) call dq_append_right_allocation(dq)
        if (dq%l < lbound(dq%array,1)) call dq_append_left_allocation(dq)
        if (dq_extra_right(dq) > dq_size(dq)) call dq_reduce_right_allocation(dq)
        if (dq_extra_left(dq) > dq_size(dq)) call dq_reduce_left_allocation(dq)
    end subroutine


    pure subroutine dq_push_back(dq,v)
        ! 右端に挿入
        class(deque),intent(inout):: dq
        integer(int32),intent(in):: v

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%r=dq%r+1
        call dq_check_allocate_size(dq)
        dq%array(dq%r) = v
    end subroutine


    pure subroutine dq_push_front(dq,v)
        ! 左端に挿入
        class(deque),intent(inout):: dq
        integer(int32),intent(in):: v
        
        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%l=dq%l-1
        call dq_check_allocate_size(dq)
        dq%array(dq%l) = v
    end subroutine


    function dq_pop_back(dq) result(ret)
        ! 右端から取り出し
        class(deque),intent(inout):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call dq_check_allocate_size(dq)
    end function


    function dq_pop_front(dq) result(ret)
        ! 左端から取り出し
        class(deque),intent(inout):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call dq_check_allocate_size(dq)
    end function


    function dq_right(dq) result(ret)
        ! 右端を確認
        class(deque),intent(in):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%r)
    end function


    function dq_left(dq) result(ret)
        ! 左端を確認
        class(deque),intent(in):: dq
        integer(int32):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%l)
    end function


    function dq_at(dq,i) result(ret)
        class(deque),intent(in):: dq
        integer(int32),intent(in):: i
        integer(int32):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(i)
    end function


    pure function dq_to_array(dq) result(ret)
        class(deque),intent(in):: dq
        integer(int32):: ret(1:dq_size(dq))

        ret(1:dq_size(dq)) = dq%array(dq%l:dq%r)
    end function


    subroutine dq_debug_print(dq)
        type(deque),intent(inout):: dq
        integer(int32):: i,ll,rr

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        print'(a)', "This is 'dq_debug_print' => deque_mod"
        print'(a)', "Output the detailed status of the deque."
        print'("dq_lmax: ", i5,3x, "dq_rmax: ", i5,3x, "array size: ", i5)', ll, rr, size(dq%array)
        print'("lptr: ", i5,3x, "rptr: ", i5,3x, "element num: ", i5)', dq%l, dq%r, dq_size(dq)
        print'("lext: ", i5,3x, "rext: ", i5,3x)', dq_extra_left(dq), dq_extra_right(dq)
        print'(a)', "===== elements ====="
        print'(*(i0,1x))', (merge(dq%array(i), 0, dq%l<=i .and. i<=dq%r), i=ll, rr)
    end subroutine
end module


module deque_int64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: dq_debug_print

    type,public:: deque
        integer(int64),allocatable,private:: array(:)
        integer(int32),private:: l=1, r=0
    contains
        procedure:: push_back => dq_push_back, push_front => dq_push_front
        procedure:: pop_back => dq_pop_back, pop_front => dq_pop_front
        procedure:: vr => dq_right, vl => dq_left
        procedure:: at => dq_at
        procedure:: ir => dq_ir, il => dq_il
        procedure:: size => dq_size, remain => dq_remain
        procedure:: to_array => dq_to_array
        procedure:: update => dq_update
    end type
contains
    subroutine dq_err_no_elem()

        print'(a)', "you try dq_pop_back with empty deque"
        stop
    end subroutine


    pure function dq_ir(dq) result(ret)
        class(deque),intent(in):: dq
        integer(int64):: ret

        ret = dq%r
    end function


    pure function dq_il(dq) result(ret)
        class(deque),intent(in):: dq
        integer(int64):: ret

        ret = dq%l
    end function

    subroutine dq_update(dq,i,x)
        class(deque),intent(inout):: dq
        integer(int64),intent(in):: i,x

        if (.not.allocated(dq%array)) call dq_err_no_elem
        dq%array(i) = x
    end subroutine

    pure function dq_size(dq) result(ret)
        ! 残りの要素数
        class(deque),intent(in):: dq
        integer(int64):: ret

        ret = dq%r-dq%l + 1
    end function


    pure function dq_remain(dq) result(ret)
        ! 要素が残っているかどうか
        class(deque),intent(in):: dq
        logical:: ret

        ret = dq_size(dq) > 0
    end function


    pure subroutine dq_append_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr
        integer(int64),allocatable:: tmp(:)

        ! print'("add r :",i0," => ",i0)',dq_rmax(dq),dq_rmax(dq)+l
        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        allocate(tmp(ll:rr+l))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_append_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr
        integer(int64),allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        ! print'("add l :",i0," => ",i0)',dq_lmax(dq),dq_lmax(dq)-l
        allocate(tmp(ll-l:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::rr,l,ll
        integer(int64),allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = max(dq%r+l/2, dq%r+1)
        ! print'("reduce r :",i0," => ",i0)',dq_rmax(dq),rr
        allocate(tmp(ll:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::l,ll,rr
        integer(int64), allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = min(dq%l-l/2, dq%l-1)
        rr = ubound(dq%array,1)

        allocate(tmp(ll:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure function dq_extra_right(dq) result(ret)
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = ubound(dq%array,1) - dq%r
    end function


    pure function dq_extra_left(dq) result(ret)
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%l - lbound(dq%array,1)
    end function


    pure subroutine dq_check_allocate_size(dq)
        type(deque),intent(inout):: dq

        if (dq%r > ubound(dq%array,1)) call dq_append_right_allocation(dq)
        if (dq%l < lbound(dq%array,1)) call dq_append_left_allocation(dq)
        if (dq_extra_right(dq) > dq_size(dq)) call dq_reduce_right_allocation(dq)
        if (dq_extra_left(dq) > dq_size(dq)) call dq_reduce_left_allocation(dq)
    end subroutine


    pure subroutine dq_push_back(dq,v)
        ! 右端に挿入
        class(deque),intent(inout):: dq
        integer(int64),intent(in):: v

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%r=dq%r+1
        call dq_check_allocate_size(dq)
        dq%array(dq%r) = v
    end subroutine


    pure subroutine dq_push_front(dq,v)
        ! 左端に挿入
        class(deque),intent(inout):: dq
        integer(int64),intent(in):: v
        
        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%l=dq%l-1
        call dq_check_allocate_size(dq)
        dq%array(dq%l) = v
    end subroutine


    function dq_pop_back(dq) result(ret)
        ! 右端から取り出し
        class(deque),intent(inout):: dq
        integer(int64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call dq_check_allocate_size(dq)
    end function


    function dq_pop_front(dq) result(ret)
        ! 左端から取り出し
        class(deque),intent(inout):: dq
        integer(int64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call dq_check_allocate_size(dq)
    end function


    function dq_right(dq) result(ret)
        ! 右端を確認
        class(deque),intent(in):: dq
        integer(int64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%r)
    end function


    function dq_left(dq) result(ret)
        ! 左端を確認
        class(deque),intent(in):: dq
        integer(int64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%l)
    end function


    function dq_at(dq,i) result(ret)
        class(deque),intent(in):: dq
        integer(int64),intent(in):: i
        integer(int64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(i)
    end function


    pure function dq_to_array(dq) result(ret)
        class(deque),intent(in):: dq
        integer(int64):: ret(1:dq_size(dq))

        ret(1:dq_size(dq)) = dq%array(dq%l:dq%r)
    end function


    subroutine dq_debug_print(dq)
        type(deque),intent(inout):: dq
        integer(int32):: i,ll,rr

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        print'(a)', "This is 'dq_debug_print' => deque_mod"
        print'(a)', "Output the detailed status of the deque."
        print'("dq_lmax: ", i5,3x, "dq_rmax: ", i5,3x, "array size: ", i5)', ll, rr, size(dq%array)
        print'("lptr: ", i5,3x, "rptr: ", i5,3x, "element num: ", i5)', dq%l, dq%r, dq_size(dq)
        print'("lext: ", i5,3x, "rext: ", i5,3x)', dq_extra_left(dq), dq_extra_right(dq)
        print'(a)', "===== elements ====="
        print'(*(i0,1x))', (merge(dq%array(i), 0_int64, dq%l<=i .and. i<=dq%r), i=ll, rr)
    end subroutine
end module


module deque_real64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: dq_debug_print

    type,public:: deque
        real(real64),allocatable,private:: array(:)
        integer(int32),private:: l=1, r=0
    contains
        procedure:: push_back => dq_push_back, push_front => dq_push_front
        procedure:: pop_back => dq_pop_back, pop_front => dq_pop_front
        procedure:: vr => dq_right, vl => dq_left
        procedure:: at => dq_at
        procedure:: ir => dq_ir, il => dq_il
        procedure:: size => dq_size, remain => dq_remain
        procedure:: to_array => dq_to_array
        procedure:: update => dq_update
    end type
contains
    subroutine dq_err_no_elem()

        print'(a)', "you try dq_pop_back with empty deque"
        stop
    end subroutine


    pure function dq_ir(dq) result(ret)
        class(deque),intent(in):: dq
        real(real64):: ret

        ret = dq%r
    end function


    pure function dq_il(dq) result(ret)
        class(deque),intent(in):: dq
        real(real64):: ret

        ret = dq%l
    end function

    subroutine dq_update(dq,i,x)
        class(deque),intent(inout):: dq
        integer(int32),intent(in):: i
        real(real64),intent(in):: x

        if (.not.allocated(dq%array)) call dq_err_no_elem
        dq%array(i) = x
    end subroutine

    pure function dq_size(dq) result(ret)
        ! 残りの要素数
        class(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%r-dq%l + 1
    end function


    pure function dq_remain(dq) result(ret)
        ! 要素が残っているかどうか
        class(deque),intent(in):: dq
        logical:: ret

        ret = dq_size(dq) > 0
    end function


    pure subroutine dq_append_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr
        real(real64),allocatable:: tmp(:)

        ! print'("add r :",i0," => ",i0)',dq_rmax(dq),dq_rmax(dq)+l
        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        allocate(tmp(ll:rr+l))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_append_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr
        real(real64),allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        ! print'("add l :",i0," => ",i0)',dq_lmax(dq),dq_lmax(dq)-l
        allocate(tmp(ll-l:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::rr,l,ll
        real(real64),allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = lbound(dq%array,1)
        rr = max(dq%r+l/2, dq%r+1)
        ! print'("reduce r :",i0," => ",i0)',dq_rmax(dq),rr
        allocate(tmp(ll:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure subroutine dq_reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::l,ll,rr
        real(real64), allocatable:: tmp(:)

        l = dq%r-dq%l + 1
        ll = min(dq%l-l/2, dq%l-1)
        rr = ubound(dq%array,1)

        allocate(tmp(ll:rr))
        tmp(ll:rr) = dq%array(ll:rr)
        call move_alloc(tmp, dq%array)
    end subroutine


    pure function dq_extra_right(dq) result(ret)
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = ubound(dq%array,1) - dq%r
    end function


    pure function dq_extra_left(dq) result(ret)
        type(deque),intent(in):: dq
        integer(int32):: ret

        ret = dq%l - lbound(dq%array,1)
    end function


    pure subroutine dq_check_allocate_size(dq)
        type(deque),intent(inout):: dq

        if (dq%r > ubound(dq%array,1)) call dq_append_right_allocation(dq)
        if (dq%l < lbound(dq%array,1)) call dq_append_left_allocation(dq)
        if (dq_extra_right(dq) > dq_size(dq)) call dq_reduce_right_allocation(dq)
        if (dq_extra_left(dq) > dq_size(dq)) call dq_reduce_left_allocation(dq)
    end subroutine


    pure subroutine dq_push_back(dq,v)
        ! 右端に挿入
        class(deque),intent(inout):: dq
        real(real64),intent(in):: v

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%r=dq%r+1
        call dq_check_allocate_size(dq)
        dq%array(dq%r) = v
    end subroutine


    pure subroutine dq_push_front(dq,v)
        ! 左端に挿入
        class(deque),intent(inout):: dq
        real(real64),intent(in):: v
        
        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        dq%l=dq%l-1
        call dq_check_allocate_size(dq)
        dq%array(dq%l) = v
    end subroutine


    function dq_pop_back(dq) result(ret)
        ! 右端から取り出し
        class(deque),intent(inout):: dq
        real(real64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call dq_check_allocate_size(dq)
    end function


    function dq_pop_front(dq) result(ret)
        ! 左端から取り出し
        class(deque),intent(inout):: dq
        real(real64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call dq_check_allocate_size(dq)
    end function


    function dq_right(dq) result(ret)
        ! 右端を確認
        class(deque),intent(in):: dq
        real(real64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%r)
    end function


    function dq_left(dq) result(ret)
        ! 左端を確認
        class(deque),intent(in):: dq
        real(real64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(dq%l)
    end function


    function dq_at(dq,i) result(ret)
        class(deque),intent(in):: dq
        integer(int32),intent(in):: i
        real(real64):: ret

        if (.not. dq_remain(dq)) call dq_err_no_elem()
        ret = dq%array(i)
    end function


    pure function dq_to_array(dq) result(ret)
        class(deque),intent(in):: dq
        real(real64):: ret(1:dq_size(dq))

        ret(1:dq_size(dq)) = dq%array(dq%l:dq%r)
    end function


    subroutine dq_debug_print(dq)
        type(deque),intent(inout):: dq
        integer(int32):: i,ll,rr

        if (.not. allocated(dq%array)) allocate(dq%array(0:1))
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)
        print'(a)', "This is 'dq_debug_print' => deque_mod"
        print'(a)', "Output the detailed status of the deque."
        print'("dq_lmax: ", i5,3x, "dq_rmax: ", i5,3x, "array size: ", i5)', ll, rr, size(dq%array)
        print'("lptr: ", i5,3x, "rptr: ", i5,3x, "element num: ", i5)', dq%l, dq%r, dq_size(dq)
        print'("lext: ", i5,3x, "rext: ", i5,3x)', dq_extra_left(dq), dq_extra_right(dq)
        print'(a)', "===== elements ====="
        print'(*(i0,1x))', (merge(dq%array(i), 0d0, dq%l<=i .and. i<=dq%r), i=ll, rr)
    end subroutine
end module


module deque_mod
    use deque_int32_mod,deque_int32 => deque
    use deque_int64_mod,deque_int64 => deque
    use deque_real64_mod,deque_real64 => deque
end module
