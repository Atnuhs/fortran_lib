module deque_int32_mod
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none
    private
    type,public:: deque
        integer(int32),allocatable:: array(:)
        integer(int32):: l=1, r=0
    contains
        procedure:: push_back => dq_push_back
        procedure:: push_front => dq_push_front
        procedure:: pop_back => dq_pop_back
        procedure:: pop_front => dq_pop_front
        procedure:: size => dq_size
        procedure:: remain => dq_remain
    end type
contains
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

        ret = dq%size() > 0
    end function


    pure subroutine dq_append_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr,nl,nr,dl,dr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size() 

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = rr

        ! 新しい配列の両端
        nl = ll
        nr = rr+l ! 配列の右側をデータ長分拡張する。

        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_append_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr,nl,nr,dl,dr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = rr

        ! 新しい配列の両端
        nl = ll-l
        nr = rr
        
        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::rr,l,ll,dl,dr,nl,nr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = max(dq%r+l/2, dq%r+1)

        ! 新しい配列の両端
        nl = dl
        nr = dr
        
        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::l,ll,rr,dl,dr,nl,nr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = min(dq%l-l/2, dq%r+1)
        dr = rr

        ! 新しい配列の両端
        nl = dl
        nr = dr
        
        call resize_array(dq%array,nl,nr,dl,dr)
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
        if (dq_extra_right(dq) > dq%size()) call dq_reduce_right_allocation(dq)
        if (dq_extra_left(dq) > dq%size()) call dq_reduce_left_allocation(dq)
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

        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call dq_check_allocate_size(dq)
    end function


    function dq_pop_front(dq) result(ret)
        ! 左端から取り出し
        class(deque),intent(inout):: dq
        integer(int32):: ret

        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call dq_check_allocate_size(dq)
    end function
end module


module deque_int64_mod
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none
    private
    type,public:: deque
        integer(int64),allocatable:: array(:)
        integer(int32):: l=1, r=0
    contains
        procedure:: push_back => dq_push_back
        procedure:: push_front => dq_push_front
        procedure:: pop_back => dq_pop_back
        procedure:: pop_front => dq_pop_front
        procedure:: size => dq_size
        procedure:: remain => dq_remain
    end type
contains
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

        ret = dq%size() > 0
    end function


    pure subroutine dq_append_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr,nl,nr,dl,dr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size() 

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = rr

        ! 新しい配列の両端
        nl = ll
        nr = rr+l ! 配列の右側をデータ長分拡張する。

        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_append_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr,nl,nr,dl,dr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = rr

        ! 新しい配列の両端
        nl = ll-l
        nr = rr
        
        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::rr,l,ll,dl,dr,nl,nr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = max(dq%r+l/2, dq%r+1)

        ! 新しい配列の両端
        nl = dl
        nr = dr
        
        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::l,ll,rr,dl,dr,nl,nr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = min(dq%l-l/2, dq%r+1)
        dr = rr

        ! 新しい配列の両端
        nl = dl
        nr = dr
        
        call resize_array(dq%array,nl,nr,dl,dr)
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
        if (dq_extra_right(dq) > dq%size()) call dq_reduce_right_allocation(dq)
        if (dq_extra_left(dq) > dq%size()) call dq_reduce_left_allocation(dq)
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

        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call dq_check_allocate_size(dq)
    end function


    function dq_pop_front(dq) result(ret)
        ! 左端から取り出し
        class(deque),intent(inout):: dq
        integer(int64):: ret

        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call dq_check_allocate_size(dq)
    end function
end module


module deque_real64_mod
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none
    private
    type,public:: deque
        real(real64),allocatable:: array(:)
        integer(int32):: l=1, r=0
    contains
        procedure:: push_back => dq_push_back
        procedure:: push_front => dq_push_front
        procedure:: pop_back => dq_pop_back
        procedure:: pop_front => dq_pop_front
        procedure:: size => dq_size
        procedure:: remain => dq_remain
    end type
contains
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

        ret = dq%size() > 0
    end function


    pure subroutine dq_append_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr,nl,nr,dl,dr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size() 

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = rr

        ! 新しい配列の両端
        nl = ll
        nr = rr+l ! 配列の右側をデータ長分拡張する。

        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_append_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32):: l,ll,rr,nl,nr,dl,dr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = rr

        ! 新しい配列の両端
        nl = ll-l
        nr = rr
        
        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_reduce_right_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::rr,l,ll,dl,dr,nl,nr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = ll
        dr = max(dq%r+l/2, dq%r+1)

        ! 新しい配列の両端
        nl = dl
        nr = dr
        
        call resize_array(dq%array,nl,nr,dl,dr)
    end subroutine


    pure subroutine dq_reduce_left_allocation(dq)
        type(deque),intent(inout):: dq
        integer(int32)::l,ll,rr,dl,dr,nl,nr

        ! データ長(配列長とはことなる。pushされたデータの両端)
        l = dq%size()

        ! 現在の配列の両端
        ll = lbound(dq%array,1)
        rr = ubound(dq%array,1)

        !引継ぐデータの両端
        dl = min(dq%l-l/2, dq%r+1)
        dr = rr

        ! 新しい配列の両端
        nl = dl
        nr = dr
        
        call resize_array(dq%array,nl,nr,dl,dr)
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
        if (dq_extra_right(dq) > dq%size()) call dq_reduce_right_allocation(dq)
        if (dq_extra_left(dq) > dq%size()) call dq_reduce_left_allocation(dq)
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

        ret = dq%array(dq%r)
        dq%r=dq%r-1
        call dq_check_allocate_size(dq)
    end function


    function dq_pop_front(dq) result(ret)
        ! 左端から取り出し
        class(deque),intent(inout):: dq
        real(real64):: ret

        ret = dq%array(dq%l)
        dq%l=dq%l+1
        call dq_check_allocate_size(dq)
    end function
end module


module deque_mod
    use deque_int32_mod,deque_int32 => deque
    use deque_int64_mod,deque_int64 => deque
    use deque_real64_mod,deque_real64 => deque
end module
