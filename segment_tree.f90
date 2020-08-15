! T1, F1
!min: inf, min(a,b)
!max: 0,   max(a,b)
!sum: 0,   a+b
module segment_tree_mod
    use,intrinsic :: iso_fortran_env
    implicit none

    type seg_tree
        integer(int32):: n,n2
        integer(int32),pointer:: v(:)
    end type
contains
    function f(a,b) result(ret)
        integer(int32):: a,b,ret
        ret = F1
    end function

    subroutine st_init(st, n)
        type(seg_tree):: st
        integer(int32),intent(in):: n
        integer(int32):: x
        x=1
        do while(n > x)
            x = 2*x
        end do
        st%n = x
        st%n2 = 2*x-1
        allocate(st%v(st%n2), source=T1)
    end subroutine

    subroutine st_update(st, i, x)
        class(seg_tree):: st
        integer(int32), intent(in):: i
        integer(int32):: ind
        integer(int32), intent(in):: x
        
        ind = i + st%n - 1
        st%v(ind) = x
        do while(ind > 1)
            ind = ind/2
            st%v(ind) = f(st%v(2*ind), st%v(2*ind+1))
        end do
    end subroutine

    function st_query(st, a, b) result(ret)
        type(seg_tree):: st
        integer(int32), intent(in):: a,b
        integer(int32):: ret
        ret = st_query_sub(st, a, b, 1, 1, st%n)
    end function

    recursive function st_query_sub(st, a, b, k, l, r) result(ret)
        type(seg_tree),intent(in):: st
        integer(int32),intent(in):: a,b,k,l,r
        integer(int32):: ret, vl, vr

        if (r < a .or. b < l) then
            ret = T1
        else if (a <= l .and. r <= b) then
            ret = st%v(k)
        else
            vl = st_query_sub(st,a,b,k*2, l, (l+r)/2)
            vr = st_query_sub(st,a,b,k*2+1, (l+r)/2+1, r)
            ret = f(vl,vr)
        end if
    end function
end module