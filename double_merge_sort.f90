! ar1のソート順序でar2もソートする。

module merge_sort_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort

    interface merge_sort
        module procedure ms32, ms64
    end interface
contains
    recursive subroutine ms32(ar1, ar2)
        integer(int32),allocatable,intent(inout):: ar1(:), ar2(:)
        integer(int32):: m

        if (size(ar) <= 1) then
            return
        else
            m = size(ar)/2
            call ms32(ar1(:m), ar2(:m))
            call ms32(ar1(m+1:), ar2(m+1:))
            call sub_ms32(ar1,ar2)
        end if
    end subroutine


    subroutine sub_ms32(ar1, ar2)
        integer(int32),allocatable,intent(inout):: ar1, ar2
        integer(int32),allocatable:: tmp1(:), tmp2(:)
        integer(int32):: m,l,r,i

        m=size(ar)/2; l=1; r=m+1
        do i=1,size(ar)
            if (m < l) then
                ret(i)=ar(r); r=r+1
            else if (size(ar) < r) then
                ret(i)=ar(l); l=l+1
            else
                if (ar(l) <= ar(r)) then
                    ret(i) = ar(l); l=l+1
                else
                    ret(i) = ar(r); r=r+1
                end if
            end if
        end do
    end function


    recursive function ms64(ar) result(ret)
        integer(int64),intent(in):: ar(:)
        integer(int64):: ret(size(ar))
        integer(int64):: m

        if (size(ar) <= 1) then
            ret(:) = ar(:)
        else
            m = size(ar)/2
            ret = sub_ms64([ms64(ar(:m)),ms64(ar(m+1:))])
        end if
    end function


    function sub_ms64(ar) result(ret)
        integer(int64),intent(in):: ar(:)
        integer(int64):: ret(size(ar))
        integer(int64):: m,l,r,i

        m = size(ar)/2; l=1;r=m+1
        do i=1,size(ar)
            if (m < l) then
                ret(i)=ar(r); r=r+1
            else if (size(ar) < r) then
                ret(i)=ar(l); l=l+1
            else
                if (ar(l) <= ar(r)) then
                    ret(i) = ar(l); l=l+1
                else
                    ret(i) = ar(r); r=r+1
                end if
            end if
        end do
    end function
end module