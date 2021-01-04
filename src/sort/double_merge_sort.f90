! ar1のソート順序でar2もソートする。
module double_merge_sort_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: double_merge_sort

    interface double_merge_sort
        module procedure dms32, dms64
    end interface
contains
    recursive subroutine dms32(ar1, ar2)
        integer(int32),intent(inout):: ar1(:), ar2(:)
        integer(int32):: m

        if (size(ar1) <= 1) then
            return
        else
            m = size(ar1)/2
            call dms32(ar1(:m), ar2(:m))
            call dms32(ar1(m+1:), ar2(m+1:))
            call sub_ms32(ar1,ar2)
        end if
    end subroutine


    subroutine sub_ms32(ar1, ar2)
        integer(int32),intent(inout):: ar1(:), ar2(:)
        integer(int32):: tmp1(1:size(ar1)), tmp2(1:size(ar2))
        integer(int32):: m,l,r,i

        m=size(ar1)/2; l=1; r=m+1
        tmp1(:)=ar1(:); tmp2(:)=ar2(:)

        do i=1,size(ar1)
            if (m < l) then
                ar1(i:) = tmp1(r:)
                ar2(i:) = tmp2(r:)
                return
            else if (size(ar1) < r) then
                ar1(i:) = tmp1(l:m)
                ar2(i:) = tmp2(l:m)
                return
            else
                if (tmp1(l) <= tmp1(r)) then
                    ar1(i) = tmp1(l)
                    ar2(i) = tmp2(l)
                    l=l+1
                else
                    ar1(i) = tmp1(r)
                    ar2(i) = tmp2(r)
                    r=r+1
                end if
            end if
        end do
    end subroutine


    recursive subroutine dms64(ar1, ar2)
        integer(int64),intent(inout):: ar1(:), ar2(:)
        integer(int32):: m

        if (size(ar1) <= 1) then
            return
        else
            m = size(ar1)/2
            call dms64(ar1(:m), ar2(:m))
            call dms64(ar1(m+1:), ar2(m+1:))
            call sub_ms64(ar1,ar2)
        end if
    end subroutine


    subroutine sub_ms64(ar1, ar2)
        integer(int64),intent(inout):: ar1(:), ar2(:)
        integer(int64):: tmp1(1:size(ar1)), tmp2(1:size(ar2))
        integer(int32):: m,l,r,i

        m=size(ar1)/2; l=1; r=m+1
        tmp1(:)=ar1(:); tmp2(:)=ar2(:)

        do i=1,size(ar1)
            if (m < l) then
                ar1(i:) = tmp1(r:)
                ar2(i:) = tmp2(r:)
                return
            else if (size(ar1) < r) then
                ar1(i:) = tmp1(l:m)
                ar2(i:) = tmp2(l:m)
                return
            else
                if (tmp1(l) <= tmp1(r)) then
                    ar1(i) = tmp1(l)
                    ar2(i) = tmp2(l)
                    l=l+1
                else
                    ar1(i) = tmp1(r)
                    ar2(i) = tmp2(r)
                    r=r+1
                end if
            end if
        end do
    end subroutine
end module