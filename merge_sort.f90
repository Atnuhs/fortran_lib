module merge_sort_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort

    interface merge_sort
        module procedure ms32, ms64
    end interface
contains
    recursive function ms32(ar) result(ret)
        integer(int32),intent(in):: ar(:)
        integer(int32):: ret(size(ar))
        integer(int32):: m

        if (size(ar) <= 1) then
            ret(:) = ar(:)
        else
            m = size(ar)/2
            ret = sub_ms32([ms32(ar(:m)),ms32(ar(m+1:))])
        end if
    end function


    function sub_ms32(ar) result(ret)
        integer(int32),intent(in):: ar(:)
        integer(int32):: ret(size(ar))
        integer(int32):: m,l,r,i

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