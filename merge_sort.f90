module merge_sort_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    recursive function merge_sort(ar) result(ret)
        integer(int32),intent(in):: ar(:)
        integer(int32):: ret(size(ar))
        integer(int32):: m

        if (size(ar) <= 1) then
            ret(:) = ar(:)
        else
            m = size(ar)/2
            ret = merge_([merge_sort(ar(:m)),merge_sort(ar(m+1:))])
        end if
    end function

    function merge_(ar) result(ret)
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
end module