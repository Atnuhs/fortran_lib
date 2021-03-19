module merge_sort_mod
    ! call merge_sort(arr) <- int32, int64, character
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort

    interface merge_sort
        module procedure ms32, ms64, msc
    end interface
contains
    recursive subroutine ms32(ar)
        integer(int32),intent(inout):: ar(:)
        integer(int32):: m

        if (size(ar) <= 1) then
            return
        else
            m = size(ar)/2
            call ms32(ar(:m)); call ms32(ar(m+1:))
            call sub_ms32(ar)
        end if
    end subroutine


    subroutine sub_ms32(ar)
        integer(int32),intent(inout):: ar(:)
        integer(int32):: tmp(1:size(ar))
        integer(int32):: m,l,r,i
        
        tmp(:) = ar(:)
        m = size(ar)/2; l=1;r=m+1
        do i=1,size(ar)
            if (m < l) then
                ar(i:)=tmp(r:); return 
            else if (size(ar) < r) then
                ar(i:)=tmp(l:m); return
            else
                if (tmp(l) <= tmp(r)) then
                    ar(i) = tmp(l); l=l+1
                else
                    ar(i) = tmp(r); r=r+1
                end if
            end if
        end do
    end subroutine


    recursive subroutine ms64(ar)
        integer(int64),intent(inout):: ar(:)
        integer(int32):: m

        if (size(ar) <= 1) then
            return
        else
            m = size(ar)/2
            call ms64(ar(:m)); call ms64(ar(m+1:))
            call sub_ms64(ar)
        end if
    end subroutine


    subroutine sub_ms64(ar)
        integer(int64),intent(inout):: ar(:)
        integer(int64):: tmp(1:size(ar))
        integer(int32):: m,l,r,i
        
        tmp(:) = ar(:)
        m = size(ar)/2; l=1;r=m+1
        do i=1,size(ar)
            if (m < l) then
                ar(i:)=tmp(r:); return
            else if (size(ar) < r) then
                ar(i:)=tmp(l:m); return
            else
                if (tmp(l) <= tmp(r)) then
                    ar(i) = tmp(l); l=l+1
                else
                    ar(i) = tmp(r); r=r+1
                end if
            end if
        end do
    end subroutine


    recursive subroutine msc(ar)
        character(*),intent(inout):: ar(:)
        integer(int32):: m

        if (size(ar) <= 1) then
            return
        else
            m = size(ar)/2
            call msc(ar(:m)); call msc(ar(m+1:))
            call sub_msc(ar)
        end if
    end subroutine


    subroutine sub_msc(ar)
        character(*),intent(inout):: ar(:)
        character(:),allocatable:: tmp(:)
        integer(int32):: m,l,r,i
        
        allocate(tmp, source=ar)
        m = size(ar)/2; l=1;r=m+1
        do i=1,size(ar)
            if (m < l) then
                ar(i:)=tmp(r:); return 
            else if (size(ar) < r) then
                ar(i:)=tmp(l:m); return
            else
                if (tmp(l) <= tmp(r)) then
                    ar(i) = tmp(l); l=l+1
                else
                    ar(i) = tmp(r); r=r+1
                end if
            end if
        end do
    end subroutine
end module