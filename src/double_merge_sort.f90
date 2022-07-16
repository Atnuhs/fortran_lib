module double_merge_sort_int32
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    subroutine merge_sort(ar, id)
        integer(int32),intent(inout):: ar(:), id(:)
        integer(int32):: tmp(size(ar)), idtmp(size(id))

        call merge_sort_sub(1_int32, size(ar,kind=int32), ar, tmp, id, idtmp)
    end subroutine

    recursive subroutine merge_sort_sub(l, r, ar, tmp, id, idtmp)
        integer(int32),intent(in):: l, r
        integer(int32),intent(inout):: ar(:), tmp(:), id(:), idtmp(:)
        integer(int32):: m

        if (r-l <= 1) then
            if (ar(l) > ar(r)) then
                m = ar(l)
                ar(l) = ar(r)
                ar(r) = m
                m = id(l)
                id(l) = id(r)
                id(r) = m
            end if
        else
            m = (l+r) / 2
            call merge_sort_sub(l, m ,ar, tmp, id, idtmp)
            call merge_sort_sub(m+1, r, ar, tmp, id, idtmp)
            call merge_sub(l, m, r, ar, tmp, id, idtmp)
        end if
    end subroutine

    subroutine merge_sub(l, n1, n2, ar, tmp, id, idtmp)
        integer(int32), intent(in):: l, n1, n2
        integer(int32), intent(inout):: ar(:), tmp(:), id(:), idtmp(:)
        integer(int32):: i1, i2, it, nt

        i1 = l
        i2 = n1+1
        it = 1
        nt = n2-l+1

        do while(i1 <= n1 .and. i2 <= n2)
            if (ar(i1) <= ar(i2)) then
                tmp(it) = ar(i1)
                idtmp(it) = id(i1)
                i1=i1+1
            else
                tmp(it) = ar(i2)
                idtmp(it) = id(i1)
                i2=i2+1
            end if
            it=it+1
        end do

        if (i1 <= n1) then
            tmp(it:nt) = ar(i1:n1)
            idtmp(it:nt) = id(i1:n1)
        else if (i2 <= n2) then
            tmp(it:nt) = ar(i2:n2)
            idtmp(it:nt) = id(i2:n2)
        end if
        ar(l:n2) = tmp(1:nt)
        id(l:n2) = idtmp(1:nt)
    end subroutine
end module



module double_merge_sort_int64
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    subroutine merge_sort(ar, id)
        integer(int64),intent(inout):: ar(:), id(:)
        integer(int64):: tmp(size(ar)), idtmp(size(id))

        call merge_sort_sub(1_int64, size(ar,kind=int64), ar, tmp, id, idtmp)
    end subroutine

    recursive subroutine merge_sort_sub(l, r, ar, tmp, id, idtmp)
        integer(int64),intent(in):: l, r
        integer(int64),intent(inout):: ar(:), tmp(:), id(:), idtmp(:)
        integer(int64):: m

        if (r-l <= 1) then
            if (ar(l) > ar(r)) then
                m = ar(l)
                ar(l) = ar(r)
                ar(r) = m
                m = id(l)
                id(l) = id(r)
                id(r) = m
            end if
        else
            m = (l+r) / 2
            call merge_sort_sub(l, m ,ar, tmp, id, idtmp)
            call merge_sort_sub(m+1, r, ar, tmp, id, idtmp)
            call merge_sub(l, m, r, ar, tmp, id, idtmp)
        end if
    end subroutine

    subroutine merge_sub(l, n1, n2, ar, tmp, id, idtmp)
        integer(int64), intent(in):: l, n1, n2
        integer(int64), intent(inout):: ar(:), tmp(:), id(:), idtmp(:)
        integer(int64):: i1, i2, it, nt

        i1 = l
        i2 = n1+1
        it = 1
        nt = n2-l+1

        do while(i1 <= n1 .and. i2 <= n2)
            if (ar(i1) <= ar(i2)) then
                tmp(it) = ar(i1)
                idtmp(it) = id(i1)
                i1=i1+1
            else
                tmp(it) = ar(i2)
                idtmp(it) = id(i1)
                i2=i2+1
            end if
            it=it+1
        end do

        if (i1 <= n1) then
            tmp(it:nt) = ar(i1:n1)
            idtmp(it:nt) = id(i1:n1)
        else if (i2 <= n2) then
            tmp(it:nt) = ar(i2:n2)
            idtmp(it:nt) = id(i2:n2)
        end if
        ar(l:n2) = tmp(1:nt)
        id(l:n2) = idtmp(1:nt)
    end subroutine
end module


module double_merge_sort_char
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    subroutine merge_sort(ar, id)
        character(*),intent(inout):: ar(:)
        integer(int32), intent(inout):: id(:)
        character(len(ar(1))):: tmp(size(ar))
        integer(int32):: idtmp(size(id))
        

        call merge_sort_sub(1_int32, size(ar,kind=int32), ar, tmp, id, idtmp)
    end subroutine

    recursive subroutine merge_sort_sub(l, r, ar, tmp, id, idtmp)
        integer(int32),intent(in):: l, r
        character(*),intent(inout):: ar(:), tmp(:)
        integer(int32),intent(inout):: id(:), idtmp(:)
        integer(int32):: m
        character(len(ar(1))):: c

        if (r-l <= 1) then
            if (ar(l) > ar(r)) then
                c = ar(l)
                ar(l) = ar(r)
                ar(r) = c
                m = id(l)
                id(l) = id(r)
                id(r) = m
            end if
        else
            m = (l+r) / 2
            call merge_sort_sub(l, m ,ar, tmp, id, idtmp)
            call merge_sort_sub(m+1, r, ar, tmp, id, idtmp)
            call merge_sub(l, m, r, ar, tmp, id, idtmp)
        end if
    end subroutine

    subroutine merge_sub(l, n1, n2, ar, tmp, id, idtmp)
        integer(int32), intent(in):: l, n1, n2
        character(*), intent(inout):: ar(:), tmp(:)
        integer(int32), intent(inout):: id(:), idtmp(:)
        integer(int32):: i1, i2, it, nt

        i1 = l
        i2 = n1+1
        it = 1
        nt = n2-l+1

        do while(i1 <= n1 .and. i2 <= n2)
            if (ar(i1) <= ar(i2)) then
                tmp(it) = ar(i1)
                idtmp(it) = id(i1)
                i1=i1+1
            else
                tmp(it) = ar(i2)
                idtmp(it) = id(i2)
                i2=i2+1
            end if
            it=it+1
        end do

        if (i1 <= n1) then
            tmp(it:nt) = ar(i1:n1)
            idtmp(it:nt) = id(i1:n1)
        else if (i2 <= n2) then
            tmp(it:nt) = ar(i2:n2)
            idtmp(it:nt) = id(i2:n2)
        end if
        ar(l:n2) = tmp(1:nt)
        id(l:n2) = idtmp(1:nt)
    end subroutine
end module


module double_merge_sort_mod
    use,intrinsic :: iso_fortran_env
    use double_merge_sort_int32, ms32 => merge_sort
    use double_merge_sort_int64, ms64 => merge_sort
    use double_merge_sort_char,  msch => merge_sort
    interface merge_sort
        module procedure ms32, ms64, msch
    end interface
end module
