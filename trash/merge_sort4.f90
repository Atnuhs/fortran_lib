    recursive subroutine merge_sort(ar, fst, lst)
        integer(int32),intent(inout):: ar(:)
        integer(int32),intent(in):: fst,lst
        integer(int32):: mdl

        if (lst-fst < 2) then
            if (ar(fst) > ar(lst)) call swap(ar(fst),ar(lst))
            return
        end if

        mdl = (fst+lst)/2
        call merge_sort(ar, fst, mdl)
        call merge_sort(ar, mdl+1, lst)
        call merge_(ar, fst, mdl, lst)
    end subroutine

    subroutine merge_(ar, fst, mdl, lst)
        integer(int32),intent(inout):: ar(:)
        integer(int32),intent(in):: fst, mdl, lst
        integer(int32),allocatable:: tmp(:)
        integer(int32):: li, ri, ti

        allocate(tmp(lst-fst+1))

        li=fst
        ri=mdl+1   
        ti=1

        do while (li <= mdl .and. ri <= lst)
            if (ar(li) <= ar(ri)) then
                tmp(ti) = ar(li)
                li=li+1
            else
                tmp(ti) = ar(ri)
                ri=ri+1
            end if
            ti=ti+1
        end do

        if (li <= mdl) then
            tmp(ti:) = ar(li:mdl)
        else
            tmp(ti:) = ar(ri:lst)
        end if

        ar(fst:lst) = tmp(:)
        deallocate(tmp)
    end subroutine

    subroutine swap(x,y)
        integer(int32),intent(inout):: x,y
        integer(int32):: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine