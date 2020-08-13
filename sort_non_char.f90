module merge_sort_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort, double_merge_sort
    interface merge_sort
        module procedure ms32, ms64
    end interface

    interface double_merge_sort
        module procedure msd3232, msd6464
    end interface

contains
    recursive subroutine ms32(ar, fst, lst)
        integer(int32),intent(inout):: ar(:)
        integer(int32),intent(in):: fst,lst
        integer(int32):: mdl

        if (lst-fst < 2) then
            if (ar(fst) > ar(lst)) call swap32(ar(fst),ar(lst))
            return
        end if

        mdl = (fst+lst)/2
        call ms32(ar, fst, mdl)
        call ms32(ar, mdl+1, lst)
        call merge32(ar, fst, mdl, lst)
    end subroutine


    subroutine merge32(ar, fst, mdl, lst)
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


    subroutine swap32(x,y)
        integer(int32),intent(inout):: x,y
        integer(int32):: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine


    recursive subroutine ms64(ar, fst, lst)
        integer(int64),intent(inout):: ar(:)
        integer(int64),intent(in):: fst,lst
        integer(int64):: mdl

        if (lst-fst < 2) then
            if (ar(fst) > ar(lst)) call swap64(ar(fst),ar(lst))
            return
        end if

        mdl = (fst+lst)/2
        call ms64(ar, fst, mdl)
        call ms64(ar, mdl+1, lst)
        call merge64(ar, fst, mdl, lst)
    end subroutine


    subroutine merge64(ar, fst, mdl, lst)
        integer(int64),intent(inout):: ar(:)
        integer(int64),intent(in):: fst, mdl, lst
        integer(int64),allocatable:: tmp(:)
        integer(int64):: li, ri, ti

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


    subroutine swap64(x,y)
        integer(int64),intent(inout):: x,y
        integer(int64):: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine

    recursive subroutine msd3232(ar1, ar2, fst, lst)
        integer(int32),intent(inout):: ar1(:),ar2(:)
        integer(int32),intent(in):: fst,lst
        integer(int32):: mdl

        if (lst - fst < 2) then
            if (ar1(fst) > ar1(lst)) then
                call swap32(ar1(fst), ar1(lst))
                call swap32(ar2(fst), ar2(lst))
            end if
            return
        end if

        mdl = (fst+lst)/2

        call msd3232(ar1,ar2,fst,mdl)
        call msd3232(ar1,ar2,mdl+1,lst)
        call merged3232(ar1,ar2,fst,mdl,lst)
    end subroutine


    subroutine merged3232(ar1,ar2,fst,mdl,lst)
        integer(int32),intent(inout):: ar1(:),ar2(:)
        integer(int32),intent(in):: fst,mdl,lst
        integer(int32),allocatable:: t1(:),t2(:)
        integer(int32):: li,ri,ti

        allocate(t1(lst-fst+1), t2(lst-fst+1))

        li=fst
        ri=mdl+1
        ti=1

        do while(li <= mdl .and. ri <= lst)
            if (ar1(li) <= ar1(ri)) then
                t1(ti) = ar1(li) 
                t2(ti) = ar2(li)
                li=li+1
            else
                t1(ti) = ar1(ri)
                t2(ti) = ar2(ri)
                ri=ri+1
            end if
            ti=ti+1
        end do

        if (li <= mdl) then
            t1(ti:) = ar1(li:mdl)
            t2(ti:) = ar2(li:mdl)
        else
            t1(ti:) = ar1(ri:lst)
            t2(ti:) = ar2(ri:lst)
        end if

        ar1(fst:lst) = t1(:)
        ar2(fst:lst) = t2(:)

        deallocate(t1,t2)
    end subroutine


    recursive subroutine msd6464(ar1, ar2, fst, lst)
        integer(int64),intent(inout):: ar1(:),ar2(:)
        integer(int64),intent(in):: fst,lst
        integer(int64):: mdl

        if (lst - fst < 2) then
            if (ar1(fst) > ar1(lst)) then
                call swap64(ar1(fst), ar1(lst))
                call swap64(ar2(fst), ar2(lst))
            end if
            return
        end if

        mdl = (fst+lst)/2

        call msd6464(ar1,ar2,fst,mdl)
        call msd6464(ar1,ar2,mdl+1,lst)
        call merged6464(ar1,ar2,fst,mdl,lst)
    end subroutine


    subroutine merged6464(ar1,ar2,fst,mdl,lst)
        integer(int64),intent(inout):: ar1(:),ar2(:)
        integer(int64),intent(in):: fst,mdl,lst
        integer(int64),allocatable:: t1(:),t2(:)
        integer(int64):: li,ri,ti

        allocate(t1(lst-fst+1), t2(lst-fst+1))

        li=fst
        ri=mdl+1
        ti=1

        do while(li <= mdl .and. ri <= lst)
            if (ar1(li) <= ar1(ri)) then
                t1(ti) = ar1(li) 
                t2(ti) = ar2(li)
                li=li+1
            else
                t1(ti) = ar1(ri)
                t2(ti) = ar2(ri)
                ri=ri+1
            end if
            ti=ti+1
        end do

        if (li <= mdl) then
            t1(ti:) = ar1(li:mdl)
            t2(ti:) = ar2(li:mdl)
        else
            t1(ti:) = ar1(ri:lst)
            t2(ti:) = ar2(ri:lst)
        end if

        ar1(fst:lst) = t1(:)
        ar2(fst:lst) = t2(:)

        deallocate(t1,t2)
    end subroutine
end module