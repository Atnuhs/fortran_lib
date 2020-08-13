contains
    recursive subroutine double_sort(ar1, ar2, fst, lst)
        integer(8),intent(inout):: ar1(:),ar2(:)
        integer(8),intent(in):: fst,lst
        integer(8):: mdl

        if (lst - fst < 2) then
            if (ar1(fst) < ar1(lst)) then
                call swap(ar1(fst), ar1(lst))
                call swap(ar2(fst), ar2(lst))
            end if
            return
        end if

        mdl = (fst+lst)/2

        call double_sort(ar1,ar2,fst,mdl)
        call double_sort(ar1,ar2,mdl+1,lst)
        call double_merge_(ar1,ar2,fst,mdl,lst)
    end subroutine


    subroutine double_merge_(ar1,ar2,fst,mdl,lst)
        integer(8),intent(inout):: ar1(:),ar2(:)
        integer(8),intent(in):: fst,mdl,lst
        integer(8),allocatable:: t1(:),t2(:)
        integer(8):: li,ri,ti

        allocate(t1(lst-fst+1), t2(lst-fst+1))

        li=fst
        ri=mdl+1
        ti=1

        do while(li <= mdl .and. ri <= lst)
            if (ar1(li) >= ar1(ri)) then
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

    
    subroutine swap(x,y)
        integer(8),intent(inout):: x,y
        integer(8):: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine

    