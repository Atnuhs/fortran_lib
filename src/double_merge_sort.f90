module double_merge_sort_int32
    use,intrinsic :: iso_fortran_env
    implicit none
    
contains
    subroutine double_merge_sort(ar1, ar2)
        integer(int32),intent(inout):: ar1(:), ar2(:)
        integer(int32):: tmp1(size(ar1)),tmp2(size(ar2))
        integer(int32):: i, n, d, hd

        n = size(ar1)
        do i=1,n-1,2
            if (ar1(i+1) < ar1(i))then 
                call swap(ar1(i), ar1(i+1))
                call swap(ar2(i), ar2(i+1))
            end if
        end do
        hd=2
        d=4
        do while(hd < n)
            do i=1,n-d,d
                call merge_sub(ar1(i:i+d-1), ar2(i:i+d-1), tmp1, tmp2, hd, hd+1, d)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                call merge_sub(ar1(i:), ar2(i:i+d-1), tmp1, tmp2, hd, hd+1, n-i+1) 
            end if
            hd=d
            d=d*2
        end do
    end subroutine


    subroutine merge_sub(ar1, ar2, tmp1, tmp2, n1, i2, n2)
        integer(int32),intent(inout):: ar1(:), tmp1(:), ar2(:), tmp2(:)
        integer(int32),value:: n1, i2, n2
        integer(int32):: i,i1
        
        i1=1
        tmp1(i1:n1) = ar1(i1:n1)
        tmp2(i1:n1) = ar2(i1:n1)
        do i=1,n2
            if (n1 < i1) then
                return
            else if (n2 < i2) then
                ar1(i:)=tmp1(i1:n1)
                ar2(i:)=tmp2(i1:n1)
                return
            else
                if (ar1(i2) <= tmp1(i1)) then
                    ar1(i) = ar1(i2)
                    ar2(i) = ar2(i2)
                    i2=i2+1
                else
                    ar1(i) = tmp1(i1)
                    ar2(i) = tmp2(i1)
                    i1=i1+1
                end if
            end if
        end do
    end subroutine


    subroutine swap(x, y)
        integer(int32),intent(inout):: x, y
        integer(int32):: t1, t2

            t1 = x
            t2 = y
            x = t2
            y = t1
    end subroutine
end module double_merge_sort_int32



module double_merge_sort_int64
    use,intrinsic :: iso_fortran_env
    implicit none
    
contains
    subroutine double_merge_sort(ar1, ar2)
        integer(int64),intent(inout):: ar1(:), ar2(:)
        integer(int64):: tmp1(size(ar1)),tmp2(size(ar2))
        integer(int32):: i, n, d, hd

        n = size(ar1)
        do i=1,n-1,2
            if (ar1(i+1) < ar1(i))then 
                call swap(ar1(i), ar1(i+1))
                call swap(ar2(i), ar2(i+1))
            end if
        end do
        hd=2
        d=4
        do while(hd < n)
            do i=1,n-d,d
                call merge_sub(ar1(i:i+d-1), ar2(i:i+d-1), tmp1, tmp2, hd, hd+1, d)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                call merge_sub(ar1(i:), ar2(i:i+d-1), tmp1, tmp2, hd, hd+1, n-i+1) 
            end if
            hd=d
            d=d*2
        end do
    end subroutine


    subroutine merge_sub(ar1, ar2, tmp1, tmp2, n1, i2, n2)
        integer(int64),intent(inout):: ar1(:), tmp1(:), ar2(:), tmp2(:)
        integer(int32),value:: n1, i2, n2
        integer(int32):: i,i1
        
        i1=1
        tmp1(i1:n1) = ar1(i1:n1)
        tmp2(i1:n1) = ar2(i1:n1)
        do i=1,n2
            if (n1 < i1) then
                return
            else if (n2 < i2) then
                ar1(i:)=tmp1(i1:n1)
                ar2(i:)=tmp2(i1:n1)
                return
            else
                if (ar1(i2) <= tmp1(i1)) then
                    ar1(i) = ar1(i2)
                    ar2(i) = ar2(i2)
                    i2=i2+1
                else
                    ar1(i) = tmp1(i1)
                    ar2(i) = tmp2(i1)
                    i1=i1+1
                end if
            end if
        end do
    end subroutine


    subroutine swap(x, y)
        integer(int64),intent(inout):: x, y
        integer(int64):: t1, t2

            t1 = x
            t2 = y
            x = t2
            y = t1
    end subroutine
end module double_merge_sort_int64



module double_merge_sort_char
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    subroutine double_merge_sort(ar1, ar2)
        character(*),intent(inout):: ar1(:), ar2(:)
        character(:),allocatable:: tmp1(:), tmp2(:)
        integer(int32):: i, n, d, hd

        allocate(tmp1, mold=ar1)
        allocate(tmp2, mold=ar2)
        n = size(ar1)
        do i=1,n-1,2
            if (ar1(i+1) < ar1(i))then 
                call swap(ar1(i), ar1(i+1))
                call swap(ar2(i), ar2(i+1))
            end if
        end do
        hd=2
        d=4
        do while(hd < n)
            do i=1,n-d,d
                call merge_sub(ar1(i:i+d-1), ar2(i:i+d-1), tmp1, tmp2, hd, hd+1, d)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                call merge_sub(ar1(i:), ar2(i:i+d-1), tmp1, tmp2, hd, hd+1, n-i+1) 
            end if
            hd=d
            d=d*2
        end do
    end subroutine


    subroutine merge_sub(ar1, ar2, tmp1, tmp2, n1, i2, n2)
        character(*),intent(inout):: ar1(:), tmp1(:), ar2(:), tmp2(:)
        integer(int32),value:: n1, i2, n2
        integer(int32):: i,i1
        
        i1=1
        tmp1(i1:n1) = ar1(i1:n1)
        tmp2(i1:n1) = ar2(i1:n1)
        do i=1,n2
            if (n1 < i1) then
                return
            else if (n2 < i2) then
                ar1(i:)=tmp1(i1:n1)
                ar2(i:)=tmp2(i1:n1)
                return
            else
                if (ar1(i2) <= tmp1(i1)) then
                    ar1(i) = ar1(i2)
                    ar2(i) = ar2(i2)
                    i2=i2+1
                else
                    ar1(i) = tmp1(i1)
                    ar2(i) = tmp2(i1)
                    i1=i1+1
                end if
            end if
        end do
    end subroutine


    subroutine swap(x, y)
        character(*),intent(inout):: x, y
        character(:),allocatable:: t1, t2

            t1 = x
            t2 = y
            x = t2
            y = t1
    end subroutine
end module double_merge_sort_char



module double_merge_sort_mod
    use,intrinsic :: iso_fortran_env
    use double_merge_sort_int32, dms32 => double_merge_sort
    use double_merge_sort_int64, dms64 => double_merge_sort
    use double_merge_sort_char,  dmsch => double_merge_sort
    interface double_merge_sort
        module procedure dms32, dms64, dmsch
    end interface
end module