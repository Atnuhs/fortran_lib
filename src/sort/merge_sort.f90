module merge_sort_int32
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    subroutine merge_sort(ar)
        integer(int32),intent(inout):: ar(:)
        integer(int32):: i,n,d,hd,tmp(size(ar))

        n = size(ar)
        do i=1,n-1,2
            if (ar(i+1) < ar(i)) call swap(ar(i), ar(i+1))
        end do
        hd=2
        d=4
        do while(hd < n)
            do i=1,n-d,d
                call merge_sub(ar(i:i+d-1), tmp, hd, hd+1, d)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                call merge_sub(ar(i:), tmp, hd, hd+1, n-i+1) 
            end if
            hd=d
            d=d*2
        end do
    end subroutine


    subroutine merge_sub(ar, tmp, n1, i2, n2)
        integer(int32),intent(inout):: ar(:), tmp(:)
        integer(int32),value:: n1, i2, n2
        integer(int32):: i,i1
        
        i1=1
        tmp(i1:n1) = ar(i1:n1)
        do i=1,n2
            if (n1 < i1) then
                return
            else if (n2 < i2) then
                ar(i:)=tmp(i1:n1)
                return
            else
                if (ar(i2) <= tmp(i1)) then
                    ar(i) = ar(i2); i2=i2+1
                else
                    ar(i) = tmp(i1); i1=i1+1
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
end module


module merge_sort_int64
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    subroutine merge_sort(ar)
        integer(int64),intent(inout):: ar(:)
        integer(int64):: i,n,d,hd,tmp(size(ar))

        n = size(ar)
        do i=1,n-1,2
            if (ar(i+1) < ar(i)) call swap(ar(i), ar(i+1))
        end do
        hd=2
        d=4
        do while(hd < n)
            do i=1,n-d,d
                call merge_sub(ar(i:i+d-1), tmp, hd, hd+1, d)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                call merge_sub(ar(i:), tmp, hd, hd+1, n-i+1) 
            end if
            hd=d
            d=d*2
        end do
    end subroutine


    subroutine merge_sub(ar, tmp, n1, i2, n2)
        integer(int64),intent(inout):: ar(:), tmp(:)
        integer(int64),value:: n1, i2, n2
        integer(int64):: i,i1
        
        i1=1
        tmp(i1:n1) = ar(i1:n1)
        do i=1,n2
            if (n1 < i1) then
                return
            else if (n2 < i2) then
                ar(i:)=tmp(i1:n1)
                return
            else
                if (ar(i2) <= tmp(i1)) then
                    ar(i) = ar(i2); i2=i2+1
                else
                    ar(i) = tmp(i1); i1=i1+1
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
end module


module merge_sort_char
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    subroutine merge_sort(ar)
        character(*),intent(inout):: ar(:)
        character(:),allocatable:: tmp(:)
        integer(int32):: i,n,d,hd

        n = size(ar)
        allocate(tmp, source=ar)
        do i=1,n-1,2
            if (ar(i+1) < ar(i)) call swap(ar(i), ar(i+1))
        end do
        hd=2
        d=4
        do while(hd < n)
            do i=1,n-d,d
                call merge_sub(ar(i:i+d-1), tmp, hd, hd+1, d)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                call merge_sub(ar(i:), tmp, hd, hd+1, n-i+1) 
            end if
            hd=d
            d=d*2
        end do
    end subroutine


    subroutine merge_sub(ar, tmp, n1, i2, n2)
        character(*),intent(inout):: ar(:), tmp(:)
        integer(int32),value:: n1, i2, n2
        integer(int32):: i,i1
        
        i1=1
        tmp(i1:n1) = ar(i1:n1)
        do i=1,n2
            if (n1 < i1) then
                return
            else if (n2 < i2) then
                ar(i:)=tmp(i1:n1)
                return
            else
                if (ar(i2) <= tmp(i1)) then
                    ar(i) = ar(i2); i2=i2+1
                else
                    ar(i) = tmp(i1); i1=i1+1
                end if
            end if
        end do
    end subroutine

    subroutine swap(x, y)
        character(*),intent(inout):: x, y
        character(:),allocatable:: t1, t2

        allocate(t1, mold=x)
        allocate(t2, mold=y)
        t1 = x
        t2 = y
        x = t2
        y = t1
    end subroutine
end module


module merge_sort_mod
    use,intrinsic :: iso_fortran_env
    use merge_sort_int32, ms32 => merge_sort
    use merge_sort_int64, ms64 => merge_sort
    use merge_sort_char,  msch => merge_sort
    interface merge_sort
        module procedure ms32, ms64, msch
    end interface
end module