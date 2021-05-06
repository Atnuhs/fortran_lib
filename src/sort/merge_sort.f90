module merge_sort_int32
    ! call merge_sort(arr) <- int32, int64, character
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: merge_sort
contains
    recursive subroutine merge_sort(ar)
        integer(int32),intent(inout):: ar(:)
        integer(int32):: i,n,d,hd,tmp(size(ar))

        n = size(ar)
        do i=1,n-1,2
            if (ar(i+1) < ar(i)) call swap(ar(i), ar(i+1))
        end do
        hd=2
        d=4
        do while(hd <= n)
            do i=1,n-d,d
                call merge_sub(ar(i:i+d-1), tmp, hd, hd+1, d)
            end do
            i = (n/d)*d+1
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

program main
    use,intrinsic :: iso_fortran_env
    use merge_sort_int32
    implicit none

    integer(int32):: n
    integer(int32), allocatable:: a(:)

    read*, n
    allocate(a(n))
    read*, a(:)
    call merge_sort(a)
    print'(*(i0,1x))', a(:)
end program main