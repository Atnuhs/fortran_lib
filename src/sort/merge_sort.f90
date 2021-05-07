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
        do while(hd < n)
            ! print'(a)', repeat('=',40)
            do i=1,n-d,d
                ! print'("span ",*(i0,1x))', i, i+d-1
                call merge_sub(ar(i:i+d-1), tmp, hd, hd+1, d)
                ! print'("    : ", *(i0,1x))', ar(i:i+d-1)
            end do
            i = ((n+d-1)/d-1)*d+1
            if (i+hd <= n) then
                ! print'("span ",*(i0,1x))', i, n
                call merge_sub(ar(i:), tmp, hd, hd+1, n-i+1) 
                ! print'("    : ", *(i0,1x))', ar(i:n)
            end if
            ! print'("all: ",*(i0,1x))', ar
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

    integer(int32):: n, i
    integer(int32), allocatable:: a(:)

    read*, n
    allocate(a(n), source=[(random(-100,100),i=1,n)])
    call merge_sort(a)
    print'(*(i0,1x))', a(:)
    print*, (a(i)-a(i-1)>=0, i=2,n)

contains
function random(l,r) result(v)
    integer(int32),intent(in):: l,r
    integer(int32):: v
    real(real64):: rn

    call random_number(rn)
    v = int(rn*(r-l+1)) + l
end function
end program main