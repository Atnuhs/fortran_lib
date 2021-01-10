
program test_fft
    use,intrinsic :: iso_fortran_env
    use fft_mod
    implicit none
    integer(int32):: n, n2, i
    real(real64),allocatable:: ar(:), ai(:), br(:), bi(:)


    n = 1000
    call init_fft(n)
    n2 = get_fft_len()
    allocate(ar(n2),source=[(dble(i), i=1, n2)])
    allocate(ai(n2),source=0d0)
    allocate(br(n2), source=ar)
    allocate(bi(n2), source=ai)
    call fft2(ar,ai)
    call ifft2(ar,ai)
    call dft(br,bi)
    call idft(br,bi)

    open(unit=10, file="fft_debug", status="replace")
    write(10,'(a)') 'i, ar,ai,br,bi,cr,ci'
        do i=1,min(50,n)
            write(10,*) i, ar(i),ai(i),br(i),bi(i)
        end do
    write(10,'(a)') 'sum of... ar,ai,br,bi,cr,ci'
        write(10, *) sum(ar),sum(ai), sum(br), sum(bi)
    close(10)
contains
    function auto_correlation_function_non_fft(ar) result(ret)
        real(real64):: ar(:)
        real(real64):: ret(size(ar))
        integer(int32):: d, i, n
        
        n = size(ar)
        ret(:) = 0
        do d=0,n-1
            do i=1,n-d
                ret(d+1)=ret(d+1)+ar(i)*ar(i+d)
            end do
        end do
    end function

    subroutine dft_sub(xr, xi, inv)
        real(real64),intent(inout):: xr(:), xi(:)
        real(real64):: yr(size(xr)),yi(size(xi)), theta,k
        integer(int32):: i,j,n
        logical:: inv

        n = size(xr)
        yr(:) = 0d0
        yi(:) = 0d0
        theta = -2d0*acos(-1d0)/dble(n)
        if (inv) theta = -theta
        do i=1,n
            do j=1,n
                k = dble((j-1)*(i-1))*theta
                yr(i)=yr(i)+xr(j)*cos(k) - xi(j)*sin(k)
                yi(i)=yi(i)+xr(j)*sin(k) + xi(j)*cos(k)
            end do
        end do
        xr(:) = yr(:)
        xi(:) = yi(:)
    end subroutine

    subroutine dft(xr,xi)
        real(real64),intent(inout):: xr(:),xi(:)

        call dft_sub(xr,xi,.false.)
    end subroutine 

    subroutine idft(xr,xi)
        real(real64),intent(inout):: xr(:),xi(:)

        call dft_sub(xr,xi,.true.)
        xr(:) = xr(:) / dble(size(xr))
        xi(:) = xi(:) / dble(size(xi))
    end subroutine
end program test_fft