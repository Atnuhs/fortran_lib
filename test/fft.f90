
program test_fft
    use,intrinsic :: iso_fortran_env
    use fft_2_mod
    implicit none
    integer(int32):: n, n2, i
    real(real64),allocatable:: a(:), b(:), c(:), d(:)

    n = 4
    n2 = 1
    do while (n2 < n)
        n2 = n2*2
    end do
    allocate(a(n),source=[(dble(i), i=1, n)])
    allocate(b(n), c(n), d(n))

    b = auto_correlation_function(a, n2)
    call liner_correlation(a,a,c)
    d = auto_correlation_function_non_fft(a)

    open(unit=10, file="fft_debug", status="replace")
    write(10,'(a)') 'i, source, acffft, lcfft, nonfft (should be -> acffft == lcfft == nonfft)'
        do i=1,min(50,n)
            write(10,*) i, a(i), b(i), c(i), d(i), d(i)-c(i)
        end do
    write(10,'(a)') 'sum of... acffft, lcfft, nonfft (should be -> acffft == lcfft == nonfft)'
        write(10, *) sum(b),sum(c), sum(d)
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
end program test_fft