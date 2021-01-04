program bench_fft
    use,intrinsic :: iso_fortran_env
    use fft_2_mod
    implicit none
    integer(int32):: n, i

    read*, n
    i=1
    open(unit=11, file='fft_bench_result.txt', status='replace')
        do while (i <= n)
            call benchmark(i)
            i=lshift(i,1)
        end do
    close(11)
contains
    subroutine benchmark(n)
        integer(int32):: n, i,nexc
        integer(int32),parameter:: maxexc=10000
        real(real64):: t1, t2, t3
        real(real64),parameter:: maxt=2d-1

        t1 = bench_acf(n)
        if (t1 < maxt) then
            nexc=maxexc
            if (t1 > 0d0) nexc=min(int(maxt/t1), nexc)
            do i=1,nexc
                t1 = t1 + bench_acf(n)
            end do
            t1=t1/real(nexc,kind=real64)
        end if

        t2 = bench_acf_nonfft(n)
        if (t2 < maxt) then
            nexc=maxexc
            if (t2 > 0d0) nexc=min(int(maxt/t2), nexc)
            do i=1,nexc
                t2 = t2 + bench_acf_nonfft(n)
            end do
            t2=t2/real(nexc,kind=real64)
        end if
        
        t3 = bench_acf_non_fft_vectorized(n)
        if (t3 < maxt) then
            nexc=maxexc
            if (t3 > 0d0) nexc=min(int(maxt/t3), nexc)
            do i=1,nexc
                t3 = t3 + bench_acf_non_fft_vectorized(n)
            end do
            t3=t3/real(nexc,kind=real64)
        end if

        write(11,*) n, t1, t2, t3
        print*, n, t1, t2, t3
    end subroutine


    function bench_acf(n) result(time)
        integer(int32):: n,n2,i
        real(real64):: a(n), b(n), time, time_begin, time_end

        n2 = 1
        do while (n2 < n)
            n2 = n2*2
        end do
        a(:) = [(real(i, kind=real64), i=1,n)]
        call cpu_time(time_begin)
        b(:) = auto_correlation_function(a, n)
        call cpu_time(time_end)
        time = time_end - time_begin
    end function

    function bench_acf_nonfft(n) result(time)
        integer(int32):: n,n2,i
        real(real64):: a(n), b(n), time, time_begin, time_end

        a(:) = [(real(i, kind=real64), i=1,n)]
        call cpu_time(time_begin)
        b(:) = auto_correlation_function_non_fft(a)
        call cpu_time(time_end)
        time = time_end - time_begin
    end function

    function bench_acf_non_fft_vectorized(n) result(time)
        integer(int32):: n,n2,i
        real(real64):: a(n), b(n), time, time_begin, time_end

        a(:) = [(real(i, kind=real64), i=1,n)]
        call cpu_time(time_begin)
        b(:) = auto_correlation_function_non_fft_vectorized(a)
        call cpu_time(time_end)
        time = time_end - time_begin
    end function


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


    function auto_correlation_function_non_fft_vectorized(ar) result(ret)
        real(real64):: ar(:)
        real(real64):: ret(size(ar))
        integer(int32):: d, i, n
        
        n = size(ar)
        ret(:) = 0
        do d=0,n-1
            ret(d+1) = sum([(ar(i)*ar(i+d), i=1, n-d)])
        end do
    end function
end program bench_fft