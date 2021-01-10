program bench_fft
    use,intrinsic :: iso_fortran_env
    use fft_mod
    implicit none
    abstract interface
    function acf(ar)
        use,intrinsic :: iso_fortran_env
        implicit none
        real(real64),intent(in):: ar(:)
        real(real64):: acf(size(ar))
    end function
end interface
    integer(int32):: n, i
    real(real64):: t1,t2,t3

    read*, n
    i=1
    print*, 'a'
    open(unit=11, file='fft_bench_result.txt', status='replace')
        do while (i <= n)
            write(*,'(i10)',advance="no") i
            t1 = bench_fftacf(i)
            write(*,'(e15.6)',advance="no") t1
            t2 = benchmark(auto_correlation_function_non_fft, i)
            write(*,'(e15.6)',advance="no") t2
            t3 = benchmark(auto_correlation_function_non_fft_vectorized, i)
            write(*,'(e15.6)') t3
            write(11,*) i, t1, t2, t3
            i=lshift(i,1)
        end do
    close(11)
contains
    function bench_fftacf(n) result(time)
        real(real64):: time
        real(real64),parameter:: maxtime=0.1d0
        real(real64):: ar(n)
        integer(int32):: i, n, n2

        time = 0d0
        i=0
        ar(:) = [(i, i=1,n)]
        call init_acf(n)

        do while(time <= maxtime)
            time = time + bench_sub_fftacf(ar)
            i=i+1
        end do
        time = time/real(i,kind=real64)
        write(*,'(i10)',advance="no") i
    end function

    function bench_sub_fftacf(ar) result(time)
        real(real64):: time, time_begin, time_end
        real(real64):: ar(:), b(size(ar))

        call cpu_time(time_begin)
        b(:) = auto_correlation_function(ar)
        call cpu_time(time_end)
        time = time_end - time_begin
    end function

    function benchmark(f,n) result(time)
        procedure(acf):: f
        real(real64):: time
        real(real64),parameter:: maxtime=0.1d0
        real(real64):: ar(n)
        integer(int32):: i, n, n2

        time = 0d0
        i=0
        ar(:) = [(i, i=1,n)]
        n2 = 1
        do while (n2 < n)
            n2 = n2*2
        end do

        do while(time <= maxtime)
            time = time + bench_sub(f,ar,n2)
            i=i+1
        end do
        time = time/real(i,kind=real64)
        write(*,'(i10)',advance="no") i
    end function


    function bench_sub(f,ar,n) result(time)
        procedure(acf):: f
        integer(int32):: n
        real(real64):: time, time_begin, time_end
        real(real64):: ar(:), b(size(ar))

        call cpu_time(time_begin)
        b(:) = f(ar)
        call cpu_time(time_end)
        time = time_end - time_begin
    end function

    function auto_correlation_function_non_fft(ar) result(ret)
        real(real64),intent(in):: ar(:)
        real(real64):: ret(size(ar))
        integer(int32):: d, i, n
        
        n = size(ar)
        do d=0,n-1
            ret(d+1) = 0d0
            do i=1,n-d
                ret(d+1)=ret(d+1)+ar(i)*ar(i+d)
            end do
        end do
    end function


    function auto_correlation_function_non_fft_vectorized(ar) result(ret)
        real(real64),intent(in):: ar(:)
        real(real64):: ret(size(ar)), ari
        integer(int32):: d, i, j, n
        
        n = size(ar)
        ret(:) = 0
        ! do d=0,n-1
        !     ret(d+1) = sum([(ar(i)*ar(i+d), i=1, n-d)])
        ! end do

        do i=1,n
            ari = ar(i)
            do j=i,n
                ret(j-i+1) = ret(j-i+1) + ari*ar(j)
            end do  
        end do
    end function
end program bench_fft