module mt19937
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter,private:: N = 624
    integer(int32),parameter,private:: N1 = 625
    integer(int32),parameter,private:: M = 397
    integer(int32),parameter,private:: MATA = -1727483681
    integer(int32),parameter,private:: UMASK = -2147483648_int64
    integer(int32),parameter,private:: LMASK = 2147483647
    integer(int32),parameter,private:: TMASKB = -1658038656
    integer(int32),parameter,private:: TMASKC = -272236544
    real(real64),parameter,private:: b32 = 2.0d0 ** 32
    integer(int32),private :: mti = N1, mt(0:N-1), mag01(0:1) = [0, MATA]
contains
    subroutine set_seed(seed)
        integer(int32),intent(in) :: seed
        mt(0) = iand(seed, -1)
        do mti = 1, N-1
            mt(mti) = iand(69069 * mt(mti-1), -1)
        end do
    end subroutine set_seed

    function random_mt()
        integer(int32) :: y, kk
        real(real64):: random_mt

        if(mti >= N) then
            if(mti == N+1) call set_seed(4357)
            do kk=0, N-M-1
                y = ior(iand(mt(kk), UMASK), iand(mt(kk+1), LMASK))
                mt(kk) = ieor(ieor(mt(kk+M), ishft(y, -1)), mag01(iand(y, 1)))
            end do
    
            do kk = N-M, N-2
                y = ior(iand(mt(kk), UMASK), iand(mt(kk+1), LMASK))
                mt(kk) = ieor(ieor(mt(kk+(M-N)), ishft(y, -1)), mag01(iand(y, 1)))
            end do
    
            y = ior(iand(mt(N-1), UMASK), iand(mt(0), LMASK))
            mt(N-1) = ieor(ieor(mt(M-1), ishft(y, -1)), mag01(iand(y, 1)))
            mti = 0
        endif
    
        y = mt(mti)
        mti = mti+1
        y = ieor(y, ishft(y, -11))
        y = ieor(y, iand(ishft(y, 7), TMASKB))
        y = ieor(y, iand(ishft(y, 15), TMASKC))
        y = ieor(y, ishft(y, -18))

        if(y < 0) then
            random_mt = (dble(y)+b32) / b32
        else
            random_mt = dble(y) / b32
        endif
    end function random_mt
end module mt19937