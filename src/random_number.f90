module mt19937
    ! This module is the submodule of "random_number" module
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: set_seed, random_mt
    integer(int32),parameter:: N = 624
    integer(int32),parameter:: N1 = 625
    integer(int32),parameter:: M = 397
    integer(int32),parameter:: MATA = -1727483681
    integer(int32),parameter:: UMASK = -2147483648_int64
    integer(int32),parameter:: LMASK = 2147483647
    integer(int32),parameter:: TMASKB = -1658038656
    integer(int32),parameter:: TMASKC = -272236544
    real(real64),parameter:: b32 = 2.0d0 ** 32
    integer(int32) :: mti = N1, mt(0:N-1), mag01(0:1) = [0, MATA]
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


module multiple_congruential_method
    ! This module is the submodule of "random_number" module
    ! These are the random numbers used by Ootoriken.
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: random_mcm, init_mcm
    real(real64),parameter:: b0=5d0**9,tm=2d0**25
    real(real64):: dran=3d0**11
contains
    subroutine init_mcm()
        implicit none

        dran=3d0**11
    end subroutine

    function random_mcm() result(ret)
        real(real64):: ret

        dran=mod(b0*dran,tm)
        ret=dran/tm
    end function
end module


module random_number
    ! This module include
    ! random_mt random_mcm
    ! random_gaussian

    ! 1. random_mt
    ! This is a function that uses the 
    ! Mersenne Twister to return a uniform random number.
    ! ex.) rnum = random_mt()

    ! 2. random_mcm
    ! This is a function that uses the 
    ! multiplicative congruence method 
    ! to return a uniform random number.
    ! ex.) rnum = random_mcm()

    ! 3. random_gaussian
    ! This is a function that uses the 
    ! Box-Muller method to return 
    ! a normal random number.
    ! ex.) rnum = random_gaussian(random_mt) <- use Mersenne Twister
    ! ex.) rnum = random_gaussian(random_mcm) <- use multiplicative congruence method
    use,intrinsic :: iso_fortran_env
    use multiple_congruential_method
    use mt19937
    implicit none
    abstract interface
        function random_func()
            use,intrinsic :: iso_fortran_env
            real(real64):: random_func
        end function
    end interface
    private
    public:: random_gaussian, random_mt, random_mcm
    public:: init_mcm
    real(real64),parameter:: pi2 = acos(-1d0)*2d0
contains
    function random_gaussian(rand) result(ret)
        procedure(random_func):: rand
        real(real64):: ret
        ret=sqrt(-2d0*log(rand()))*cos(pi2*rand())
    end function
end module