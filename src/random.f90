! use random_mod

! rnd()          => real64,         0 <= v < 1
! randint(n)     => int32 or int64, 0 <= v <= n
! randrange(l,r) => int32 or int64, l <= v <= r

module random01_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function random01() result(v)
        real(real64):: v

        call random_number(v)
    end function
end module


module random32
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        integer(int32),intent(in):: l,r
        integer(int32):: v

        v = int(rnd()*(r-l+1)) + l
    end function

    function randint(n) result(v)
        integer(int32),intent(in):: n
        integer(int32):: v

        v = randrange(0_int32, n)
    end function
end module


module random64
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        integer(int64),intent(in):: l,r
        integer(int64):: v

        v = int(rnd()*(r-l+1)) + l
    end function

    function randint(n) result(v)
        integer(int64),intent(in):: n
        integer(int64):: v

        v = randrange(0_int64, n)
    end function
end module


module random_mod
    use,intrinsic :: iso_fortran_env
    use random01_mod, only: rnd => random01
    use random32, rr32=>randrange, ri32=>randint
    use random64, rr64=>randrange, ri64=>randint
    
    interface randrange
        module procedure rr32, rr64
    end interface

    interface randint
        module procedure ri32, ri64
    end interface
end module