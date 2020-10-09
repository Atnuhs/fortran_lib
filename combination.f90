module combination_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    public
    integer(int64), allocatable, private:: frac(:), ifrac(:)
contains
    subroutine create_fraction(n,md)
        integer(int64),intent(in):: n, md
        integer(int64):: i

        allocate(frac(0:n), ifrac(0:n))
        frac(0) = 1; ifrac(0) = 1
        do i=1,n
            frac(i) = mod(frac(i-1)*i, md)
            ifrac(i) = inv(frac(i),md)
        end do
    end subroutine


    function comb(n,p,md) result(ret)
        integer(int32):: n,p,md
        integer(int64):: ret
        
        ret = mod(mod(frac(n)*ifrac(p),md)*ifrac(n-p),md)
    end function


    function inv(x,md) result(ret)
        integer(int64),intent(in):: x,md
        integer(int64):: ret

        ret = mod_pow(x,md-2,md)
    end function


    function mod_pow(base,exponent,md) result(ret)
        use,intrinsic :: iso_fortran_env
        integer(int64),intent(in):: md
        integer(int64),value:: base, exponent
        integer(int64):: ret

        ret = 1
        do while(exponent > 0)
            if (btest(exponent, 0)) ret=mod(ret*base,md)
            base=mod(base*base,md)
            exponent=rshift(exponent,1)
        end do
    end function
end module