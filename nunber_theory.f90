module number_theory_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: fc_init,comb,inv,mod_pow
    public:: lcm, gcd
    integer(int64):: md,n
    integer(int64), allocatable:: frac(:), ifrac(:)

contains
    subroutine fc_init(input_n, input_md)
        integer(int64),intent(in):: input_n, input_md
        integer(int64):: i

        n = input_n; md = input_md
        allocate(frac(0:n), ifrac(0:n))
        frac(0) = 1; ifrac(0) = 1
        do i=1,n
            frac(i) = mod(frac(i-1)*i, md)
            ifrac(i) = inv(frac(i))
        end do
    end subroutine


    function comb(n,p) result(ret)
        integer(int64):: n,p
        integer(int64):: ret
        ret = mod(mod(frac(n)*ifrac(p),md)*ifrac(n-p),md)
    end function


    function inv(x) result(ret)
        integer(int64),intent(in):: x
        integer(int64):: ret
        ret = mod_pow(x,md-2)
    end function


    function mod_pow(base,exponent) result(ret)
        use,intrinsic :: iso_fortran_env
        integer(int64),intent(in):: base,exponent
        integer(int64):: ret,a,x

        ret = 1
        a = base
        x = exponent
        do while(x > 0)
            if (btest(x,0)) ret = mod(ret*a,md)
            a=mod(a*a,md)
            x=rshift(x,1)
        end do
    end function


    function lcm(x,y) result(ret)
        integer(int32):: x,y,ret
        ret=x*y/gcd(x,y)
    end function


    recursive function gcd(x,y) result(ret)
        integer(int32):: x,y,ret
    
        if (mod(x,y) == 0) then
            ret = y
            return
        end if
        ret = gcd(y,mod(x,y))
    end function
end module