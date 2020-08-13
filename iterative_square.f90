function iterative_square(a,n,md) result(ret)
    use,intrinsic :: iso_fortran_env
    integer(int64):: a,n,i,md,mul
    integer(int64):: ret

    ret = 1
    mul = a
    do i=1,bit_size(n)
        if (btest(n,i-1)) ret = mod(ret*mul,md)
        mul=mod(mul*mul,md)
    end do
end function