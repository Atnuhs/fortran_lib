! contain math_mod
    ! mod_pow(b,exp,m)
        ! solve:: ret = mod( b**exp, m )繰り返し二乗法で高速化
        ! input:: b, exp, m
        ! output:: ret
    ! mod_inv(a,m)
        ! solve:: mod(ax, m) = 1
        !      => ax + my = 1
        ! input:: a,m
        ! output:: x <- inverse of a at mod m
    ! lcm(a,b)
        ! solve:: ret = lcm(a,b)
        ! input:: a, b
        ! output:: ret
    ! gcd(a,b)
        ! solve:: ret = gcd(a,b)
        ! input:: a, b
        ! output:: ret
    ! extgcd(a,b,x,y)
        ! solve:: ax + by = gcd(a,b) (拡張ユークリッドの互除法)
        ! input:: a, b
        ! output::  x, y, gcd(a,b)
    ! chineserem(b,m,md) 
        ! solve:: mod(b_1*k_1, m_1) = x (-> b_1の倍数をm_1で割ったあまりがx)
        !          :
        !         mod(b_n*k_n, m_n) = x を満たす最小のx
        ! input:: b(1:n), m(1:n)
        ! output:: x (< md)
! combination_mod
    ! make_fraction(n,md)
        ! solve:: generate frac, ifrac (frac(i) = i! % md, ifrac(i) = mod_inv(i!))
        ! input:: n, md
        ! output:: none
    ! perm(n,p,md)
        ! solve:: ret = nPp % md (n個のものをp個選んで並べる並べ方。をmdで割った値)
        ! input:: n, p, md
        ! output:: ret
    ! comb(n,p,md)
        ! solve:: ret = nCp % md (n個のものをp個選ぶ選び方。をmdで割った値)
        ! input:: n, p, md
        ! output:: ret





module math_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function mod_pow(base, exponent, m) result(ret)
        integer(int64),intent(in):: m
        integer(int64),value:: base, exponent
        integer(int64):: ret

        ret = 1
        do while(exponent > 0)
            if (btest(exponent, 0)) ret=mod(ret*base,m)
            base=mod(base*base,m)
            exponent=shiftr(exponent,1)
        end do
    end function


    recursive function mod_inv(a,m) result(ret)
        integer(int64),intent(in):: a,m
        integer(int64):: ret, gcd_ma, x, y

        gcd_ma = extgcd(a,m,x,y)
        
        if (gcd_ma /= 1_int64) then
            ret = -1_int64
        else
            ret = modulo(x,m)
        end if
    end function


    function lcm(a, b) result(ret)
        integer(int64),intent(in):: a,b
        integer(int64):: ret

        ret = a/gcd(a,b)*b
    end function


    recursive function gcd(a, b) result(ret)
        integer(int64),intent(in):: a,b
        integer(int64):: ret

        if (b == 0_int64) then
            ret = a
        else
            ret = gcd(b, mod(a,b))
        end if
    end function


    recursive function extgcd(a, b, x, y) result(ret)
        integer(int64),value:: a,b
        integer(int64),intent(out):: x,y
        integer(int64):: ret ! gcd(a,b)

        if (b==0_int64) then
            ret = a
            x = 1_int64
            y = 0_int64
        else
            ret = extgcd(b, mod(a,b), y, x)
            y = y - a/b * x
        end if
    end function


    function chineserem(b, m, md) result(ret)
        integer(int64),allocatable:: b(:),m(:)
        integer(int64):: md
        integer(int64),allocatable:: x0(:), mmul(:)
        integer(int64):: ret, i, j, g, gi, gj
        integer(int64):: t

        do i=1_int64,size(b)
            do j=1_int64, i-1_int64
                g = gcd(m(i),m(j))
                if (mod(b(i)-b(j), g) /= 0_int64) then
                    ret = -1_int64
                    return
                end if
                m(i) = m(i) / g
                m(j) = m(j) / g
                gi = gcd(m(i),g)
                gj = g/gi
                do while(g /= 1)
                    g = gcd(gi,gj)
                    gi = gi*g
                    gj = gj/g
                end do
                m(i) = m(i)*gi
                m(j) = m(j)*gj
                b(i) = mod(b(i), m(i))
                b(j) = mod(b(j), m(j))
            end do
        end do

        m = [m,md]
        allocate(x0(size(m)), source=0_int64)
        allocate(mmul(size(m)), source=1_int64)

        do i=1_int64,size(b)
            t = modulo((b(i)-x0(i)) * mod_inv(mmul(i), m(i)), m(i))
            do j=i+1,size(m)
                x0(j) = modulo(x0(j) + t * mmul(j), m(j))
                mmul(j) = modulo(mmul(j)*m(i), m(j))
            end do
        end do
        ret = modulo(x0(size(x0)), md)
    end function
end module


module combination_mod
    use,intrinsic :: iso_fortran_env
    use math_mod
    implicit none
    public
    integer(int64), allocatable, private:: frac(:), ifrac(:)
contains
    subroutine make_fraction(n,md)
        integer(int64),intent(in):: n, md
        integer(int64):: i

        allocate(frac(0:n), ifrac(0:n))
        frac(0) = 1; ifrac(0) = 1
        do i=1,n
            frac(i) = mod(frac(i-1)*i, md)
            ifrac(i) = mod_inv(frac(i),md)
        end do
    end subroutine


    function perm(n,p,md) result(ret)
        integer(int64),intent(in):: n,p,md
        integer(int64):: ret
        
        ret = mod(frac(n)*ifrac(n-p), md)
    end function

    
    function comb(n,p,md) result(ret)
        integer(int64),intent(in):: n,p,md
        integer(int64):: ret
        
        ret = mod(mod(frac(n)*ifrac(p),md)*ifrac(n-p),md)
    end function
end module


module algorithm_mod 
    use math_mod
    use combination_mod
    implicit none
end module