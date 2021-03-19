module math_mod
    ! This module include
    ! gcd, lcm
    ! extgcd
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: byte = int64
contains
    function lcm(a, b) result(ret)
        integer(byte),intent(in):: a,b
        integer(byte):: ret

        ret = a*b/gcd(a,b)
    end function


    recursive function gcd(a, b) result(ret)
        integer(byte),intent(in):: a,b
        integer(byte):: ret

        if (b == 0_byte) then
            ret = a
        else
            ret = gcd(b, mod(a,b))
        end if
    end function


    recursive function extgcd(a, b, x, y) result(ret)
        ! solve:: ax + by = gcd(a,b) 
        ! input:: a, b
        ! output::  x, y, gcd(a,b)
        integer(byte),value:: a,b
        integer(byte),intent(out):: x,y
        integer(byte):: ret ! gcd(a,b)

        if (b==0_byte) then
            ret = a
            x = 1_byte
            y = 0_byte
        else
            ret = extgcd(b, mod(a,b), y, x)
            y = y - a/b * x
        end if
    end function

    recursive function mod_inv(a,m) result(ret)
        ! solve:: mod(ax, m) = 1
        !      => ax + my = 1
        ! input:: a,m
        ! output:: x <- 逆元
        integer(byte),intent(in):: a,m
        integer(byte):: ret, gcd_ma, x, y

        gcd_ma = extgcd(a,m,x,y)
        
        if (gcd_ma /= 1_byte) then
            ret = -1_byte
        else
            ret = modulo(x,m)
        end if
    end function


    function chineserem(b, m, md) result(ret)
        ! solve:: mod(b_1*k_1, m_1) = x
        !          :
        !         mod(b_n*k_n, m_n) = x を満たす最小のx
        ! input:: b(1:n), m(1:n)
        ! output:: x%md
        integer(byte),allocatable:: b(:),m(:)
        integer(byte):: md
        integer(byte),allocatable:: x0(:), mmul(:)
        integer(byte):: ret, i, j, g, gi, gj
        integer(byte):: t

        do i=1_byte,size(b)
            do j=1_byte, i-1_byte
                g = gcd(m(i),m(j))
                if (mod(b(i)-b(j), g) /= 0_byte) then
                    ret = -1_byte
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
        allocate(x0(size(m)), source=0_byte)
        allocate(mmul(size(m)), source=1_byte)

        do i=1_byte,size(b)
            t = modulo((b(i)-x0(i)) * mod_inv(mmul(i), m(i)), m(i))
            do j=i+1,size(m)
                x0(j) = modulo(x0(j) + t * mmul(j), m(j))
                mmul(j) = modulo(mmul(j)*m(i), m(j))
            end do
        end do
        ret = modulo(x0(size(x0)), md)
    end function
end module