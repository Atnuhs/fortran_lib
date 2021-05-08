module mint_mod
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    integer(int64) :: md = 1000000007_8
    type,public:: mint
        integer(int64), private :: val = 0_8
    contains
        procedure:: to_int => mint_to_int64
        procedure,private:: mi_add
        procedure,private:: mi_sub
        procedure,private:: mi_mul
        procedure,private:: mi_div
        procedure,private:: mi_pow
        procedure,private:: mi_asgn
        generic:: operator( + ) => mi_add
        generic:: operator( - ) => mi_sub
        generic:: operator( * ) => mi_mul
        generic:: operator( / ) => mi_div
        generic:: operator( ** ) => mi_pow
        generic:: assignment( = ) => mi_asgn
    end type

    interface mint
        module procedure:: mi_init
    end interface
contains
    pure elemental function mi_init(num) result(ret)
        class(*),intent(in):: num
        type(mint):: ret

        ret = to_mint(num)
    end function


    pure elemental function int_to_int64(num) result(ret)
        class(*), intent(in) :: num
        integer(int64):: ret

        select type (num)
            type is (mint)
                ret = num%val
            type is (integer(int64))
                ret = num
            type is (integer(int32))
                ret = int(num, int64)
            type is (integer(int16))
                ret = int(num, int64)
            type is (integer(int8))
                ret = int(num, int64)
            class default
                ret = 0_8
                ret = 1_8/ret
        end select
        ret=modulo(ret,md)
    end function


    pure elemental function mint_to_int64(mx) result(ret)
        class(mint),intent(in):: mx
        integer(int64):: ret

        ret = int_to_int64(mx)
    end function


    pure elemental function to_mint(num) result(ret)
        class(*), intent(in):: num
        type(mint):: ret

        ret%val = int_to_int64(num)
    end function


    pure elemental subroutine mi_asgn(xm,y)
        class(mint), intent(inout) :: xm
        class(*), intent(in) :: y
        
        xm%val = int_to_int64(y)
    end subroutine


    pure elemental function mi_add(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret

        ret%val = modulo(xm%val + int_to_int64(y), md)
    end function


    pure elemental function mi_sub(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret

        ret%val = modulo(xm%val - int_to_int64(y), md)
    end function



    pure elemental function mi_mul(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint) ret

        ret%val = modulo(xm%val * int_to_int64(y), md)
    end function


    pure elemental function mi_div(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret

        ret%val = xm%val * int_to_int64(inv(to_mint(y)))
    end function

    
    pure elemental function inv(xm) result(ret)
        class(mint), intent(in) :: xm
        type(mint):: ret
        integer(int64) :: a, b, c, n
        integer(int64) :: x, y, z, m

        a = xm%val
        b = md
        c = 0_8
        n = 1_8
        do while (b /= 0_8)
            x = b
            y = mod(a,b)
            z = n-a/b*c
            m = c
            a = x
            b = y
            c = z
            n = m
        end do
        ret%val = n
    end function


    pure elemental function mi_pow(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret
        integer(int64) :: n
        type(mint) :: i

        ret%val = 1_8
        i%val = xm%val
        n = int_to_int64(y)
        do while (n > 0_8)
            if (btest(n,0)) ret%val = int_to_int64(mi_mul(ret,i))
            i%val = int_to_int64(mi_mul(i,i))
            n = rshift(n,1)
        end do
    end function
end module mint_mod