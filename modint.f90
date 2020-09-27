module mint_mod
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) :: md = 1000000007_8
    type mint
        integer(int64), private :: val = 0_8
    contains
        procedure:: to_int => mint_to_int64
    end type
    interface operator( + )
        module procedure :: add
    end interface
    interface operator( - )
        module procedure :: sub
    end interface
    interface operator( * )
        module procedure :: mul
    end interface
    interface operator( / )
        module procedure :: div
    end interface
    interface operator( ** )
        module procedure :: pow
    end interface
    interface assignment( = )
        module procedure:: asgn
    end interface
    interface dot_product
        module procedure :: dot_prod
    end interface dot_product
    interface matmul
        module procedure :: matmul1, matmul2, matmul3
    end interface matmul
contains
    pure elemental function to_int64(num) result(ret)
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

        ret = to_int64(mx)
    end function

    pure elemental function to_modint(num) result(ret)
        class(*), intent(in):: num
        type(mint):: ret

        ret%val = to_int64(num)
    end function


    pure elemental subroutine asgn(xm,y)
        type(mint), intent(inout) :: xm
        class(*), intent(in) :: y
        
        xm%val = to_int64(y)
    end subroutine


    pure elemental function add(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret

        ret%val = modulo(xm%val + to_int64(y), md)
    end function


    pure elemental function sub(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret

        ret%val = modulo(xm%val - to_int64(y), md)
    end function



    pure elemental function mul(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint) ret

        ret%val = modulo(xm%val * to_int64(y), md)
    end function


    pure elemental function div(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret

        ret%val = xm%val * to_int64(inv(to_modint(y)))
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


    pure elemental function pow(xm,y) result(ret)
        class(mint), intent(in) :: xm
        class(*), intent(in) :: y
        type(mint):: ret
        integer(int64) :: n
        type(mint) :: i

        ret%val = 1_8
        i%val = xm%val
        n = to_int64(y)
        do while (n > 0_8)
            if (btest(n,0)) ret%val = to_int64(mul(ret,i))
            i%val = to_int64(mul(i,i))
            n = rshift(n,1)
        end do
    end function

    pure type(mint) function dot_prod(x,y) result(ret)
        type(mint), intent(in) :: x(:), y(:)
        integer(int64):: i
        if (size(x,1) /= size(y,1)) i = to_int64('')
        do i = 1, size(x,1)
            call asgn(ret,add(ret,mul(x(i),y(i))))
        end do
    end function


    pure function matmul1(x,y) result(ret)
        type(mint), intent(in) :: x(:,:), y(:)
        type(mint) :: ret(size(x,1))
        integer :: i
        do i = 1, size(x,1)
            call asgn(ret(i),dot_prod(x(i,:),y))
        end do
    end function


    pure function matmul2(x,y) result(ret)
        type(mint), intent(in) :: x(:), y(:,:)
        type(mint) :: ret(size(y,2))
        integer :: i
        do i = 1, size(y,2)
            call asgn(ret(i),dot_prod(x,y(:,i)))
        end do
    end function


    pure function matmul3(x,y) result(ret)
        type(mint), intent(in) :: x(:,:), y(:,:)
        type(mint) :: ret(size(x,1),size(y,2))
        integer :: i
        do i = 1, size(x,1)
            call asgn(ret(i,:),matmul2(x(i,:),y))
        end do
    end function
end module mint_mod