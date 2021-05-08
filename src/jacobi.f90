module jacobi_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    real(real64),parameter:: pi = acos(-1d0)
    private
    public:: jacobi

contains
    subroutine  jacobi(a,n)
        integer(int32),intent(in):: n
        real(real64), intent(inout):: a(n,n)
        integer(int32):: iter,x,y,i
        real(real64):: theta

        do iter = 1,100
            call search_max(a,n,x,y)
            theta = calc_theta(a,n,x,y)
            call calc_new_a(a,n,x,y,theta)
            print*, "#",iter,x,y
            do i=1,n
                print'(*(f8.4))', a(:,i)
            end do
        end do
    end subroutine


    subroutine search_max(a,n,x,y)
        integer(int32),intent(in):: n
        real(real64), intent(in):: a(n,n)
        integer(int32),intent(out):: x,y
        real(real64):: axy
        integer(int32)::i,j

        axy = 0d0
        do i=1,n
            do j=1,n
                if (i==j) cycle
                if (abs(a(j,i)) > axy) then
                    axy = abs(a(j,i))
                    y=i; x=j
                end if
            end do
        end do
    end subroutine


    function calc_theta(a,n,x,y) result(theta)
        integer(int32),intent(in):: x,y,n
        real(real64),intent(in)::a(n,n)
        real(real64):: theta

        if (a(x,x) == a(y,y)) then
            theta = pi/4d0
        else
            theta = atan(-2*a(x,y)/(a(y,y)-a(x,x)))/2d0
        end if
    end function


    subroutine calc_new_a(a,n,x,y,theta)
        integer(int32),intent(in):: x,y,n
        real(real64),intent(inout):: a(n,n)
        real(real64),intent(in):: theta
        real(real64):: new_a(n,n), ct, st

        ct = cos(theta)
        st = sin(theta)

        new_a(:,:) = a(:,:)
        new_a(:,x) = ct*a(:,x) + st*a(:,y)
        new_a(:,y) = ct*a(:,y) - st*a(:,x)
        new_a(x,:) = ct*a(x,:) + st*a(y,:)
        new_a(y,:) = ct*a(y,:) - st*a(x,:)
        new_a(x,x) = st*(st*a(y,y)+ct*a(x,y)) + ct*(st*a(x,y) + ct*a(x,x))
        new_a(y,y) = ct*(ct*a(y,y)-st*a(x,y)) - st*(ct*a(x,y) - st*a(x,x))
        new_a(x,y) = st*(ct*a(y,y)-st*a(x,y)) + ct*(ct*a(x,y) - st*a(x,x))
        new_a(y,x) = st*(ct*a(y,y)-st*a(x,y)) + ct*(ct*a(x,y) - st*a(x,x))
        a(:,:) = new_a(:,:)
    end subroutine
end module