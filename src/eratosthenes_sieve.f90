module eratosthenes_sieve_mod
    use,intrinsic :: iso_fortran_env
    use vector_int64_mod
    implicit none
    public
    type(vector_int64):: sieve_vec, primes

contains
    subroutine sieve(n)
        integer(int64),intent(in):: n
        integer(int64):: i,j
        
        sieve_vec = vector_int64(n)
        do i=2,n
            if (sieve_vec%at(i)/=0) cycle
            call primes%push_back(i)
            call sieve_vec%update(i,i)
            j=i*i
            do while(j <= n)
                if (sieve_vec%at(j)==0) call sieve_vec%update(j,i)
                j=j+i
            end do
        end do
    end subroutine


    function factor_vec(x) result(ret)
        integer(int64),value:: x
        type(vector_int64):: ret

        call ret%push_back(1_int64)
        do while(x/=1)
            call ret%push_back(sieve_vec%at(x))
            x=x / sieve_vec%at(x)
        end do
    end function


    function factor(x) result(ret)
        integer(int64),intent(in):: x
        integer(int64):: i,n
        type(vector_int64):: fv
        integer(int64), allocatable:: tmp(:,:), ret(:,:)

        fv = factor_vec(x)
        n=1
        allocate(tmp(2,fv%size()),source=0_int64)
        tmp(:,1) = [fv%at(1_int64),1_int64]
        do i=2,fv%size()
            if (fv%at(i) == fv%at(i-1)) then
                tmp(2,n) = tmp(2,n)+1
            else
                n=n+1
                tmp(:,n) = [fv%at(i), 1_int64]
            end if
        end do
        ret = tmp(:,1:n)
    end function


    function is_prime(x) result(ret)
        integer(int64),intent(in):: x
        logical:: ret

        ret = sieve_vec%at(x) == x
    end function


    function lcm(x,y) result(ret)
        integer(int64),intent(in):: x,y
        integer(int64):: ret

        ret = x*y/gcd(x,y)
    end function


    recursive function gcd(x,y) result(ret)
        integer(int64),intent(in):: x,y
        integer(int64):: ret
    
        if (mod(x,y) == 0) then
            ret = y
            return
        end if
        ret = gcd(y,mod(x,y))
    end function
end module