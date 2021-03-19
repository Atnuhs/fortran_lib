module eratosthenes_sieve_mod
    use,intrinsic :: iso_fortran_env
    use vector_mod
    implicit none
    integer(int32),private,parameter:: prec=int32
    public
    type(vector):: sieve_vec, primes

contains
    subroutine sieve(n)
        integer(prec),intent(in):: n
        integer(prec):: i,j
        
        sieve_vec = vector(n)
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
        integer(prec),value:: x
        type(vector):: ret

        call ret%push_back(1_prec)
        do while(x/=1)
            call ret%push_back(sieve_vec%at(x))
            x=x / sieve_vec%at(x)
        end do
    end function


    function factor(x) result(ret)
        integer(prec),intent(in):: x
        integer(prec):: i,n
        type(vector):: fv
        integer(prec), allocatable:: tmp(:,:), ret(:,:)

        fv = factor_vec(x)
        n=1
        allocate(tmp(2,fv%size()),source=0_prec)
        tmp(:,1) = [fv%at(1_prec),1_prec]
        do i=2,fv%size()
            if (fv%at(i) == fv%at(i-1)) then
                tmp(2,n) = tmp(2,n)+1
            else
                n=n+1
                tmp(:,n) = [fv%at(i), 1_prec]
            end if
        end do
        ret = tmp(:,1:n)
    end function


    function is_prime(x) result(ret)
        integer(prec),intent(in):: x
        logical:: ret

        ret = sieve_vec%at(x) == x
    end function
end module