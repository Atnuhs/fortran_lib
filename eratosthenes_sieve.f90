! fenwic treeでバグらない要注意

module vector_int64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: vec_to_array, vec_size
    type,public:: vector_int64
        integer(int64),allocatable:: array(:)
        integer(int64),private:: l=0
    contains
        procedure:: push_back=>vec_push_back, insert=>vec_insert
        procedure:: pop_back=>vec_pop_back, pop=>vec_pop, erase => vec_erase
        procedure:: at=>vec_at, back=>vec_back, head=>vec_head
        procedure:: to_array => vec_to_array, size => vec_size
        procedure:: update => vec_update
    end type

    interface vector_int64
        module procedure vec_init_from_len, vec_init_from_array
    end interface
contains
    function vec_init_from_len(n,ini) result(ret)
        type(vector_int64):: ret
        integer(int64):: n
        integer(int64),optional:: ini
        integer(int64):: x

        x=0
        if (present(ini)) x=ini
        allocate(ret%array(n), source=x)
        ret%l=n
    end function


    function vec_init_from_array(ar) result(ret)
        type(vector_int64):: ret
        integer(int64):: ar(:)

        allocate(ret%array, source=ar)
        ret%l = size(ar)
    end function


    pure function vec_size(vec) result(ret)
        class(vector_int64),intent(in):: vec
        integer(int64):: ret

        ret = vec%l
    end function


    pure subroutine check_array_allocation(vec)
        type(vector_int64),intent(inout):: vec

        if (.not. allocated(vec%array)) allocate(vec%array(1))
    end subroutine


    function vec_at(vec,i) result(ret)
        class(vector_int64),intent(inout):: vec
        integer(int64):: i,ret

        call check_array_allocation(vec)
        ret = vec%array(i)
    end function


    subroutine vec_update(vec, i, x)
        class(vector_int64),intent(inout):: vec
        integer(int64),intent(in):: i,x

        call check_array_allocation(vec)
        vec%array(i) = x
    end subroutine


    function vec_back(vec) result(ret)
        class(vector_int64),intent(inout):: vec
        integer(int64):: ret
        
        ret = vec%at(vec%l)
    end function


    function vec_head(vec) result(ret)
        class(vector_int64),intent(inout):: vec
        integer(int64):: ret
        
        ret = vec%at(1_int64)
    end function


    pure subroutine vec_append_array(vec,l,r)
        type(vector_int64),intent(inout):: vec
        integer(int64),intent(in):: l,r
        integer(int64),allocatable:: tmp(:)
        
        allocate(tmp(l:2*r))
        tmp(l:r) = vec%array(l:r)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine vec_reduce_array(vec,l,r)
        type(vector_int64),intent(inout):: vec
        integer(int64),intent(in):: l,r
        integer(int64),allocatable:: tmp(:)
        
        allocate(tmp(l:r/2))
        tmp(l:r/2) = vec%array(l:r/2)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine check_allocation_size(vec)
        type(vector_int64),intent(inout):: vec
        integer(int64):: len_alloc

        call check_array_allocation(vec)
        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) then
            call vec_append_array(vec,1_int64,len_alloc)
        else if (vec%l <= len_alloc/2) then
            call vec_reduce_array(vec,1_int64,len_alloc)
        end if
    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector_int64),intent(inout):: vec
        integer(int64),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    pure subroutine vec_insert(vec,i,v)
        class(vector_int64),intent(inout):: vec
        integer(int64),intent(in)::i, v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(i+1:vec%l+1) = vec%array(i:vec%l)
        vec%array(i) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector_int64),intent(inout):: vec
        integer(int64):: ret

        ret = vec%back()
        vec%l=vec%l-1
        call check_allocation_size(vec)
    end function


    function vec_pop(vec,i) result(ret)
        class(vector_int64),intent(inout):: vec
        integer(int64),intent(in):: i
        integer(int64):: ret

        ret = vec%at(i)
        vec%l=vec%l-1
        vec%array(i:vec%l) = vec%array(i+1:vec%l+1)
        call check_allocation_size(vec)
    end function


    subroutine vec_erase(vec,i)
        class(vector_int64):: vec
        integer(int64),intent(in):: i
        integer(int64):: dmp

        dmp = vec%pop(i)
    end subroutine


    function vec_to_array(vec) result(ret)
        class(vector_int64),intent(inout):: vec
        integer(int64):: ret(1:vec%l)

        call check_array_allocation(vec)
        ret = vec%array(1:vec%l)
    end function
end module


module eratosthenes_sieve_mod
    use,intrinsic :: iso_fortran_env
    use vector_int64_mod
    implicit none
    public
    type(vector_int64):: f, primes

contains
    subroutine sieve(n)
        integer(int64),intent(in):: n
        integer(int64):: i,j
        
        f = vector_int64(n)
        do i=2,n
            if (f%at(i)/=0) cycle
            call primes%push_back(i)
            call f%update(i,i)
            j=i*i
            do while(j <= n)
                if (f%at(j)==0) call f%update(j,i)
                j=j+i
            end do
        end do
    end subroutine


    function factor_vec(x) result(ret)
        integer(int64),value:: x
        type(vector_int64):: ret

        do while(x/=1)
            call ret%push_back(f%at(x))
            x=x / f%at(x)
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

        ret = f%at(x) == x
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


program main
    use,intrinsic :: iso_fortran_env
    use eratosthenes_sieve_mod
    use vector_int64_mod
    implicit none
    integer(int64):: i,n
    type(vector_int64):: v
    integer(int64), allocatable:: ff(:,:)

    read*, n
    call sieve(n)
    print'(*(i0,1x))', primes%to_array()
    read*, n
    ff = factor(n)
    do i=1,ubound(ff,2)
        print'(*(i0,1x))', ff(:,i)
    end do

    
end program main