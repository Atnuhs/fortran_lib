module vector_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: vec_to_array, vec_size
    type,public:: vector_int32
        integer(int32),allocatable:: array(:)
        integer(int32),private:: l=0
    contains
        procedure:: push_back=>vec_push_back, insert=>vec_insert
        procedure:: pop_back=>vec_pop_back, pop=>vec_pop, erase => vec_erase
        procedure:: at=>vec_at, back=>vec_back, head=>vec_head
    end type
contains
    pure function vec_size(vec) result(ret)
        type(vector_int32),intent(in):: vec
        integer(int32):: ret

        ret = vec%l
    end function


    pure subroutine check_array_allocation(vec)
        type(vector_int32),intent(inout):: vec

        if (.not. allocated(vec%array)) allocate(vec%array(1))
    end subroutine


    function vec_at(vec,i) result(ret)
        class(vector_int32),intent(inout):: vec
        integer(int32):: i,ret

        call check_array_allocation(vec)
        ret = vec%array(i)
    end function


    function vec_back(vec) result(ret)
        class(vector_int32),intent(inout):: vec
        integer(int32):: ret
        
        ret = vec%at(vec%l)
    end function


    function vec_head(vec) result(ret)
        class(vector_int32),intent(inout):: vec
        integer(int32):: ret
        
        ret = vec%at(1)
    end function


    pure subroutine vec_append_array(vec,l,r)
        type(vector_int32),intent(inout):: vec
        integer(int32),intent(in):: l,r
        integer(int32),allocatable:: tmp(:)
        
        allocate(tmp(l:2*r))
        tmp(l:r) = vec%array(l:r)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine vec_reduce_array(vec,l,r)
        type(vector_int32),intent(inout):: vec
        integer(int32),intent(in):: l,r
        integer(int32),allocatable:: tmp(:)
        integer(int32):: nr

        nr = (r/4)*3
        allocate(tmp(l:nr))
        tmp(l:nr) = vec%array(l:nr)
        call move_alloc(tmp, vec%array)
    end subroutine


    subroutine check_allocation_size(vec)
        type(vector_int32),intent(inout):: vec
        integer(int32):: len_alloc

        call check_array_allocation(vec)
        len_alloc = size(vec%array)

        if (vec%l > len_alloc) then
            call vec_append_array(vec,1,len_alloc)
        else if (vec%l <= len_alloc/2) then
            call vec_reduce_array(vec,1,len_alloc)
        end if
    end subroutine


    subroutine vec_push_back(vec, v)
        class(vector_int32),intent(inout):: vec
        integer(int32),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    subroutine vec_insert(vec,i,v)
        class(vector_int32),intent(inout):: vec
        integer(int32),intent(in)::i, v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(i+1:vec%l+1) = vec%array(i:vec%l)
        vec%array(i) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector_int32),intent(inout):: vec
        integer(int32):: ret

        ret = vec%back()
        vec%l=vec%l-1
        call check_allocation_size(vec)
    end function


    function vec_pop(vec,i) result(ret)
        class(vector_int32),intent(inout):: vec
        integer(int32),intent(in):: i
        integer(int32):: ret

        ret = vec%at(i)
        vec%l=vec%l-1
        vec%array(i:vec%l) = vec%array(i+1:vec%l+1)
        call check_allocation_size(vec)
    end function


    subroutine vec_erase(vec,i)
        class(vector_int32):: vec
        integer(int32),intent(in):: i
        integer(int32):: dmp

        dmp = vec%pop(i)
    end subroutine


    function vec_to_array(vec) result(ret)
        type(vector_int32),intent(inout):: vec
        integer(int32):: ret(1:vec%l)

        call check_array_allocation(vec)
        ret = vec%array(1:vec%l)
    end function
end module


module fenwic_tree_mod
    use vector_int32_mod
    use,intrinsic :: iso_fortran_env
    type,public:: fenwic
        type(vector_int32):: vec
    contains
        procedure:: push => fw_push
        procedure:: prefix_sum => fw_prefix_sum
        procedure:: sec_sum => fw_sum
        procedure:: add => fw_add
    end type
contains
    function lsb(x) result(ret)
        integer(int32):: x,ret

        ret = iand(x,-x)
    end function


    function fw_size(fw) result(ret)
        type(fenwic),intent(in):: fw
        integer(int32):: ret
        
        ret = vec_size(fw%vec)
    end function

    subroutine fw_push(fw,x)
        class(fenwic),intent(inout):: fw
        integer(int32),intent(in):: x
        integer(int32):: i,bx,k,n

        i=1; bx=x; n=fw_size(fw)+1; k=lsb(n)
        do while(i /= k)
            bx=bx+fw%vec%at(n-i)
            i=i*2
        end do
        call fw%vec%push_back(bx)
    end subroutine


    function fw_prefix_sum(fw,i) result(ret)
        class(fenwic):: fw
        integer(int32),intent(in):: i
        integer(int32):: bi,ret

        ret=0; bi=i
        do while(bi>0)
            ret=ret+fw%vec%at(bi)
            bi=bi-lsb(bi)
        end do
    end function


    function fw_sum(fw,l,r) result(ret)
        class(fenwic):: fw
        integer(int32),intent(in):: l,r
        integer(int32):: ret

        ret = fw%prefix_sum(r) - fw%prefix_sum(l-1)
    end function


    subroutine fw_add(fw,i,x)
        class(fenwic):: fw
        integer(int32),intent(in):: i,x
        integer(int32):: bi

        bi = i
        do while (bi <= fw_size(fw))
            fw%vec%array(bi) = fw%vec%array(bi) + x
            bi=bi+lsb(bi)
        end do
    end subroutine


    subroutine fw_from_array(fw,ar)
        type(fenwic):: fw
        integer(int32):: ar(:)
        integer(int32):: n,i

        n=size(ar)
        do i=1,n
            call fw_push(fw,ar(i))
        end do
    end subroutine


    subroutine fw_debug_print(fw)
        type(fenwic):: fw

        print'(*(i0,1x))', vec_to_array(fw%vec)
    end subroutine
end module


program main
    use,intrinsic :: iso_fortran_env
    use fenwic_tree_mod
    implicit none
    type(fenwic):: fw
    integer(int32):: i,n

    n = 6
    do i=0,n
        call fw_push(fw,10**i)
        call fw_debug_print(fw)
    end do
end program main