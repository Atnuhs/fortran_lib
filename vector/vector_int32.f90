! fenwic treeでバグらない要注意

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
        procedure:: to_array => vec_to_array, size => vec_size
        procedure:: update => vec_update
    end type
    interface vector_int32
        module procedure vec_init_from_len, vec_init_from_array
    end interface
    contains
    function vec_init_from_len(n,ini) result(ret)
        type(vector_int32):: ret
        integer(int32):: n
        integer(int32),optional:: ini
        integer(int32):: x

        x=0
        if (present(ini)) x=ini
        allocate(ret%array(n), source=x)
        ret%l = n
    end function


    function vec_init_from_array(ar) result(ret)
        type(vector_int32):: ret
        integer(int32):: ar(:)

        allocate(ret%array, source=ar)
        ret%l = size(ar)
    end function
    pure function vec_size(vec) result(ret)
        class(vector_int32),intent(in):: vec
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


    subroutine vec_update(vec, i, x)
        class(vector_int32),intent(inout):: vec
        integer(int32),intent(in):: i,x

        call check_array_allocation(vec)
        vec%array(i) = x
    end subroutine


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
        
        allocate(tmp(l:r/2))
        tmp(l:r/2) = vec%array(l:r/2)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine check_allocation_size(vec)
        type(vector_int32),intent(inout):: vec
        integer(int32):: len_alloc

        call check_array_allocation(vec)
        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) then
            call vec_append_array(vec,1,len_alloc)
        else if (vec%l <= len_alloc/2) then
            call vec_reduce_array(vec,1,len_alloc)
        end if
    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector_int32),intent(inout):: vec
        integer(int32),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    pure subroutine vec_insert(vec,i,v)
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
        class(vector_int32),intent(inout):: vec
        integer(int32):: ret(1:vec%l)

        call check_array_allocation(vec)
        ret = vec%array(1:vec%l)
    end function
end module