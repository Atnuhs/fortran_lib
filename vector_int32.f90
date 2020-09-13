module vector_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: vec_to_array, vec_size
    type,public:: vector_int32
        integer(int32),allocatable:: array(:)
        integer(int32):: l
    contains
        procedure:: push_back=>vec_push_back, insert=>vec_insert
        procedure:: pop_back=>vec_pop_back, pop=>vec_pop, erase => vec_erase
        procedure:: at=>vec_at, back=>vec_back, head=>vec_head
    end type

    interface vector_int32
        module procedure vector_init
    end interface
contains
    function vector_init() result(vec)
        type(vector_int32):: vec
        vec%l=0
        allocate(vec%array(1))
    end function


    function vec_size(vec) result(ret)
        type(vector_int32):: vec
        integer(int32):: ret

        ret = vec%l
    end function


    function vec_back(vec) result(ret)
        class(vector_int32):: vec
        integer(int32):: ret
        
        ret = vec%at(vec%l)
    end function


    function vec_head(vec) result(ret)
        class(vector_int32):: vec
        integer(int32):: ret
        
        ret = vec%at(1)
    end function


    function vec_at(vec,i) result(ret)
        class(vector_int32):: vec
        integer(int32):: i,ret
        
        ret = vec%array(i)
    end function


    subroutine vec_push_back(vec, v)
        class(vector_int32):: vec
        integer(int32):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    subroutine vec_insert(vec,i,v)
        class(vector_int32):: vec
        integer(int32)::i, v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(i+1:vec%l+1) = vec%array(i:vec%l)
        vec%array(i) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector_int32):: vec
        integer(int32):: ret

        ret = vec%back()
        vec%l=vec%l-1
        call check_allocation_size(vec)
    end function


    function vec_pop(vec,i) result(ret)
        class(vector_int32):: vec
        integer(int32):: i,ret

        ret = vec%at(i)
        vec%l=vec%l-1
        vec%array(i:vec%l) = vec%array(i+1:vec%l+1)
        call check_allocation_size(vec)
    end function


    subroutine vec_erase(vec,i)
        class(vector_int32):: vec
        integer(int32):: i,dmp

        dmp = vec%pop(i)
    end subroutine


    subroutine check_allocation_size(vec)
        type(vector_int32):: vec
        integer(int32):: len_alloc

        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) then
            call vec_append_array(vec,1,len_alloc)
        else if (vec%l <= len_alloc/2) then
            call vec_reduce_array(vec,1,len_alloc)
        end if
    end subroutine


    subroutine vec_append_array(vec,l,r)
        type(vector_int32):: vec
        integer(int32):: l,r
        integer(int32),allocatable:: tmp(:)
        
        allocate(tmp(l:2*r))
        tmp(l:r) = vec%array(l:r)
        call move_alloc(tmp, vec%array)
    end subroutine


    subroutine vec_reduce_array(vec,l,r)
        type(vector_int32):: vec
        integer(int32):: l,r
        integer(int32),allocatable:: tmp(:)
        
        allocate(tmp(l:r/2))
        tmp(l:r/2) = vec%array(l:r/2)
        call move_alloc(tmp, vec%array)
    end subroutine


    function vec_to_array(vec) result(ret)
        type(vector_int32):: vec
        integer(int32):: ret(1:vec%l)
        ret = vec%array(1:vec%l)
    end function
end module