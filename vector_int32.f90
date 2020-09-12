module vector_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: vec_print, vec_size
    type,public:: vector_int32
        integer(int32),pointer:: array(:) => null()
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
        allocate(vec%array(1))
        vec%l = 0
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
        vec%array(i:vec%l) = [v,vec%array(i:vec%l-1)]
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

        if (vec%l > size(vec%array)) then
            vec%array => vec_append_array(vec)
        else if (vec%l <= size(vec%array)/2) then
            vec%array => vec_reduce_array(vec)
        end if
    end subroutine


    function vec_append_array(vec) result(ret)
        type(vector_int32):: vec
        integer(int32),pointer,dimension(:):: ret
        integer(int32):: l
        
        l=size(vec%array)*2
        allocate(ret(1:l), source=reshape(vec%array, [l], pad=[0]))
    end function


    function vec_reduce_array(vec) result(ret)
        type(vector_int32):: vec
        integer(int32),pointer,dimension(:):: ret
        integer(int32):: l

        l=size(vec%array)/2
        allocate(ret(1:l), source=vec%array(1:l))
    end function


    subroutine vec_print(vec)
        type(vector_int32):: vec
        
        print'(*(i0,1x))', vec%array(1:vec%l)
    end subroutine
end module