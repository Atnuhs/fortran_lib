module vector_int32_mod
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none
    private
    type,public:: vector
        integer(int32),allocatable:: array(:)
        integer(int32):: l=0
    contains
        procedure:: push_back=>vec_push_back
        procedure:: pop_back=>vec_pop_back
    end type
contains
    pure subroutine check_allocation_size(vec)
        type(vector),intent(inout):: vec
        integer(int32):: len_alloc

        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) call append_array(vec%array)
    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector),intent(inout):: vec
        integer(int32),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int32):: ret

        ret = vec%array(vec%l)
        vec%l=vec%l-1
    end function
end module



module vector_int64_mod
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none
    private
    type,public:: vector
        integer(int64),allocatable:: array(:)
        integer(int32):: l=0
    contains
        procedure:: push_back=>vec_push_back
        procedure:: pop_back=>vec_pop_back
    end type
contains
    pure subroutine check_allocation_size(vec)
        type(vector),intent(inout):: vec
        integer(int32):: len_alloc

        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) call append_array(vec%array)

    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector),intent(inout):: vec
        integer(int64),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int64):: ret

        ret = vec%array(vec%l)
        vec%l=vec%l-1
    end function
end module


module vector_real64_mod
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none
    private
    type,public:: vector
        real(real64),allocatable:: array(:)
        integer(int32):: l=0
    contains
        procedure:: push_back=>vec_push_back
        procedure:: pop_back=>vec_pop_back
    end type
contains
    pure subroutine check_allocation_size(vec)
        type(vector),intent(inout):: vec
        integer(int32):: len_alloc

        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) call append_array(vec%array)
    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector),intent(inout):: vec
        real(real64),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector),intent(inout):: vec
        real(real64):: ret

        ret = vec%array(vec%l)
        vec%l=vec%l-1
    end function
end module



module vector_mod
    use vector_int32_mod, vector_int32 => vector
    use vector_int64_mod, vector_int64 => vector
    use vector_real64_mod, vector_real64 => vector
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: vector_int32
    public:: vector_int64
    public:: vector_real64
end module