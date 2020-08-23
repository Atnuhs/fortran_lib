module vector_int32_mod
    use,intrinsic :: iso_fortran_env
    type vector
        integer(int32),allocatable:: array(:)
        integer(int32):: l
    contains
        procedure :: push_back => vector_regist
    end type

    interface vector
        module procedure vector_init
    end interface
contains
    function vector_init() result(vec)
        type(vector):: vec
        allocate(vec%array(1))
        vec%l = 0
    end function

    subroutine vector_regist(vec, v)
        class(vector):: vec
        integer(int32):: v

        if (vec%l+1 > size(vec%array)) call add_(vec)
        vec%l=vec%l+1
        vec%array(vec%l) = v
    end subroutine


    subroutine add_(vec)
        type(vector):: vec
        integer(int32),allocatable:: tmp(:)
        integer(int32):: l

        l = size(vec%array)
        allocate(tmp(l))
        tmp(:) = vec%array(:)
        deallocate(vec%array)
        allocate(vec%array(l*2))
        vec%array(1:l) = tmp(:)
        deallocate(tmp)
    end subroutine
end module