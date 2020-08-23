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


module graph_mod
    use,intrinsic :: iso_fortran_env
    use vector_int32_mod
    implicit none

    type graph
        type(vector),allocatable:: n(:)
    contains
        procedure:: regist => graph_regist
        procedure:: l => graph_elem_num
        procedure:: v => graph_elem_val
    end type
    interface graph
        module procedure graph_init
    end interface
    private
    public:: graph
contains
    function graph_init(n) result(g)
        type(graph):: g
        integer(int32),intent(in):: n
        integer(int32):: i

        allocate(g%n(n))
        do i=1,n
            g%n(i) = vector()
        end do
    end function


    subroutine graph_regist(g,x,y)
        class(graph):: g
        integer(int32):: x,y

        call g%n(x)%push_back(y)
        call g%n(y)%push_back(x)
    end subroutine


    function graph_elem_num(g,i) result(ret)
        class(graph):: g
        integer(int32):: i,ret

        ret = g%n(i)%l
    end function


    function graph_elem_val(g,i,j) result(ret)
        class(graph):: g
        integer(int32):: i,j,ret

        ret = g%n(i)%array(j)
    end function
end module