! need vector_int32_mod
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