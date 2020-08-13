module graph_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type node
        integer(int32),allocatable:: to(:)
        integer(int32):: l
    contains
        procedure::  init => node_init
        procedure:: regist => node_regist
    end type


    type graph
        type(node),allocatable:: n(:)
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
            call g%n(i)%init()
        end do
    end function


    subroutine graph_regist(g,x,y)
        class(graph):: g
        integer(int32):: x,y

        call g%n(x)%regist(y)
        call g%n(y)%regist(x)
    end subroutine


    function graph_elem_num(g,i) result(ret)
        class(graph):: g
        integer(int32):: i,ret

        ret = g%n(i)%l
    end function


    function graph_elem_val(g,i,j) result(ret)
        class(graph):: g
        integer(int32):: i,j,ret

        ret = g%n(i)%to(j)
    end function


    subroutine node_init(n)
        class(node):: n
        allocate(n%to(1))

        n%l = 0
    end subroutine


    subroutine node_regist(n,m)
        class(node):: n
        integer(int32):: m

        if (n%l+1 > size(n%to)) call add_(n)
        n%l=n%l+1
        n%to(n%l) = m
    end subroutine


    subroutine add_(n)
        type(node):: n
        integer(int32),allocatable:: tmp(:)
        integer(int32):: l

        l = size(n%to)
        allocate(tmp(l))
        tmp(:) = n%to(:)
        deallocate(n%to)
        allocate(n%to(l*2))
        n%to(1:l) = tmp(:)
        deallocate(tmp)
    end subroutine
end module