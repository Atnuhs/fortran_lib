module union_find_mod
    use,intrinsic :: iso_fortran_env
    private
    type,public:: union_find
        integer(int32),allocatable,private:: parent(:)
    contains
        procedure:: unite => uf_unite
        procedure:: root => uf_root
    end type

    interface union_find
        module procedure:: uf_init
    end interface
contains
    function uf_init(n) result(uf)
        type(union_find):: uf
        integer(int32):: n

        allocate(uf%parent, source=[(i,i=1,n)])
    end function


    recursive function uf_root(uf,x) result(ret)
        class(union_find):: uf
        integer(int32):: x,ret

        if (uf%parent(x) == x) then
            ret = x
        else
            uf%parent(x) = uf%root(uf%parent(x))
            ret = uf%parent(x)
        end if
    end function


    subroutine uf_unite(uf,a,b)
        class(union_find):: uf
        integer(int32),intent(in):: a,b
        integer(int32):: ra, rb

        ra = uf%root(a); rb = uf%root(b)
        if (ra == rb) return
        uf%parent(ra) = rb
    end subroutine
end module