module union_find_mod
    use,intrinsic :: iso_fortran_env
    private
    type,public:: union_find
        integer(int64),allocatable,private:: parent(:)
    contains
        procedure:: root => uf_root
        procedure:: is_family => uf_is_family
        procedure:: unite => uf_unite
        procedure:: size => uf_size
    end type

    interface union_find
        module procedure:: uf_init
    end interface
contains
    function uf_init(n) result(uf)
        type(union_find):: uf
        integer(int64):: n,i

        allocate(uf%parent, source=[(-1_int64,i=1_int64,n)])
    end function

    recursive function uf_root(uf,x) result(ret)
        class(union_find):: uf
        integer(int64):: x,ret

        if (uf%parent(x) < 0) then
            ret = x
        else
            uf%parent(x) = uf%root(uf%parent(x))
            ret = uf%parent(x)
        end if
    end function

    function uf_is_family(uf,a,b)
        class(union_Find):: uf
        integer(int64),intent(in):: a,b
        logical:: uf_is_family

        uf_is_family = uf%root(a) == uf%root(b)
    end function

    subroutine uf_unite(uf,a,b)
        class(union_find):: uf
        integer(int64),intent(in):: a,b
        integer(int64):: ra, rb

        ra = uf%root(a); rb = uf%root(b)
        if (ra == rb) return
        if (uf%size(ra) < uf%size(rb)) call swap()
        uf%parent(ra) = uf%parent(ra) + uf%parent(rb)
        uf%parent(rb) = ra
        contains
            subroutine swap()
                ra = xor(ra,rb)
                rb = xor(ra,rb)
                ra = xor(ra,rb)
            end subroutine
    end subroutine

    function uf_size(uf,x)
        class(union_find):: uf
        integer(int64),intent(in):: x
        integer(int64):: uf_size

        uf_size = -uf%parent(uf%root(x))
    end function