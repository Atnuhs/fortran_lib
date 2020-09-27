module lazy_segtree_mod
    type elem
    end type
    type lazy_segtree
        class(elem),allocatable:: ss(:)
        
    end type
end module

program main
    use,intrinsic :: iso_fortran_env
    use lazy_segtree_mod
    implicit none
    type, extends(elem):: s 
        integer(int32):: x
        integer(int32):: y
    end type
    
end program main