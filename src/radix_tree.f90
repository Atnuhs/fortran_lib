module radix_tree_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: ia=ichar('a'), iz=ichar('z')

    type radix_tree_child
        type(radix_tree),pointer:: p => null()
    end type
    type radix_tree
        character(:),allocatable:: val
        type(radix_tree_child):: c(ia:iz)
        type(radix_tree),pointer:: parent => null()
    end type
contains
    function rt_init() result(rt)
        type(radix_tree),pointer:: rt

        allocate(rt)
        rt%val = ''
    end function

    subroutine search_longest_match(ca, cb, match_str, rest_ca, rest_cb)
        character(:),allocatable,intent(out):: match_str, rest_ca, rest_cb
        character(*),intent(in):: ca, cb
        integer(int32):: i, na, nb, n

        na = len_trim(ca)
        nb = len_trim(cb)
        n = min(na,nb)
        i=1

        do while(i <= n)
            if (ca(i:i) /= cb(i:i)) then
                match_str = ca(:i-1)
                rest_ca = ca(i:na)
                rest_cb = cb(i:nb)
                return
            end if
            i=i+1
        end do

        match_str = ca(1:n)
        rest_ca = ca(n+1:na)
        rest_cb = cb(n+1:nb)
    end subroutine


    subroutine insert(rt, val)
        type(radix_tree),pointer:: rt, now
        character(*):: val
        character(:),allocatable:: match, rest_node, rest_val

        rest_node=''
        now => rt
        do while(len_trim(rest_node)==0)
            call search_longest_match(rt%val, val, match, rest_node, rest_val)
            
        end do



    end subroutine
end module


program main
    use,intrinsic :: iso_fortran_env
    use radix_tree_mod
    implicit none
    character(:),allocatable:: ca, cb, match, rest_ca, rest_cb

end program main