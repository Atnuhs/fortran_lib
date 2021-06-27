program test_splay_node
    use,intrinsic :: iso_fortran_env
    use splay_node_mod
    implicit none

    print'(a)', '### splay_node test'
    if (.not. test_rooting()) error stop

contains
    function test_rooting() result(ret)
        type(splay_node),pointer:: root
        integer(int32):: n,i
        logical:: ret
        ret = .true.
        root => null()
        n = 100
        do i=1,n
            print'(i0)', i
            call sn_rooting(root,i)
            root%value=i
        end do

        call debug_print(root, 1, '')
    end function

    recursive subroutine debug_print(now, ind, clr)
        type(splay_node),pointer:: now
        integer(int32):: ind,i
        character(1):: clr

    if (.not. associated(now)) return
    call debug_print(now%right,ind+1,"r")

    do i=1,ind
        write(*,'(a)',advance="no") '  '
    end do

    if (clr == "r") then
        write(*,'(a)',advance="no") "┌ "
    else if (clr == "l") then
        write(*,'(a)',advance="no") "└ "
    else
        write(*,'(a)',advance="no") "  "
    end if
    print'("[ ",2(i0,1x),"]")', now%value, now%size
    call debug_print(now%left,ind+1,"l")
    end subroutine

end program test_splay_node