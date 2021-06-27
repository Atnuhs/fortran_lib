program test_splay_node
    use,intrinsic :: iso_fortran_env
    use splay_node_mod
    use random_mod
    implicit none

    print'(a)', '### splay_node test'
    if (.not. test_rooting()) error stop

contains
    function test_rooting() result(ret)
        type(splay_node),pointer:: node(:), root
        integer(int32):: n,i,v
        logical:: ret
        ret = .true.

        n = 10
        allocate(node(10000))
        do i=1,n
            node(i)%parent => node(i+1)
            node(i+1)%left => node(i)
            call sn_size_update(node(i+1))
        end do
        root => node(n+1)
        call debug_print(root, 1, '')
        do i=1,10
            v = randrange(1,n)
            print*, repeat("=",20), i,v, repeat("=",20)
            call sn_rooting(root, v)
            root%value = i
            call debug_print(root, 1, '')
        end do
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
        write(*,'(a)',advance="no") "*"
    end if
    print'("[ ",2(i0,1x),"]")', now%value, now%size
    call debug_print(now%left,ind+1,"l")
    end subroutine

end program test_splay_node