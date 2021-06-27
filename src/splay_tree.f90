

module splay_tree_mod
    use,intrinsic :: iso_fortran_env
    use splay_node_mod
contains
    


    subroutine st_merge(lroot, rroot)
        type(splay_node),pointer:: lroot, rroot
        ! lrootにrrootをくっつける

        if (.not. associated(lroot))then
            lroot = rroot
        else if (.not. associated(rroot))then
            lroot = lroot
            return
        else
            ! lrootの右端をrootに
            call st_rooting(lroot, lroot%size)
            ! rrootをlrootの右の子にする
            lroot%right = rroot
            rroot%parent = lroot
            call lroot%size_update()
        end if
    end subroutine


    subroutine st_split(root, left_cnt, lroot, rroot)
        type(splay_node),pointer:: root, lroot, rroot
        integer(int32), intent(in):: left_cnt

        if (left_cnt == 0) then
            lroot => null()
            rroot = root
        else if (left_cnt == root%size) then
            lroot = root
            rroot => null()
        else
            call st_rooting(root, left_cnt)
            lroot = root
            rroot = root%right
            lroot%right => null()
            rroot%parent => null()
            call lroot%size_update()
        end if
    end subroutine


    subroutine st_insert(root, ind, node)
        class(splay_node):: root, node
        integer(int32),intent(in):: ind

    end subroutine


    subroutine st_delete(root, ind)
        class(splay_node):: root
        integer(int32),intent(in):: ind
end subroutine
end module