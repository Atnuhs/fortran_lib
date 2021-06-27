

module splay_tree_mod
    use,intrinsic :: iso_fortran_env
    use splay_node_mod
contains
    subroutine st_merge(lroot, rroot)
        type(splay_node),pointer:: lroot, rroot
        ! lrootにrrootをくっつける
        ! 片方空っぽの時のための例外処理
        if (.not. associated(lroot))then
            lroot = rroot
        else if (.not. associated(rroot))then
            lroot = lroot
            return
        else ! 両方あるとき 
            ! lrootの右端をrootに
            call sn_rooting(lroot, lroot%size)
            ! rrootをlrootの右の子にする
            lroot%right = rroot
            rroot%parent = lroot
            call sn_size_update(lroot)
        end if
    end subroutine


    subroutine st_split(root, left_cnt, lroot, rroot)
        type(splay_node),pointer:: root, lroot, rroot
        integer(int32), intent(in):: left_cnt

        if (left_cnt == 0) then
            ! 左のサイズが0
            lroot => null()
            rroot = root
        else if (left_cnt == root%size) then
            ! 左のサイズが全部
            lroot = root
            rroot => null()
        else
            call sn_rooting(root, left_cnt)
            lroot = root
            rroot = root%right
            lroot%right => null()
            rroot%parent => null()
            call sn_size_update(lroot)
        end if
    end subroutine


    subroutine st_insert(root, ind, node)
        type(splay_node),pointer:: root, node
        type(splay_node),pointer:: lroot, rroot
        integer(int32),intent(in):: ind

        call st_split(root, ind-1, lroot, rroot)
        call st_merge(lroot, node)
        call st_merge(lroot, rroot)
        root => lroot
    end subroutine


    subroutine st_delete(root, ind)
        type(splay_node),pointer:: root
        type(splay_node),pointer:: lroot, rroot
        integer(int32),intent(in):: ind

        call sn_rooting(root, ind)
        lroot => root%left
        rroot => root%right
        if (associated(lroot)) lroot%parent => null()
        if (associated(rroot)) rroot%parent => null()
        deallocate(root)
        call st_merge(lroot, rroot)
        root => lroot
        call sn_size_update(root)
    end subroutine
end module