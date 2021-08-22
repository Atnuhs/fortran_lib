module splay_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type:: splay_node
        type(splay_node),pointer:: left => null()
        type(splay_node),pointer:: right => null()
        type(splay_node),pointer:: parent => null()
        integer(int32):: size = 1
        integer(int32):: value, minimum
    end type

contains
    function sn_state(sn)
        type(splay_node), pointer:: sn
        integer(int32):: sn_state

        sn_state=0
        if (associated(sn%parent)) then
            if (associated(sn%parent%left, sn)) sn_state=1
            if (associated(sn%parent%right, sn)) sn_state=-1
        end if
    end function


    subroutine sn_rotate(sn)
        type(splay_node),pointer:: sn
        type(splay_node),pointer:: pp, p, c

        ! 登場人物:: 自分(sn), 子供(c), 親(p), 親親(pp)
        ! 自分と親の存在は確定。親親の存在は未定

        ! 自分=子供の片方, 自分=親, 親=親親 それぞれ双方向
        ! 計6本の辺を張り替える。

        p => sn%parent
        pp => p%parent

        if (associated(p%left, sn)) then ! 左回転
            c => sn%right
            sn%right => p
            p%left => c
        else ! 右回転
            c => sn%left
            sn%left => p
            p%right => c
        end if

        if (associated(pp)) then
            if (associated(pp%left, p)) pp%left => sn
            if (associated(pp%right, p)) pp%right => sn
        end if
        
        sn%parent => pp
        p%parent => sn
        if (associated(c)) c%parent => p

        ! 回転後は自分と親のサイズが変わっている。
        ! 回転後は自分は親より上にいるので、親->自分の順にsize_update
        call sn_size_update(p)
        call sn_size_update(sn)
    end subroutine


    subroutine sn_splay(sn)
        type(splay_node),pointer:: sn

        ! 自分の親がいなくなるまで(=自分が根になるまで)続ける
        do while(associated(sn%parent))
            if (sn_state(sn%parent) == 0) then 
                ! 親の親がいない
                call sn_rotate(sn)
            else if (sn_state(sn) == sn_state(sn%parent)) then 
                ! me->p->ppが直線 => 親->自分の順にrotate
                call sn_rotate(sn%parent)
                call sn_rotate(sn)
            else ! me->p->ppが折れ線 => 自分を2回rotate
                call sn_rotate(sn)
                call sn_rotate(sn)
            end if
        end do
    end subroutine


    subroutine sn_size_update(sn)
        class(splay_node):: sn

        sn%size = 1
        sn%minimum = sn%value 
        if (associated(sn%left)) then
            sn%size = sn%size+sn%left%size
            sn%minimum = min(sn%minimum, sn%left%minimum)
        end if
        if (associated(sn%right)) then
            sn%size = sn%size+sn%right%size
            sn%minimum = min(sn%minimum, sn%right%minimum)
        end if
    end subroutine


    subroutine sn_rooting(root, ind)
        type(splay_node),pointer:: root
        type(splay_node),pointer:: now
        integer(int32),value:: ind
        integer(int32):: now_ind

        now => root
        ! ind番目のノードを探しに、根から降りていく
        ! nowは今いるノード, now_indは今いるノードが何番目のノードか。
        do while(.true.)
            
            if (associated(now%left))then
                now_ind = now%left%size + 1
            else
                now_ind = 1
            end if
            if (ind < now_ind) then
                now => now%left
            end if
            if (ind == now_ind) then
                call sn_splay(now)
                root => now
                return
            end if
            if (ind > now_ind) then
                now => now%right
                ind = ind-now_ind
            end if
        end do
    end subroutine
end module
