module splay_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: prec=int64
    type:: splay_node
        type(splay_node),pointer:: left => null()
        type(splay_node),pointer:: right => null()
        type(splay_node),pointer:: parent => null()
        integer(int32):: size = 1
        integer(prec):: value, minimum
    contains
        procedure:: state => sn_state
        procedure:: rotate => sn_rotate
        procedure:: size_update => sn_size_update
        procedure:: splay => sn_splay
        procedure,private,pass:: sn_equals
        procedure,private:: sn_assign
        generic:: operator( == ) => sn_equals
        generic:: assignment( = ) => sn_assign
    end type

contains
    function sn_equals(sn1,sn2) result(is_equals)
        class(splay_node),intent(in):: sn1, sn2
        logical:: is_equals

        is_equals = .true.
        is_equals = is_equals .and. associated(sn1%left, sn2%left)
        is_equals = is_equals .and. associated(sn1%right, sn2%right)
        is_equals = is_equals .and. associated(sn1%parent, sn2%parent)
        is_equals = is_equals .and. sn1%value == sn2%value
        is_equals = is_equals .and. sn1%size == sn2%size
    end function


    subroutine sn_assign(sn1,sn2)
        class(splay_node),intent(inout):: sn1
        type(splay_node),intent(in):: sn2

        sn1%left = sn2%left
        sn1%right = sn2%right
        sn1%parent = sn2%parent
        sn1%size = sn2%size
        sn1%value = sn2%value
    end subroutine


    function sn_state(sn)
        class(splay_node),intent(in):: sn
        integer(int32):: sn_state

        sn_state=0
        if (associated(sn%parent)) then
            if (sn%parent%left == sn) sn_state=1
            if (sn%parent%right == sn) sn_state=-1
        end if
    end function


    subroutine sn_rotate(sn)
        class(splay_node),intent(inout):: sn
        type(splay_node),pointer:: pp, p, c

        ! 登場人物:: 自分(sn), 親(p), 親親(pp), 子供(c)
        ! 自分と親の存在は確定。親親の存在は未定

        ! 自分=子供の片方, 自分=親, 親=親親 それぞれ双方向
        ! 計6本の辺を張り替える。

        p = sn%parent
        pp = p%parent

        if (p%left == sn) then ! 左回転
            c = sn%right
            sn%right = p
            p%left = c
        else ! 右回転
            c = sn%left
            sn%left = p
            p%right = c
        end if

        if (associated(pp)) then
            if (pp%left == p) pp%left = sn
            if (pp%right == p) pp%right = sn
        end if
        
        sn%parent = pp
        p%parent = sn
        if (associated(c)) c%parent = p

        ! 回転後は自分と親のサイズが変わっている。
        ! 回転後は自分は親より上にいるので、親->自分の順にsize_update
        call p%size_update()
        call sn%size_update()
    end subroutine


    subroutine sn_splay(sn)
        class(splay_node):: sn

        ! 自分の親がいなくなるまで(=自分が根になるまで)続ける
        do while(associated(sn%parent))
            if (sn%parent%state() == 0) then 
                ! 親の親がいない
                call sn%rotate()
            else if (sn%state() == sn%parent%state()) then 
                ! me->p->ppが直線 => 親->自分の順にrotate
                call sn%parent%rotate()
                call sn%rotate()
            else ! me->p->ppが折れ線 => 自分を2回rotate
                call sn%rotate()
                call sn%rotate()
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


    function sn_get_node(sn, ind) result(ret_node)
        class(splay_node),intent(in):: sn
        type(splay_node):: now, ret_node
        integer(prec),value:: ind
        integer(int32):: now_ind

        now = sn
        ! ind番目のノードを探しに、根から降りていく
        ! nowは今いるノード, now_indは今いるノードが何番目のノードか。
        do while(.true.)
            if (associated(now%left))then
                now_ind = sn%left%size + 1
            else
                now_ind = 1
            end if

            if (ind < now_ind) then
                now = now%left
            end if
            if (ind == now_ind) then
                call now%splay()
                ret_node = now
            end if
            if (ind > now_ind) then
                now = now%right
                ind = ind-now_ind
            end if
        end do
    end function


    subroutine sn_merge(lroot, rroot)
        class(splay_node):: lroot
        type(splay_node):: rroot
        ! 左のノードに右のノードをくっつける


    end subroutine


    subroutine sn_split(sn, left_cnt, lroot, rroot)
        class(splay_node):: sn
        type(splay_node),intent(out):: lroot, rroot
        integer(int32),intent(in):: left_cnt

    end subroutine


    subroutine sn_insert(sn, ind, node)
        class(splay_node):: sn, node
        integer(int32),intent(in):: ind

    end subroutine


    subroutine sn_delete(sn, ind)
        class(splay_node):: sn
        integer(int32),intent(in):: ind
end subroutine
end module