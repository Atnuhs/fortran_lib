module map_splay_node_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type:: map_splay_node
        type(map_splay_node),pointer:: left => null()
        type(map_splay_node),pointer:: right => null()
        type(map_splay_node),pointer:: parent => null()
        integer(int32):: size = 1
        integer(int32):: key, minimum
        integer(int32):: value
    end type

contains
    function sn_state(sn)
        ! 親がいなければ0
        ! 親の左子が自分なら1
        ! 親が右子が自分なら-1
        type(map_splay_node), pointer:: sn
        integer(int32):: sn_state

        sn_state=0
        if (associated(sn%parent)) then
            if (associated(sn%parent%left, sn)) sn_state=1
            if (associated(sn%parent%right, sn)) sn_state=-1
        end if
    end function


    subroutine sn_rotate(sn)
        type(map_splay_node),pointer:: sn
        type(map_splay_node),pointer:: pp, p, c

        ! 登場人物:: 自分(sn), 子供(c), 親(p), 親親(pp)
        ! 自分と親の存在は確定。親親の存在は未定

        ! 自分=子供の片方, 自分=親, 親=親親 それぞれ双方向
        ! 計6本の辺を張り替える。

        p => sn%parent
        pp => p%parent

        if (associated(p%left, sn)) then ! 親の左子が自分
            ! 左回転
            c => sn%right
            sn%right => p
            p%left => c
        else ! 親の右子が自分
            ! 右回転
            c => sn%left
            sn%left => p
            p%right => c
        end if

        if (associated(pp)) then ! 親親がいる場合の辺のつなぎ直し
            if (associated(pp%left, p)) pp%left => sn
            if (associated(pp%right, p)) pp%right => sn
        end if
        
        ! 上方向へのつなぎ直し
        sn%parent => pp
        p%parent => sn
        if (associated(c)) c%parent => p

        ! 回転後は自分と親のサイズが変わっている。
        ! 回転後は自分は親より上にいるので、親->自分の順にsize_update
        call sn_size_update(p)
        call sn_size_update(sn)
    end subroutine


    subroutine sn_splay(sn)
        type(map_splay_node),pointer:: sn

        ! 自分の親がいる間ループ(=自分が根になるまで)続ける
        do while(associated(sn%parent))
            ! 自分の親はいる（確定）
            if (sn_state(sn%parent) == 0) then 
                ! 親の親がいない(自分が根の子の位置)
                call sn_rotate(sn)
            else if (sn_state(sn) == sn_state(sn%parent)) then 
                ! me -> p -> pp が直線 => 親->自分の順にrotate
                call sn_rotate(sn%parent)
                call sn_rotate(sn)
            else ! me -> p -> ppが折れ線 => 自分を2回rotate
                call sn_rotate(sn)
                call sn_rotate(sn)
            end if
        end do
    end subroutine


    subroutine sn_size_update(sn)
        class(map_splay_node):: sn

        sn%size = 1
        sn%minimum = sn%key 
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
        type(map_splay_node),pointer:: root
        type(map_splay_node),pointer:: now
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


    subroutine sn_find(root, key, now, now_p)
        type(map_splay_node),pointer:: root
        type(map_splay_node),pointer,intent(out):: now, now_p
        integer(int32):: key
        ! keyとその親を返す。

        now => root
        now_p => null()

        do while(associated(now))
            if (key < now%key) then
                now_p => now
                now => now%left
            else if (key > now%key) then
                now_p => now
                now => now%right
            else ! key == now%key
                call sn_splay(now)
                now_p => now%parent
                return
            end if
        end do
    end subroutine
end module map_splay_node_int32_mod



module map_splay_node_mod
    use,intrinsic :: iso_fortran_env
    use map_splay_node_int32_mod, map_splay_node_int32 => map_splay_node
    implicit none
end module map_splay_node_mod



module map_splay_int32_mod
    use,intrinsic :: iso_fortran_env
    use map_splay_node_mod
    implicit none
    type map_splay
        type(map_splay_node_int32),pointer:: root => null()
    contains
        procedure:: insert => ms_insert
        procedure:: has => ms_has
        procedure:: at => ms_at
        procedure:: size => ms_size
        procedure:: delete => ms_delete
    end type
contains
    subroutine ms_insert(self, key, val)
        class(map_splay),intent(inout):: self
        integer(int32),intent(in):: key, val
        type(map_splay_node_int32),pointer:: node, parent

        call sn_find(self%root, key, node, parent)
        if (.not. associated(node)) then
            ! 存在しないので作成
            allocate(node)
            node%key = key
            node%parent => parent
            call sn_splay(node)
        end if

        node%value = val
        self%root => node
    end subroutine


    function ms_has(self, key) result(ret)
        class(map_splay),intent(in):: self
        integer(int32),intent(in):: key
        type(map_splay_node_int32),pointer:: node, parent
        logical:: ret

        call sn_find(self%root, key, node, parent)
        ret = associated(node)
    end function


    function ms_at(self, key) result(ret)
        class(map_splay),intent(inout):: self
        integer(int32),intent(in):: key
        type(map_splay_node_int32),pointer:: node, parent
        integer(int32):: ret

        call sn_find(self%root, key, node, parent)

        if (.not. associated(node)) then
            ! 存在しないので作成
            allocate(node)
            node%key = key
            node%value = 0
            node%parent => parent
            call sn_splay(node)
        end if
        
        ret = node%value
        self%root => node
    end function


    function ms_size(self) result(ret)
        class(map_splay),intent(in):: self
        integer(int32):: ret

        ret = self%root%size
    end function


    subroutine ms_delete(self, key)
        class(map_splay),intent(inout):: self
        integer(int32),intent(in):: key
        type(map_splay_node_int32),pointer:: node, parent, lroot, rroot

        call sn_find(self%root, key, node, parent)
        if (.not. associated(node)) return

        lroot => node%left
        rroot => node%right
        if (associated(lroot)) lroot%parent => null()
        if (associated(rroot)) rroot%parent => null()
        deallocate(node)
        
        ! merge

        if (.not. associated(lroot))then
            lroot => rroot
        else if (.not. associated(rroot))then
            lroot => lroot
        else ! 両方あるとき 
            ! lrootの右端をrootに
            call sn_rooting(lroot, lroot%size)
            ! rrootをlrootの右の子にする
            lroot%right = rroot
            rroot%parent = lroot
            call sn_size_update(lroot)
        end if
        self%root => lroot
    end subroutine
end module map_splay_int32_mod


module map_splay_mod
    use,intrinsic :: iso_fortran_env
    use map_splay_int32_mod, map_splay_int32 => map_splay
    implicit none
end module map_splay_mod
