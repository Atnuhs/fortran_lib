module splay_set_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type:: splay_set_node
        type(splay_set_node),pointer:: left => null()
        type(splay_set_node),pointer:: right => null()
        type(splay_set_node),pointer:: parent => null()
        type(splay_set_node),pointer:: minimum => null()
        type(splay_set_node),pointer:: maximum => null()
        integer(int64):: size = 1
        integer(int64):: value
    end type

contains
    function ssn_state(node)
        type(splay_set_node), pointer:: node
        integer(int64):: ssn_state
        ! root => 0
        ! 自身が親の左の子 => 1
        ! ! ! ! ! 自身が親の右の子 => -1
        ssn_state=0
        if (associated(node%parent)) then
            if (associated(node%parent%left, node)) ssn_state=1
            if (associated(node%parent%right, node)) ssn_state=-1
        end if
    end function


    subroutine ssn_rotate(node)
        type(splay_set_node),pointer:: node
        type(splay_set_node),pointer:: pp, p, c

        ! 登場人物:: 自分(node), 子供(c), 親(p), 親親(pp)
        ! 自分と親の存在は確定。親親の存在は未定

        ! 自分=子供の片方, 自分=親, 親=親親 それぞれ双方向
        ! 計6本の辺を張り替える。

        p => node%parent
        pp => p%parent

        if (associated(p%left, node)) then ! 左回転
            c => node%right
            node%right => p
            p%left => c
        else ! 右回転
            c => node%left
            node%left => p
            p%right => c
        end if

        if (associated(pp)) then
            if (associated(pp%left, p)) pp%left => node
            if (associated(pp%right, p)) pp%right => node
        end if

        node%parent => pp
        p%parent => node
        if (associated(c)) c%parent => p

        ! 回転後は自分と親のサイズが変わっている。
        ! 回転後は自分は親より上にいるので、親->自分の順にsize_update
        call ssn_update_data(p)
        call ssn_update_data(node)
    end subroutine


    subroutine ssn_splay(node)
        type(splay_set_node),pointer:: node

        ! 自分の親がいなくなるまで(=自分が根になるまで)続ける

        do while(associated(node%parent))
            if (ssn_state(node%parent) == 0) then 
                ! 親の親がいない
                call ssn_rotate(node)
            else if (ssn_state(node) == ssn_state(node%parent)) then 
                ! me->p->ppが直線 => 親->自分の順にrotate
                call ssn_rotate(node%parent)
                call ssn_rotate(node)
            else ! me->p->ppが折れ線 => 自分を2回rotate
                call ssn_rotate(node)
                call ssn_rotate(node)
            end if
        end do
    end subroutine


    subroutine ssn_update_data(node)
        type(splay_set_node),pointer:: node

        if (.not. associated(node)) return
        ! 初期設定
        node%size = 1
        node%minimum => node
        node%maximum => node
        ! 子の情報を反映
        if (associated(node%left)) then
            node%size = node%size+node%left%size
            node%minimum => node%left%minimum
        end if
        if (associated(node%right)) then
            node%size = node%size+node%right%size
            node%maximum => node%right%maximum
        end if
    end subroutine


    subroutine ssn_at(root, id)
        type(splay_set_node),pointer:: root
        type(splay_set_node),pointer:: now
        integer(int64),value:: id
        integer(int64):: now_id

        if (.not. associated(root)) return
        if (id < 1 .or. root%size < id) return
        now => root
        ! id番目のノードを探しに、根から降りていく
        ! nowは今いるノード
        ! now_idはnowをノードを根としたときのnowのidex
        do
            now_id = 1
            if (associated(now%left)) now_id = now_id + now%left%size 

            if (id < now_id) then
                now => now%left
            else if (id == now_id) then
                call ssn_splay(now)
                root => now
                return
            else ! (id > now_id)
                now => now%right
                id = id-now_id
            end if
        end do
    end subroutine


    subroutine ssn_le(root, val)
        type(splay_set_node),pointer:: root
        type(splay_set_node),pointer:: now
        integer(int64):: val

        if (.not. associated(root)) return

        now => root
        do
            if (val >= now%value) then
                ! [ T | F ]
                !   ^now
                if (.not. associated(now%right)) then
                    ! [ T ]
                    !     ^now
                    call ssn_splay(now)
                    root => now
                    return
                end if

                if (val < now%right%minimum%value) then
                    ! [ T | F ]
                    !     ^now
                    call ssn_splay(now)
                    root => now
                    return
                end if
                ! Loop!
                now => now%right
            else
                ! [ T | F ]
                !       ^now
                if (.not. associated(now%left)) return ! could not search

                if (val >= now%left%maximum%value) then
                    ! [ T | F ]
                    !     ^^now
                    now => now%left%maximum
                    call ssn_splay(now)
                    root => now
                    return
                end if
                ! Loop!
                now => now%left
            end if
        end do
    end subroutine


    subroutine ssn_gt(root, val)
        type(splay_set_node),pointer:: root
        type(splay_set_node),pointer:: now
        integer(int64):: val

        ! ssn_le を利用する。
        ! ssn_leの探索が成功していた場合、その次に大きい要素がgtである。

        if (.not. associated(root)) return
        call ssn_le(root, val)
        if (val < root%value) then
            ! ssn_leの探索が失敗
            ! [ T ]
            now => root%minimum
            call ssn_splay(now)
            root => now
            return
        else ! (val > root%value)
            ! ssn_ltの探索成功
            if (.not. associated(root%right)) return ! can not search ge
            now => root%right%minimum
            call ssn_splay(now)
            root => now
            return
        end if
    end subroutine


    subroutine ssn_lt(root, val)
        type(splay_set_node),pointer:: root
        integer(int64)::val

        if (.not. associated(root)) return
        call ssn_le(root, val-1)
    end subroutine


    subroutine ssn_ge(root, val)
        type(splay_set_node),pointer:: root
        integer(int64),intent(in):: val
        if (.not. associated(root)) return
        call ssn_gt(root, val-1)
    end subroutine


    subroutine ssn_split(root, val, rroot)
        type(splay_set_node),pointer:: root, rroot
        integer(int64):: val

        ! 例外処理
        if (.not. associated(root)) return

        if (val < root%minimum%value) then
            rroot => root
            root => null()
            return
        end if

        if (val > root%maximum%value) then
            rroot => null()
            return
        end if

        ! min <= val <= max
        rroot => null()
        call ssn_le(root, val)
        if (.not. associated(root%right)) return
        rroot => root%right
        rroot%parent => null()
        root%right => null()
        call ssn_update_data(root)
    end subroutine


    subroutine ssn_merge(root, rroot)
        type(splay_set_node),pointer:: root, rroot

        ! 例外処理
        if (.not. associated(rroot)) return
        if (.not. associated(root)) then
            root => rroot
            return
        end if

        ! 両方ある
        root => root%maximum
        call ssn_splay(root)
        root%right => rroot
        rroot%parent => root
        call ssn_update_data(root)
        rroot => null()
    end subroutine


    subroutine ssn_insert(root, val)
        type(splay_set_node),pointer:: root, rroot, new_node
        integer(int64):: val

        allocate(new_node)
        new_node%value = val
        call ssn_update_data(new_node)

        ! 例外処理
        if (.not. associated(root)) then
            root => new_node
            new_node => null()
            return
        end if

        call ssn_split(root, val, rroot)
        ! if (val == root%value) return ! 重複を許す場合はコメントアウト
        call ssn_merge(root, new_node)
        call ssn_merge(root, rroot)
        rroot => null()
        new_node => null()
    end subroutine


    subroutine ssn_delete(root, val)
        type(splay_set_node),pointer:: root, lroot, rroot
        integer(int64):: val

        if (.not. associated(root)) return

        call ssn_le(root, val)
        if (val /= root%value) return

        lroot => root%left
        rroot => root%right
        if (associated(lroot)) lroot%parent => null()
        if (associated(rroot)) rroot%parent => null()
        deallocate(root)
        call ssn_merge(lroot, rroot)
        root => lroot
        lroot => null()
        rroot => null()
    end subroutine


    subroutine ssn_delete_at(root, id)
        type(splay_set_node),pointer:: root, lroot, rroot
        integer(int64):: id, id_root

        if (.not. associated(root)) return

        call ssn_at(root, id)

        id_root = 1
        if (associated(root%left)) id_root = id_root + root%left%size

        if (id_root /= id) return

        lroot => root%left
        rroot => root%right
        if (associated(lroot)) lroot%parent => null()
        if (associated(rroot)) rroot%parent => null()

        deallocate(root)
        call ssn_merge(lroot, rroot)
        root => lroot

        lroot => null()
        rroot => null()
    end subroutine


    subroutine ssn_print_node(node, indent, lr)
        character(100):: fmt, header
        character(10):: line
        type(splay_set_node),pointer:: node
        integer(int64), intent(in):: indent, lr

        if (lr==0) then
            line = "' ', "
        else if (lr == 1) then
            line = "'┌', "
        else if (lr == -1) then
            line = "'└', "
        end if

        fmt = "'[ val: ', i0, ' |size: ', i0, ' |min: ', i0, ' |max: ', i0, ' ]')"
        write(header, "('(', a, a)") "'" // repeat(' ',indent) // "'," ,trim(line)
        ! print'(a)', trim(header)//trim(fmt)
        print trim(header)//trim(fmt), node%value, node%size, node%minimum%value, node%maximum%value
    end subroutine


    recursive subroutine ssn_print_tree(node, indent, lr)
        type(splay_set_node),pointer:: node
        integer(int64),intent(in):: indent, lr

        if (.not. associated(node)) return
        call ssn_print_tree(node%left, indent+1, 1_int64)
        call ssn_print_node(node, indent, lr)
        call ssn_print_tree(node%right, indent+1, -1_int64)
    end subroutine
end module



module splay_set_mod
    use,intrinsic:: iso_fortran_env
    use splay_set_node_mod
    implicit none
    private
    type,public:: splay_set
        type(splay_set_node),pointer:: root => null()
    contains
        procedure:: insert => sn_insert
        procedure:: delete => sn_delete
        procedure:: delete_at => sn_delete_at
        procedure:: at => sn_at
        procedure:: le => sn_le
        procedure:: lt => sn_lt
        procedure:: ge => sn_ge
        procedure:: gt => sn_gt
        procedure:: has => sn_has
        procedure:: maxval => sn_maxval
        procedure:: minval => sn_minval
        procedure:: size => sn_size
        procedure:: print => sn_print
    end type
contains
    subroutine sn_insert(ss, val)
        class(splay_set):: ss
        integer(int64):: val

        call ssn_insert(ss%root, val)
    end subroutine


    subroutine sn_delete(ss, val)
        class(splay_set):: ss
        integer(int64):: val

        call ssn_delete(ss%root, val)
    end subroutine


    subroutine sn_delete_at(ss, val)
        class(splay_set):: ss
        integer(int64):: val

        call ssn_delete_at(ss%root, val)
    end subroutine


    function sn_at(ss, id, val_error) result(val)
        class(splay_set):: ss
        integer(int64),intent(in):: id
        integer(int64),intent(in),optional:: val_error
        integer(int64):: id_root, val

        ! 例外処理 
        val = -1
        if (present(val_error)) val = val_error
        if (.not. associated(ss%root)) return
        call ssn_at(ss%root, id)
        ! rootのindex計算
        id_root = 1
        if (associated(ss%root%left)) id_root = id_root + ss%root%left%size
        if (id_root /= id) return ! いない

        val = ss%root%value
    end function


    function sn_le(ss, val, val_error) result(ret)
        class(splay_set):: ss
        integer(int64),intent(in):: val
        integer(int64),intent(in),optional:: val_error
        integer(int64):: ret

        ret = -1
        if (present(val_error)) ret = val_error 
        if (.not. associated(ss%root)) return
        call ssn_le(ss%root, val)
        if (val < ss%root%value) return ! 探索失敗
        ret = 1
        if (associated(ss%root%left)) ret = ret + ss%root%left%size
    end function


    function sn_lt(ss, val, val_error) result(ret)
        class(splay_set):: ss
        integer(int64),intent(in):: val
        integer(int64),intent(in),optional:: val_error
        integer(int64):: ret

        ret = -1
        if (present(val_error)) ret = val_error 

        if (.not. associated(ss%root)) return
        call ssn_lt(ss%root, val)

        if (val <= ss%root%value) return ! 探索失敗
        ret = 1
        if (associated(ss%root%left)) ret = ret + ss%root%left%size
    end function


    function sn_ge(ss, val, val_error) result(ret)
        class(splay_set):: ss
        integer(int64),intent(in):: val
        integer(int64),intent(in),optional:: val_error
        integer(int64):: ret

        ret = -1
        if (present(val_error)) ret = val_error 

        if (.not. associated(ss%root)) return
        call ssn_ge(ss%root, val)
        if (val > ss%root%value) return ! 探索失敗
        ret = 1
        if (associated(ss%root%left)) ret = ret + ss%root%left%size
    end function


    function sn_gt(ss, val, val_error) result(ret)
        class(splay_set):: ss
        integer(int64),intent(in):: val
        integer(int64),intent(in),optional:: val_error
        integer(int64):: ret

        ret = -1
        if (present(val_error)) ret = val_error 

        if (.not. associated(ss%root)) return
        call ssn_gt(ss%root, val)

        if (val >= ss%root%value) return ! 探索失敗
        ret = 1
        if (associated(ss%root%left)) ret = ret + ss%root%left%size
    end function


    function sn_has(ss, val) result(ret)
        class(splay_set):: ss
        integer(int64),intent(in):: val
        logical:: ret

        ! 例外処理
        ret = .false.
        if (.not. associated(ss%root)) return
        call ssn_le(ss%root, val)
        ret = val == ss%root%value
    end function


    function sn_maxval(ss, val_error) result(val)
        class(splay_set):: ss
        integer(int64),intent(in),optional:: val_error
        integer(int64):: val

        val = -1
        if (present(val_error)) val = val_error

        if (.not. associated(ss%root)) return

        val = ss%root%maximum%value
    end function


    function sn_minval(ss, val_error) result(val)
        class(splay_set):: ss
        integer(int64),intent(in),optional:: val_error
        integer(int64):: val

        val = -1
        if (present(val_error)) val = val_error

        if (.not. associated(ss%root)) return

        val = ss%root%minimum%value
    end function


    function sn_size(ss, val_error) result(ret)
        class(splay_set):: ss
        integer(int64),intent(in),optional:: val_error
        integer(int64):: ret

        ret = -1
        if (present(val_error)) ret = val_error

        if (.not. associated(ss%root)) return

        ret = ss%root%size
    end function


    subroutine sn_print(ss)
        class(splay_set):: ss

        call ssn_print_tree(ss%root, 0_int64, 0_int64)
    end subroutine 
end module
