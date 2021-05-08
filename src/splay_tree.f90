module splay_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: prec=int64
    type:: splay_node
        type(splay_node),pointer:: left => null()
        type(splay_node),pointer:: right => null()
        type(splay_node),pointer:: parent => null()
        integer(int32):: size = 1
        integer(prec):: value
    contains
        procedure:: has_left => sn_has_left
        procedure:: has_right => sn_has_right
        procedure:: has_parent => sn_has_parent
        procedure:: state => sn_state
        procedure:: rotate => sn_rotate
        procedure:: update => sn_update
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


    function sn_has_left(sn)
        class(splay_node),intent(in):: sn
        logical:: sn_has_left

        sn_has_left = associated(sn%left)
    end function


    function sn_has_right(sn)
        class(splay_node),intent(in):: sn
        logical:: sn_has_right

        sn_has_right = associated(sn%right)
    end function


    function sn_has_parent(sn)
        class(splay_node),intent(in):: sn
        logical:: sn_has_parent

        sn_has_parent = associated(sn%parent)
    end function


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

        p = sn%parent
        pp = pp%parent

        if (p%left == sn) then
            c = sn%right
            sn%right = p
            p%left = c
        else
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
        c%parent = p

        call p%update()
        call sn%update()
    end subroutine


    subroutine sn_splay(sn)
        class(splay_node):: sn

        do while(sn%has_parent())
            if (sn%parent%state() == 0) then ! 親の親がいない
                call sn%rotate()
            else if (sn%state() == sn%parent%state()) then ! me->p->ppが直線
                call sn%parent%rotate()
                call sn%rotate()
            else ! me->p->ppが折れ線
                call sn%rotate()
                call sn%rotate()
            end if
        end do
    end subroutine


    subroutine sn_update(sn)
        class(splay_node):: sn

        sn%size = 1
        if (sn%has_left()) sn%size = sn%size+sn%left%size
        if (sn%has_right()) sn%size = sn%size+sn%right%size
    end subroutine


    function sn_get(sn, ind)
        class(splay_node),intent(in):: sn
        type(splay_node):: now, sn_get
        integer(prec),value:: ind
        integer(int32):: now_ind

        now = sn
        do while(.true.)
            now_ind = 0
            if (now%has_left()) now_ind = sn%left%size + 1
            if (ind < now_ind) then
                now = now%left
            end if
            if (ind == now_ind) then
                call now%splay()
                sn_get = now
            end if
            if (ind > now_ind) then
                now = now%right
                ind = ind-now_ind
            end if
        end do
    end function
end module