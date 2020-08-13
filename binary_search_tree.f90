module bst_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type bst_node
        integer(int64):: key
        integer(int64):: val
        type(bst_node),pointer:: c_left=>null()
        type(bst_node),pointer:: c_right=>null()
    end type
contains
    function has_key(root, key) result(ret)
        type(bst_node),pointer:: root, node
        integer(int64):: key
        logical:: ret

        ret = .false.
        node => root

        do while(associated(node))
            if (key < node%key) then
                node => node%c_left
            else if (key > node%key) then
                node => node%c_right
            else
                ret = .true.
                return
            end if
        end do
    end function


    function get_val(root, key) result(ret)
        type(bst_node),pointer:: root, node
        integer(int64):: key, ret
        node => root
        ret = 0
        if (.not. has_key(root, key)) then
            print*, 'unexpected_access at getval!'
            stop
        end if

        do while(associated(node))
            if (key < node%key) then
                node => node%c_left
            else if (key > node%key) then
                node => node%c_right
            else
                ret = node%val
                return
            end if
        end do
    end function

    function generate_new_node(key,val) result(add)
        integer(int64):: key,val
        type(bst_node),pointer:: add

        allocate(add)
        add%key=key
        add%val=val
        add%c_left => null()
        add%c_right=> null()
    end function

    subroutine add_node(root,key,val)
        type(bst_node),pointer:: root, node
        integer(int64):: key,val
        if (.not. associated(root)) then
            root => generate_new_node(key,val)
            return
        end if
        
        node => root
        do
            if (key < node%key) then
                if (.not. associated(node%c_left)) then
                    node%c_left => generate_new_node(key,val)
                    exit
                end if
                node => node%c_left
            else if (key > node%key) then
                if (.not. associated(node%c_right)) then
                    node%c_right => generate_new_node(key,val)
                    exit
                end if
                node => node%c_right
            else
                node%val = val
                exit
            end if
        end do
    end subroutine

    subroutine delete_key(root,key)
        type(bst_node),pointer:: root, node, parent,child
        integer(int64):: key

        node => root
        parent => null()

        do while(associated(node))
            if (key < node%key) then
                parent => node
                node => node%c_left
            else if (key > node%key) then
                parent => node
                node => node%c_right
            else
                if (associated(node%c_left) .and. associated(node%c_right)) then
                    call delete_two_child_node(root, node)
                else if (associated(node%c_left)) then
                    child => node%c_left
                    call delete_one_child_node(node, child)
                else if (associated(node%c_right)) then
                    child => node%c_right
                    call delete_one_child_node(node, child)
                else
                    call delete_leaf(parent, root, node)
                end if
                exit
            end if
        end do
    end subroutine

    subroutine delete_leaf(parent, root, node)
        type(bst_node),pointer:: parent, root, node
        if (associated(parent)) then
            if (associated(parent%c_left,node)) then
                parent%c_left => null()
            else
                parent%c_right => null()
            end if
            deallocate(node)
        else
            deallocate(node)
            root => null()
        end if
    end subroutine

    subroutine delete_one_child_node(node, child)
        type(bst_node),pointer:: child, node
        node%key = child%key
        node%c_left => child%c_left
        node%c_right => child%c_right
        deallocate(child)
    end subroutine

    subroutine delete_two_child_node(root, node)
        type(bst_node),pointer:: root,node
        type(bst_node),pointer:: max,max_parent

        max => node%c_left
        max_parent => node
        do while(associated(max%c_right))
            max_parent => max
            max => max%c_right
        end do

        node%key = max%key

        if (associated(max%c_left)) then
            call delete_one_child_node(max,max_parent)
        else
            call delete_leaf(max_parent,root,max)
        end if
    end subroutine

    recursive subroutine print_bst_node(root, depth)
        type(bst_node),pointer:: root
        integer(int32),optional:: depth
        integer(int32):: i, d

        if (.not. associated(root)) return
        d=0
        if (present(depth)) d=depth
        call print_bst_node(root%c_right, d+1)
        do i=1,d
            write(*,'(a)',advance='no') '-'
        end do
        print('(a,1x,a)'), 'key', root%key
        call print_bst_node(root%c_left, d+1)
    end subroutine
end module

module bst_mod
    use,intrinsic :: iso_fortran_env
    use bst_node_mod
    implicit none
    type bst
        type(bst_node),pointer:: root => null()
        contains
        procedure:: append => append_bst
        procedure:: del => delete_bst
        procedure:: has => has_key_bst
        procedure:: val => get_value_bst
        procedure:: print => print_bst
    end type

contains
    subroutine append_bst(self, key, val)
        class(bst):: self
        integer(int64):: key,val
        call add_node(self%root,key,val)
    end subroutine

    subroutine delete_bst(self, key)
        class(bst):: self
        integer(int64):: key
        call delete_key(self%root, key)
    end subroutine

    function has_key_bst(self, key) result(ret)
        class(bst):: self
        integer(int64):: key
        logical:: ret
        
        ret = has_key(self%root, key)
    end function

    function get_value_bst(self, key) result(ret)
        class(bst):: self
        integer(int64):: key, ret
        ret = get_val(self%root, key)
    end function

    subroutine print_bst(self)
        class(bst):: self
        call print_bst_node(self%root)
    end subroutine
end module
