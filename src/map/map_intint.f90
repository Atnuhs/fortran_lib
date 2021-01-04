

module map_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: map_node_insert
    public:: map_node_erase
    public:: map_node_to_array
    public:: map_node_print, map_node_print_debug
    public:: mn_min, mn_max
    public:: mn_has_key, mn_retval

    type,public:: map_node
        integer(int32):: level=1
        integer(int32):: key
        integer(int32):: val
        type(map_node),pointer:: left_child => null()
        type(map_node),pointer:: right_child => null()
    end type
contains
    function mn_has_left_child(n) result(ret)
        type(map_node),pointer:: n
        logical:: ret

        ret = associated(n%left_child)
    end function


    function mn_has_right_child(n) result(ret)
        type(map_node),pointer:: n
        logical:: ret

        ret = associated(n%right_child)
    end function


    function mn_is_leaf(n) result(ret)
        type(map_node),pointer:: n
        logical:: ret

        ret = .not. (mn_has_left_child(n) .or. mn_has_right_child(n))
    end function


    function mn_min(n) result(ret)
        type(map_node),pointer:: n,ret

        ret => n
        do while(mn_has_left_child(ret))
            ret => ret%left_child
        end do
    end function


    function mn_max(n) result(ret)
        type(map_node),pointer:: n,ret

        ret => n
        do while(mn_has_right_child(ret))
            ret => ret%right_child
        end do
    end function


    function mn_predecessor(n) result(ret)
        !left -> rightend
        type(map_node),pointer:: n,ret

        ret => mn_max(n%left_child)
    end function


    function mn_succesor(n) result(ret)
        !right -> leftend
        type(map_node),pointer:: n,ret

        ret => mn_min(n%right_child)
    end function


    function mn_skew(n) result(ret)
        type(map_node),pointer:: n, ret, sub_n

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (.not. mn_has_left_child(n)) then
            ret => n
        else if (n%level == n%left_child%level) then
            sub_n => n%left_child
            n%left_child => sub_n%right_child
            sub_n%right_child => n
            ret => sub_n
        else
            ret => n
        end if
    end function


    function mn_split(n) result(ret)
        type(map_node),pointer:: n,ret,sub_n

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (.not. mn_has_right_child(n) .or. .not. mn_has_right_child(n%right_child)) then
            ret => n
        else if (n%level == n%right_child%right_child%level) then
            sub_n => n%right_child
            n%right_child => sub_n%left_child
            sub_n%left_child => n
            sub_n%level = sub_n%level+1
            ret => sub_n
        else
            ret => n
        end if
    end function


    function mn_level_of(n) result(ret)
        type(map_node),pointer:: n
        integer(int32):: ret

        ret = 0
        if (associated(n)) ret = n%level
    end function


    function mn_decrease_level(n) result(ret)
        type(map_node),pointer:: n, ret
        integer(int32):: should_be

        should_be = min(mn_level_of(n%left_child), mn_level_of(n%right_child)) + 1
        if (should_be < n%level) then
            n%level = should_be
            if (should_be < mn_level_of(n%right_child)) then
                n%right_child%level = should_be
            end if
        end if
        ret => n
    end function


    recursive function mn_has_key(n,key) result(ret)
    type(map_node),pointer:: n
        integer(int32):: key
        logical:: ret

        if (.not. associated(n)) then
            ret = .false.
            return
        else if (key > n%key) then
            ret = mn_has_key(n%right_child, key)
        else if (key < n%key) then
            ret = mn_has_key(n%left_child, key)
        else
            ret = .true.
        end if
    end function


    recursive function mn_retval(n,key) result(ret)
    type(map_node),pointer:: n
        integer(int32):: key
        integer(int32):: ret

        if (.not. associated(n)) then
            ret = 0
            return
        else if (key > n%key) then
            ret = mn_retval(n%right_child, key)
        else if (key < n%key) then
            ret = mn_retval(n%left_child, key)
        else
            ret = n%val
        end if
    end function


    recursive function map_node_insert(n,key,val,inserted) result(ret)
        type(map_node),pointer:: n,ret
        integer(int32),intent(in):: key
        integer(int32),intent(in):: val
        logical:: inserted

        inserted = .false.
        if (.not. associated(n)) then
            allocate(ret)
            ret = map_node(key=key,val=val,level=1)
            inserted = .true.
            return
        else if (key > n%key) then
            n%right_child => map_node_insert(n%right_child, key, val, inserted)
        else if (key < n%key) then
            n%left_child => map_node_insert(n%left_child, key, val, inserted)
        end if
        n => mn_skew(n)
        n => mn_split(n)
        ret => n
    end function


    recursive function map_node_erase(n,key) result(ret)
        type(map_node),pointer:: n,ret,nn
        integer(int32):: key

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (key > n%key) then
            n%right_child => map_node_erase(n%right_child, key)
        else if (key < n%key) then
            n%left_child => map_node_erase(n%left_child, key)
        else
            if (mn_is_leaf(n)) then
                allocate(ret)
                ret => null()
                return
            else if (mn_has_left_child(n)) then
                nn => mn_predecessor(n)
                n%key = nn%key
                n%left_child => map_node_erase(n%left_child, nn%key)
            else
                nn => mn_succesor(n)
                n%key = nn%key
                n%right_child => map_node_erase(n%right_child,nn%key)
            end if
        end if
        n => mn_decrease_level(n)
        n => mn_skew(n)
        n%right_child => mn_skew(n%right_child)
        if (mn_has_left_child(n)) n%right_child%right_child => mn_skew(n%right_child%right_child)
        n => mn_split(n)
        n%right_child => mn_split(n%right_child)
        ret => n
    end function


    recursive subroutine map_node_print_debug(n,i,d)
        type(map_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call map_node_print_debug(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print*,"[", n%key, n%val, n%level, "]"
        call map_node_print_debug(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine map_node_print(n,i,d)
        type(map_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call map_node_print(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print*, n%key, n%val
        call map_node_print(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine map_node_to_array(n,ind,k_ar,v_ar)
    type(map_node),pointer:: n
    integer(int32),intent(out):: ind
    integer(int32),intent(out):: k_ar(:),v_ar(:)
    integer(int32):: i

    if (.not. associated(n)) return
    i=ind
    call map_node_to_array(n%left_child,i,k_ar,v_ar)
    k_ar(i)=n%key; v_ar(i)=n%val
    i=i+1
    call map_node_to_array(n%right_child,i,k_ar,v_ar)
    ind=i
end subroutine
end module



module map_mod
    use,intrinsic :: iso_fortran_env
    use map_node_mod
    implicit none
    private
    public:: s_print, s_print_debug
    type,public:: map
        type(map_node),pointer,private:: head => null()
        integer(int32),private:: len = 0
    contains
        procedure,public:: insert => map_insert, erase => map_erase
        procedure,public:: size => map_size, to_array => map_to_array
        procedure,public:: min => map_min, max=>map_max
        procedure,public:: empty => map_empty
        procedure,public:: has_key => map_has_key, retval => map_retval
    end type
contains
    subroutine die(i)
        integer(int32):: i
        print'(a,i0)', "die: ",i
    end subroutine


    function map_empty(m) result(ret)
        class(map),intent(in):: m
        logical:: ret

        ret = .not. associated(m%head)
    end function


    function map_min(m) result(ret)
        class(map),intent(in):: m
        type(map_node),pointer:: n
        integer(int32):: ret

        if (m%empty()) call die(1)
        n => mn_min(m%head)
        ret = n%val
    end function


    function map_max(m) result(ret)
        class(map),intent(in):: m
        type(map_node),pointer:: n
        integer(int32):: ret

        if (m%empty()) call die(2)
        n => mn_max(m%head)
        ret = n%val
    end function


    subroutine map_insert(m,key,val)
        class(map),intent(inout):: m
        integer(int32),intent(in):: key
        integer(int32),intent(in):: val
        type(map_node),pointer:: new
        logical:: inserted

        if (m%empty()) then
            allocate(new)
            new = map_node(key=key,val=val)
            m%head => new
        else
            m%head => map_node_insert(m%head,key,val,inserted)
        end if
        if(inserted) m%len=m%len+1
    end subroutine


    subroutine map_erase(m,key)
        class(map),intent(inout):: m
        integer(int32),intent(in):: key

        m%head => map_node_erase(m%head, key)
        m%len=max(m%len-1, 0)
    end subroutine


    subroutine s_print(m)
        type(map),intent(inout):: m
        
        call map_node_print(m%head,0,' ')
    end subroutine


    subroutine s_print_debug(m)
        type(map),intent(inout):: m
        
        print"(a,i0)", "len:",m%len
        print*, "==[key, val, level]=="
        call map_node_print_debug(m%head,0,' ')
    end subroutine


    function map_size(m) result(ret)
        class(map),intent(inout):: m
        integer(int32):: ret

        ret = m%len
    end function


    subroutine map_to_array(m,v_ar,k_ar)
        class(map):: m
        integer(int32):: k_ar(m%len),i
        integer(int32):: v_ar(m%len)

        i=1
        call map_node_to_array(m%head,i,k_ar,v_ar)
    end subroutine


    function map_has_key(m,key) result(ret)
        class(map),intent(in):: m
        integer(int32),intent(in):: key
        logical:: ret

        ret = mn_has_key(m%head,key)
    end function


    function map_retval(m,key) result(ret)
        class(map),intent(in):: m
        integer(int32),intent(in):: key
        integer(int32):: ret

        ret = mn_retval(m%head,key)
    end function
end module