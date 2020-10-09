module set_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: set_node_insert
    public:: set_node_erase
    public:: set_node_to_array
    public:: set_node_print, set_node_print_debug
    public:: sn_min, sn_max
    type,public:: set_node
        integer(int32):: level=1
        integer(int32):: value
        type(set_node),pointer:: left_child => null()
        type(set_node),pointer:: right_child => null()
    end type
contains
    function sn_has_left_child(n) result(ret)
        type(set_node),pointer:: n
        logical:: ret

        ret = associated(n%left_child)
    end function


    function sn_has_right_child(n) result(ret)
        type(set_node),pointer:: n
        logical:: ret

        ret = associated(n%right_child)
    end function


    function sn_is_leaf(n) result(ret)
        type(set_node),pointer:: n
        logical:: ret

        ret = .not. (sn_has_left_child(n) .or. sn_has_right_child(n))
    end function


    function sn_min(n) result(ret)
        type(set_node),pointer:: n,ret

        ret => n
        do while(sn_has_left_child(ret))
            ret => ret%left_child
        end do
    end function


    function sn_max(n) result(ret)
        type(set_node),pointer:: n,ret

        ret => n
        do while(sn_has_right_child(ret))
            ret => ret%right_child
        end do
    end function


    function sn_predecessor(n) result(ret)
        !left -> rightend
        type(set_node),pointer:: n,ret

        ret => sn_max(n%left_child)
    end function


    function sn_succesor(n) result(ret)
        !right -> leftend
        type(set_node),pointer:: n,ret

        ret => sn_min(n%right_child)
    end function


    function sn_skew(n) result(ret)
        type(set_node),pointer:: n, ret, sub_n

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (.not. sn_has_left_child(n)) then
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


    function sn_split(n) result(ret)
        type(set_node),pointer:: n,ret,sub_n

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (.not. sn_has_right_child(n) .or. .not. sn_has_right_child(n%right_child)) then
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


    function sn_level_of(n) result(ret)
        type(set_node),pointer:: n
        integer(int32):: ret

        ret = 0
        if (associated(n)) ret = n%level
    end function


    function sn_decrease_level(n) result(ret)
        type(set_node),pointer:: n, ret
        integer(int32):: should_be

        should_be = min(sn_level_of(n%left_child), sn_level_of(n%right_child)) + 1
        if (should_be < n%level) then
            n%level = should_be
            if (should_be < sn_level_of(n%right_child)) then
                n%right_child%level = should_be
            end if
        end if
        ret => n
    end function


    recursive function sn_has(n,val) result(ret)
    type(set_node),pointer:: n
        integer(int32):: val
        logical:: ret

        if (.not. associated(n)) then
            ret = .false.
            return
        else if (val > n%value) then
            ret = sn_has(n%right_child, val)
        else if (val < n%value) then
            ret = sn_has(n%left_child, val)
        else
            ret = .true.
        end if
    end function


    recursive function set_node_insert(n,val,inserted) result(ret)
        type(set_node),pointer:: n,ret
        integer(int32):: val
        logical:: inserted

        inserted = .false.
        if (.not. associated(n)) then
            allocate(ret)
            ret = set_node(value=val,level=1)
            inserted = .true.
            return
        else if (val > n%value) then
            n%right_child => set_node_insert(n%right_child, val, inserted)
        else if (val < n%value) then
            n%left_child => set_node_insert(n%left_child, val, inserted)
        end if
        n => sn_skew(n)
        n => sn_split(n)
        ret => n
    end function


    recursive function set_node_erase(n,val) result(ret)
        type(set_node),pointer:: n,ret,nn
        integer(int32):: val

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (val > n%value) then
            n%right_child => set_node_erase(n%right_child, val)
        else if (val < n%value) then
            n%left_child => set_node_erase(n%left_child, val)
        else
            if (sn_is_leaf(n)) then
                allocate(ret)
                ret => null()
                return
            else if (sn_has_left_child(n)) then
                nn => sn_predecessor(n)
                n%value = nn%value
                n%left_child => set_node_erase(n%left_child, nn%value)
            else
                nn => sn_succesor(n)
                n%value = nn%value
                n%right_child => set_node_erase(n%right_child,nn%value)
            end if
        end if
        n => sn_decrease_level(n)
        n => sn_skew(n)
        n%right_child => sn_skew(n%right_child)
        if (sn_has_left_child(n)) n%right_child%right_child => sn_skew(n%right_child%right_child)
        n => sn_split(n)
        n%right_child => sn_split(n%right_child)
        ret => n
    end function


    recursive subroutine set_node_print_debug(n,i,d)
        type(set_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call set_node_print_debug(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print'("[ ",2(i0,1x),"]")', n%value, n%level
        call set_node_print_debug(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine set_node_print(n,i,d)
        type(set_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call set_node_print(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print'(i0)', n%value
        call set_node_print(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine set_node_to_array(n,ind,ar)
    type(set_node),pointer:: n
    integer(int32),intent(inout):: ind
    integer(int32),intent(inout):: ar(:)
    integer(int32):: i

    if (.not. associated(n)) return
    i=ind
    call set_node_to_array(n%left_child,i,ar)
    ar(i) = n%value
    i=i+1
    call set_node_to_array(n%right_child,i,ar)
    ind=i
end subroutine
end module



module set_mod
    use,intrinsic :: iso_fortran_env
    use set_node_mod
    implicit none
    private
    public:: s_print, s_print_debug
    type,public:: set
        type(set_node),pointer,private:: head => null()
        integer(int32),private:: len = 0
    contains
        procedure,public:: insert => set_insert, erase => set_erase
        procedure,public:: size => set_size, to_array => set_to_array
        procedure,public:: min => set_min, max=>set_max
        procedure,public:: empty => set_empty
    end type
contains
    subroutine die(i)
        integer(int32):: i
        print'(a,i0)', "die: ",i
    end subroutine


    function set_empty(s) result(ret)
        class(set),intent(in):: s
        logical:: ret

        ret = .not. associated(s%head)
    end function


    function set_min(s) result(ret)
        class(set),intent(in):: s
        type(set_node),pointer:: n
        integer(int32):: ret

        if (s%empty()) call die(1)
        n => sn_min(s%head)
        ret = n%value
    end function


    function set_max(s) result(ret)
        class(set),intent(in):: s
        type(set_node),pointer:: n
        integer(int32):: ret

        if (s%empty()) call die(2)
        n => sn_max(s%head)
        ret = n%value
    end function


    subroutine set_insert(s,val)
        class(set),intent(inout):: s
        integer(int32),intent(in):: val
        type(set_node),pointer:: new
        logical:: inserted

        if (s%empty()) then
            allocate(new)
            new = set_node(value=val)
            s%head => new
        else
            s%head => set_node_insert(s%head,val,inserted)
        end if
        if(inserted) s%len=s%len+1
    end subroutine


    subroutine set_erase(s,val)
        class(set),intent(inout):: s
        integer(int32),intent(in):: val

        s%head => set_node_erase(s%head, val)
        s%len=max(s%len-1, 0)
    end subroutine


    subroutine s_print(s)
        type(set),intent(inout):: s
        
        call set_node_print(s%head,0,' ')
    end subroutine


    subroutine s_print_debug(s)
        type(set),intent(inout):: s
        
        print"(a,i0)", "len:",s%len
        print*, "==[value, level]=="
        call set_node_print_debug(s%head,0,' ')
    end subroutine


    function set_size(s) result(ret)
        class(set),intent(inout):: s
        integer(int32):: ret

        ret = s%len
    end function


    function set_to_array(s) result(ret)
        class(set):: s
        integer(int32):: ret(s%len),i

        i=1
        call set_node_to_array(s%head,i,ret)
    end function
end module


program main
    use,intrinsic :: iso_fortran_env
    use set_mod
    implicit none
    type(set):: s,t
    integer(int32):: i,n

    read*, n
    do i=1,n
        call s%insert(mod(i,10))
        ! call s_print_debug(s)
    end do

    do i=1,10,2
        call s%erase(i)
        call s_print_debug(s)
    end do
    print'(*(i0,1x))', s%to_array()
end program main