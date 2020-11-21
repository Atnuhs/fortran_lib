module map_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: mn_insert64
    public:: mn_erase
    public:: mn_to_array
    public:: mn_print, mn_print_debug
    public:: mn_minval64, mn_maxval64
    public:: mn_has_key, mn_at64

    integer(int64),parameter:: init_val=-1_int64
    type,public:: map_node
        integer(int32):: level=1
        character(:),allocatable:: key
        integer(int64):: val=init_val
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


    function mn_minnode(n) result(ret)
        type(map_node),intent(in),pointer:: n
        type(map_node),pointer:: ret

        ret => n
        do while(mn_has_left_child(ret))
            ret => ret%left_child
        end do
    end function

    function mn_minval64(n) result(ret)
        type(map_node),intent(in),pointer:: n
        type(map_node),pointer:: tmp
        integer(int64):: ret

        ret = init_val
        if (associated(n)) then
            tmp => mn_minnode(n)
            ret = tmp%val
        end if
    end function

    function mn_maxnode(n) result(ret)
        type(map_node),pointer:: n,ret

        ret => n
        do while(mn_has_right_child(ret))
            ret => ret%right_child
        end do
    end function


    function mn_maxval64(n) result(ret)
        type(map_node),intent(in),pointer:: n
        type(map_node),pointer:: tmp
        integer(int64):: ret

        ret = init_val
        if (associated(n)) then
            tmp => mn_maxnode(n)
            ret = tmp%val
        end if
    end function


    function mn_predecessor_node(n) result(ret)
        !left -> rightend
        type(map_node),pointer:: n,ret

        ret => mn_maxnode(n%left_child)
    end function


    function mn_succesor_node(n) result(ret)
        !right -> leftend
        type(map_node),pointer:: n,ret

        ret => mn_minnode(n%right_child)
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
        character(*):: key
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


    recursive function mn_at64(n,key) result(ret)
    type(map_node),pointer:: n
        character(*):: key
        integer(int64):: ret

        if (.not. associated(n)) then
            ret = 0
            return
        else if (key > n%key) then
            ret = mn_at64(n%right_child, key)
        else if (key < n%key) then
            ret = mn_at64(n%left_child, key)
        else
            ret = n%val
        end if
    end function


    recursive function mn_insert64(n,key,val,inserted) result(ret)
        type(map_node),pointer:: n,ret
        character(*),intent(in):: key
        integer(int64),intent(in):: val
        logical:: inserted

        inserted = .false.
        if (.not. associated(n)) then
            allocate(ret)
            ret = map_node(key=key,val=val,level=1)
            inserted = .true.
            return
        else if (key > n%key) then
            n%right_child => mn_insert64(n%right_child, key, val, inserted)
        else if (key < n%key) then
            n%left_child => mn_insert64(n%left_child, key, val, inserted)
        end if
        n => mn_skew(n)
        n => mn_split(n)
        ret => n
    end function


    recursive function mn_erase(n,key) result(ret)
        type(map_node),pointer:: n,ret,nn
        character(*):: key

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (key > n%key) then
            n%right_child => mn_erase(n%right_child, key)
        else if (key < n%key) then
            n%left_child => mn_erase(n%left_child, key)
        else
            if (mn_is_leaf(n)) then
                allocate(ret)
                ret => null()
                return
            else if (mn_has_left_child(n)) then
                nn => mn_predecessor_node(n)
                n%key = nn%key
                n%left_child => mn_erase(n%left_child, nn%key)
            else
                nn => mn_succesor_node(n)
                n%key = nn%key
                n%right_child => mn_erase(n%right_child,nn%key)
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


    recursive subroutine mn_print_debug(n,i,d)
        type(map_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call mn_print_debug(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print*,"[", n%key, n%val, n%level, "]"
        call mn_print_debug(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine mn_print(n,i,d)
        type(map_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call mn_print(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print*, n%key, n%val
        call mn_print(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine mn_to_array(n,ind,k_ar,v_ar)
    type(map_node),pointer:: n
    integer(int64),intent(inout):: ind
    character(*),intent(out):: k_ar(:)
    integer(int64),intent(out):: v_ar(:)
    integer(int64):: i

    if (.not. associated(n)) return
    i=ind
    call mn_to_array(n%left_child,i,k_ar,v_ar)
    k_ar(i)=n%key; v_ar(i)=n%val
    i=i+1
    call mn_to_array(n%right_child,i,k_ar,v_ar)
    ind=i
end subroutine
end module



module map_mod
    use,intrinsic :: iso_fortran_env
    use map_node_mod
    implicit none
    private
    public:: map_print, map_print_debug
    type,public:: map
        type(map_node),pointer,private:: head => null()
        integer(int32),private:: len = 0
    contains
        procedure,public:: insert => map_insert
        procedure,public:: erase => map_erase
        procedure,public:: has_key => map_has_key
        procedure,public:: at => map_at
        procedure,public:: empty => map_empty
        procedure,public:: min => map_min, max => map_max
        procedure,public:: size => map_size
        procedure,public:: to_array => map_to_array
    end type
contains
    function val_to_int64(num) result(ret)
        class(*), intent(in) :: num
        integer(int64):: ret

        select type (num)
            type is (integer(int64))
                ret = num
            type is (integer(int32))
                ret = int(num, int64)
            class default
                ret = 0_int64
                ret = 1_int64/ret
        end select
    end function


    function key_to_char(key) result(ckey)
        class(*), intent(in) :: key
        character(50):: ctmp
        character(:),allocatable:: ckey
        
        select type (key)
            type is (integer(int64))
                write(ctmp,*) key
                ckey = trim(ctmp)
            type is (integer(int32))
                write(ctmp,*) key
                ckey = trim(ctmp)
            type is (character(*))
                ckey = key
            class default
                ckey = ''
        end select
    end function


    function map_empty(m) result(ret)
        class(map),intent(in):: m
        logical:: ret

        ret = .not. associated(m%head)
    end function


    function map_min(m) result(ret)
        class(map),intent(in):: m
        integer(int64):: ret

        ret = mn_minval64(m%head)
    end function


    function map_max(m) result(ret)
        class(map),intent(in):: m
        integer(int64):: ret

        ret = mn_minval64(m%head)
    end function


    subroutine map_insert(m,key,val)
        class(map),intent(inout):: m
        class(*),intent(in):: key,val
        character(:),allocatable:: ckey
        integer(int64):: val64
        type(map_node),pointer:: new
        logical:: inserted

        ckey = key_to_char(key); val64 = val_to_int64(val)
        if (m%empty()) then
            allocate(new)
            new = map_node(key=ckey, val=val64)
            m%head => new
        else
            m%head => mn_insert64(m%head, ckey, val64, inserted)
        end if
        if(inserted) m%len=m%len+1
    end subroutine


    subroutine map_erase(m,key)
        class(map),intent(inout):: m
        class(*),intent(in):: key


        m%head => mn_erase(m%head, key_to_char(key))
        m%len=max(m%len-1, 0)
    end subroutine


    subroutine map_print(m)
        type(map),intent(inout):: m
        
        call mn_print(m%head,0,' ')
    end subroutine


    subroutine map_print_debug(m)
        type(map),intent(inout):: m
        
        print"(a,i0)", "len:",m%len
        print*, "==[key, val, level]=="
        call mn_print_debug(m%head,0,' ')
    end subroutine


    function map_size(m) result(ret)
        class(map),intent(inout):: m
        integer(int64):: ret

        ret = m%len
    end function


    subroutine map_to_array(m,v_ar,k_ar)
        class(map):: m
        integer(int64):: i
        character(*):: k_ar(m%len)
        integer(int64):: v_ar(m%len)
        i=1
        call mn_to_array(m%head, i, k_ar, v_ar)
    end subroutine


    function map_has_key(m,key) result(ret)
        class(map),intent(in):: m
        class(*),intent(in):: key
        logical:: ret

        ret = mn_has_key(m%head, key_to_char(key))
    end function


    function map_at(m,key) result(ret)
        class(map),intent(in):: m
        class(*),intent(in):: key
        integer(int64):: ret

        ret = mn_at64(m%head, key_to_char(key))
    end function
end module