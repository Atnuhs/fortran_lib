! procedure,public:: insert(i,v)   -> subroutine
! procedure,public:: erase(i)      -> subroutine
! procedure,public:: push_back(v)  -> subroutine
! procedure,public:: push_front(v) -> subroutine
! procedure,public:: pop(i)        -> vec[i] & remove vec[i]
! procedure,public:: pop_back()    -> vec[last] & remove vec[i]
! procedure,public:: pop_front()   -> vec[top] & remove vec[i]
! procedure,public:: at(i)         -> vec[i] 
! procedure,public:: size          -> size(vec)
! procedure,public:: to_array      -> arr(sorted)
! procedure,public:: update(i,v)   -> subroutine
! public:: vt_print
! public:: vt_print_debug



module vec_tree_node_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public:: vtn_insert, vtn_push_back
    public:: vtn_update
    public:: vtn_erase
    public:: vtn_to_array
    public:: vtn_print, vtn_print_debug
    public:: vtn_at
    type,public:: vec_tree_node
        integer(int32):: level=1,tree_size=1
        integer(int32):: value
        type(vec_tree_node),pointer:: left_child => null()
        type(vec_tree_node),pointer:: right_child => null()
    end type
contains
    function vtn_has_left_child(n) result(ret)
        type(vec_tree_node),pointer:: n
        logical:: ret

        ret = associated(n%left_child)
    end function


    function vtn_has_right_child(n) result(ret)
        type(vec_tree_node),pointer:: n
        logical:: ret

        ret = associated(n%right_child)
    end function


    function vtn_is_leaf(n) result(ret)
        type(vec_tree_node),pointer:: n
        logical:: ret

        ret = .not. (vtn_has_left_child(n) .or. vtn_has_right_child(n))
    end function


    function vtn_predecessor(n) result(ret)
        !left -> rightend
        type(vec_tree_node),pointer:: n,ret

        ret => n%left_child
        do while(vtn_has_right_child(ret))
            ret => ret%right_child
        end do
    end function


    function vtn_succesor(n) result(ret)
        !right -> leftend
        type(vec_tree_node),pointer:: n,ret

        ret => n%right_child
        do while(vtn_has_left_child(ret))
            ret => ret%left_child
        end do
    end function


    subroutine vtn_size_update(n)
        type(vec_tree_node),pointer:: n
        integer(int32):: old_size, new_size

        old_size = n%tree_size
        new_size = vtn_left_size_of(n)+vtn_right_size_of(n) + 1
        n%tree_size = new_size
    end subroutine


    function vtn_left_size_of(n) result(ret)
        type(vec_tree_node),pointer:: n
        integer(int32):: ret

        ret = 0
        if (vtn_has_left_child(n)) ret = n%left_child%tree_size
    end function


    function vtn_right_size_of(n) result(ret)
        type(vec_tree_node),pointer:: n
        integer(int32):: ret

        ret = 0
        if (vtn_has_right_child(n)) ret = n%right_child%tree_size
    end function


    function vtn_ind_at(n) result(ret)
        type(vec_tree_node),pointer:: n
        integer(int32):: ret

        ret = vtn_left_size_of(n)+1
    end function


    function vtn_skew(n) result(ret)
        type(vec_tree_node),pointer:: n, ret, sub_n

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (.not. vtn_has_left_child(n)) then
            ret => n
        else if (n%level == n%left_child%level) then
            sub_n => n%left_child
            n%left_child => sub_n%right_child
            sub_n%right_child => n
            ret => sub_n
        else
            ret => n
        end if
        call vtn_size_update(n)
        call vtn_size_update(ret)
    end function


    function vtn_split(n) result(ret)
        type(vec_tree_node),pointer:: n,ret,sub_n

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (.not. vtn_has_right_child(n) .or. .not. vtn_has_right_child(n%right_child)) then
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
        call vtn_size_update(n)
        call vtn_size_update(ret)
    end function


    recursive function vtn_push_back(n,i,val) result(ret)
        type(vec_tree_node),pointer:: n,ret
        integer(int32):: i
        integer(int32):: val

        if (.not. associated(n)) then
            allocate(ret)
            ret = vec_tree_node(value=val)
            return
        else
            n%right_child => vtn_push_back(n%right_child,i,val)
        end if
        n => vtn_skew(n)
        n => vtn_split(n)
        ret => n
    end function


    function vtn_level_of(n) result(ret)
        type(vec_tree_node),pointer:: n
        integer(int32):: ret

        ret = 0
        if (associated(n)) ret = n%level
    end function


    function vtn_decrease_level(n) result(ret)
        type(vec_tree_node),pointer:: n, ret
        integer(int32):: should_be

        should_be = min(vtn_level_of(n%left_child), vtn_level_of(n%right_child)) + 1
        if (should_be < n%level) then
            n%level = should_be
            if (should_be < vtn_level_of(n%right_child)) then
                n%right_child%level = should_be
            end if
        end if
        ret => n
    end function


    recursive function vtn_insert(n,i,val) result(ret)
        type(vec_tree_node),pointer:: n,ret
        integer(int32):: i
        integer(int32):: val

        if (.not. associated(n)) then
            allocate(ret)
            ret = vec_tree_node(value=val,level=1)
            return
        end if
        if (i == vtn_ind_at(n)) then
            n%left_child => vtn_push_back(n%left_child,i,val)
        else if (i > vtn_ind_at(n)) then
            n%right_child => vtn_insert(n%right_child, i-(vtn_ind_at(n)),val)
        else if (i < vtn_ind_at(n)) then
            n%left_child => vtn_insert(n%left_child,i,val)
        end if
        n => vtn_skew(n)
        n => vtn_split(n)
        ret => n
    end function


    recursive function vtn_erase(n,i,ev) result(ret)
        type(vec_tree_node),pointer:: n,ret,nn
        integer(int32):: i
        integer(int32),optional:: ev

        if (.not. associated(n)) then
            allocate(ret)
            ret => null()
            return
        else if (vtn_ind_at(n) > i) then
            n%left_child => vtn_erase(n%left_child, i,ev)
        else if (vtn_ind_at(n) < i) then
            n%right_child => vtn_erase(n%right_child, i-vtn_ind_at(n),ev)
        else
            if (present(ev)) ev = n%value
            if (vtn_is_leaf(n)) then
                allocate(ret)
                ret => null()
                return
            else if (vtn_has_left_child(n)) then
                nn => vtn_predecessor(n)
                n%left_child => vtn_erase(n%left_child,nn%tree_size)
                n%value = nn%value
            else
                nn => vtn_succesor(n)
                n%right_child => vtn_erase(n%right_child,nn%tree_size)
                n%value = nn%value
            end if
        end if
        n => vtn_decrease_level(n)
        n => vtn_skew(n)
        n%right_child => vtn_skew(n%right_child)
        if (vtn_has_left_child(n)) n%right_child%right_child => vtn_skew(n%right_child%right_child)
        n => vtn_split(n)
        n%right_child => vtn_split(n%right_child)
        ret => n
    end function


    recursive subroutine vtn_print_debug(n,i,d)
        type(vec_tree_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call vtn_print(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print'("[ ",3(i0,1x),"]")', n%value, n%level, n%tree_size
        call vtn_print(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine vtn_print(n,i,d)
        type(vec_tree_node),pointer:: n
        integer(int32):: i,j
        character(1):: d

        if (.not. associated(n)) return
        call vtn_print(n%right_child,i+1,"r")
        do j=1,i; write(*,'(a)',advance="no") '  '; end do
        if (d == "r") then
            write(*,'(a)',advance="no") "┌ "
        else if (d == "l") then
            write(*,'(a)',advance="no") "└ "
        else
            write(*,'(a)',advance="no") "  "
        end if
        print'(i0)', n%value
        call vtn_print(n%left_child,i+1,"l")
    end subroutine


    recursive subroutine vtn_to_array(n,ind,ar)
        type(vec_tree_node),pointer:: n
        integer(int32),intent(inout):: ind
        integer(int32),intent(inout):: ar(:)
        integer(int32):: i

        if (.not. associated(n)) return
        i=ind
        call vtn_to_array(n%left_child,i,ar)
        ar(i) = n%value
        i=i+1
        call vtn_to_array(n%right_child,i,ar)
        ind=i
    end subroutine


    recursive function vtn_node_at(n,ind) result(ret)
        type(vec_tree_node),pointer:: n
        integer(int32):: ind
        type(vec_tree_node),pointer:: ret

        ret => null()
        if (.not. associated(n)) return
        if (vtn_ind_at(n) > ind) then
            ret => vtn_node_at(n%left_child, ind)
        else if (vtn_ind_at(n) < ind) then
            ret => vtn_node_at(n%right_child, ind-vtn_ind_at(n))
        else ! vtn_ind_at(n) == ind
            ret => n
        end if
    end function


    function vtn_at(n,ind) result(ret)
        type(vec_tree_node),pointer:: n, retn
        integer(int32):: ind
        integer(int32):: ret


        retn => vtn_node_at(n,ind)
        ret = 0
        if (.not. associated(retn)) return
        ret = retn%value
    end function


    function vtn_update(n,ind,val) result(ret)
        type(vec_tree_node),pointer:: n, tn, ret
        integer(int32):: ind, val

        tn => vtn_node_at(n,ind)
        tn%value = val
        ret => n
    end function
end module



module vec_tree_mod
    use,intrinsic :: iso_fortran_env
    use vec_tree_node_mod
    implicit none
    private
    public:: vt_print, vt_print_debug
    type,public:: vectree
        type(vec_tree_node),pointer,private:: head => null()
        integer(int32),private:: len = 0
    contains
        procedure,public:: insert => vt_insert, erase => vt_erase
        procedure,public:: push_back => vt_push_back, push_front => vt_push_front
        procedure,public:: pop => vt_pop, pop_back => vt_pop_back, pop_front => vt_pop_front
        procedure,public:: at => vt_value_at, size => vt_size, to_array => vt_to_array
        procedure,public:: update => vt_update
    end type
contains
    subroutine vt_insert(vt,i,val)
        class(vectree):: vt
        type(vec_tree_node),pointer:: new
        integer(int32):: i
        integer(int32):: val

        if (i < 1 .or. vt%len+1 < i) then
            print'(a)', "inserting index is too big"
            print'(a)', "index, vectree length"
            print'(*(i0,1x))', i, vt%len
            stop
        else if (.not. associated(vt%head)) then
            allocate(new)
            new = vec_tree_node(value=val)
            vt%head => new
        else if (i == vt%len+1) then
            vt%head => vtn_push_back(vt%head,i,val)
        else
            vt%head => vtn_insert(vt%head,i,val)
        end if
        vt%len=vt%len+1
    end subroutine


    subroutine vt_update(vt,i,val)
        class(vectree):: vt
        integer(int32):: i,val

        if (i < 1 .or. vt%len < i) then
            print'(a)', "update index is too big"
            print'(a)', "index, vectree length"
            print'(*(i0,1x))', i, vt%len
            stop
        end if
        vt%head => vtn_update(vt%head,i,val)
    end subroutine


    subroutine vt_push_back(vt,val)
        class(vectree):: vt
        integer(int32):: val

        call vt_insert(vt,vt%len+1,val)
    end subroutine


    subroutine vt_push_front(vt,val)
        class(vectree):: vt
        integer(int32):: val

        call vt_insert(vt,1,val)
    end subroutine


    function vt_pop(vt,i) result(erased_value)
        class(vectree):: vt
        integer(int32):: i,erased_value

        if (i < 1 .or. vt%len < i) then
            print'(a)', "remove index is too big"
            print'(a)', "index, vectree length"
            print'(*(i0,1x))', i, vt%len
            stop
        end if
        vt%head => vtn_erase(vt%head,i,erased_value)
        vt%len=vt%len-1
    end function


    subroutine vt_erase(vt,i)
        class(vectree):: vt
        integer(int32):: i,tmp

        tmp = vt%pop(i)
    end subroutine


    function vt_pop_back(vt) result(ret)
        class(vectree):: vt
        integer(int32):: ret

        ret = vt%pop(vt%len)
    end function


    function vt_pop_front(vt) result(ret)
        class(vectree):: vt
        integer(int32):: ret

        ret = vt%pop(1)
    end function


    function vt_value_at(vt,i) result(ret)
        class(vectree):: vt
        integer(int32):: i
        integer(int32):: ret

        ret = vtn_at(vt%head,i)
    end function


    subroutine vt_print(vt)
        type(vectree):: vt
        
        call vtn_print(vt%head,0,' ')
    end subroutine


    subroutine vt_print_debug(vt)
        type(vectree):: vt
        
        print*, "==[value, level, tree_size]=="
        call vtn_print_debug(vt%head,0,' ')
    end subroutine


    function vt_to_array(vt) result(ret)
        class(vectree):: vt
        integer(int32):: ret(vt%len),i
        i=1
        call vtn_to_array(vt%head,i,ret)
    end function


    function vt_size(vt) result(ret)
        class(vectree):: vt
        integer(int32):: ret

        ret = vt%len
    end function
end module