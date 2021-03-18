module heap_map_char_char_mod
    ! This module include
    ! double_heap_sort(key_arr, value_arr) O(NlogN)
    ! heap map
    use,intrinsic :: iso_fortran_env
    private
    public:: to_char
    type elem
        character(:),allocatable:: v
    end type
    type, public:: heap_map_char_char
        integer(int32),private:: len
        type(elem),allocatable:: key(:),val(:)
    contains
        procedure:: size => hm_size
        procedure:: remain => hm_remain
        procedure:: top => hm_top
        procedure:: push => hm_push
        procedure:: pop => hm_pop
        procedure:: to_array => hm_to_array
    end type
    interface heap_map_char_char
        module procedure init_hm, init_hm_with_len, init_hm_with_ar
    end interface
contains
    function to_char(k) result(ckey)
        class(*), intent(in) :: k
        character(50):: ctmp
        character(:),allocatable:: ckey
    
        select type (k)
            type is (integer(int64))
                write(ctmp,*) k
                ckey = trim(adjustl(ctmp))
            type is (integer(int32))
                write(ctmp,*) k
                ckey = trim(adjustl(ctmp))
            type is (character(*))
                ckey = k
            class default
                ckey = ' '
        end select
    end function


    function to_elem(in_k) result(k)
        class(*), intent(in):: in_k
        type(elem):: k

        k = elem(to_char(in_k))
    end function 


    function init_hm() result(hm)
        type(heap_map_char_char):: hm
        hm%len=0
        allocate(hm%key(1))
        allocate(hm%val(1))
    end function


    function init_hm_with_len(n) result(hm)
        integer(int32),intent(in):: n
        type(heap_map_char_char):: hm
        hm%len=0
        allocate(hm%key(n))
        allocate(hm%val(n))
    end function
    

    function init_hm_with_ar(k_ar, v_ar) result(hm)
        class(*),intent(in):: k_ar(:), v_ar(:)
        integer(int32):: i, l
        type(heap_map_char_char):: hm

        l = min(size(k_ar), size(v_ar))
        hm = heap_map_char_char(l)
        do i=1,l
            call hm_push(hm,to_char(k_ar(i)), to_int64(v_ar(i)))
        end do
    end function


    function hm_size(hm) result(ret)
        class(heap_map_char_char):: hm
        integer(int32):: ret

        ret = hm%len
    end function


    function hm_remain(hm) result(ret)
        class(heap_map_char_char):: hm
        logical:: ret

        ret = hm%size() > 0
    end function    


    subroutine hm_push(hm,k,v)
        class(heap_map_char_char):: hm
        class(*):: k,v

        if (.not. allocated(hm%key)) then
            hm%len=0
            allocate(hm%key(1))
            allocate(hm%val(1))
        end if

        if (hm%len+1 >= size(hm%key)) call add(hm)
        hm%len = hm%len+1
        hm%key(hm%len) = to_elem(k)
        hm%val(hm%len) = to_elem(v)
        call heap_up(hm,hm%len)
    end subroutine


    subroutine hm_top(hm,k,v)
        class(heap_map_char_char):: hm
        character(*),intent(out):: k,v

        k=hm%key(1)%v
        v=hm%val(1)%v
    end subroutine


    subroutine hm_pop(hm,k,v)
        class(heap_map_char_char):: hm
        character(*),intent(out):: k,v

        k=hm%key(1)%v
        v=hm%val(1)%v
        hm%key(1) = hm%key(hm%len)
        hm%val(1) = hm%val(hm%len)
        hm%len=hm%len-1
        call heap_down(hm,1_int32)
    end subroutine


    subroutine add(hm)
        class(heap_map_char_char):: hm
        call add_array_elem(hm%key)
        call add_array_elem(hm%val)
    end subroutine


    subroutine add_array_elem(ar)
        type(elem),allocatable,intent(inout):: ar(:)
        type(elem),allocatable:: tmp(:)
        integer(int32):: l

        l = size(ar)
        allocate(tmp(1:2*l))
        tmp(1:l) = ar(1:l)
        call move_alloc(tmp, ar)
    end subroutine


    subroutine heap_up(hm,ind)
        class(heap_map_char_char):: hm
        integer(int32),value:: ind
        integer(int32):: c

        do while(ind > 1)
            c = ind/2
            if (hm%key(ind)%v <= hm%key(c)%v) return
            call kv_swap(hm,ind,c)
            ind=c 
        end do
    end subroutine


    subroutine heap_down(hm,ind)
        class(heap_map_char_char):: hm
        integer(int32),value:: ind
        integer(int32):: c1,c2,c

        do while(ind*2 <= hm%len)
            c1 = ind*2; c2 = c1+1; c=c1
            if (c2 <= hm%len) c = merge(c1,c2,hm%key(c1)%v >= hm%key(c2)%v)
            if (hm%key(c)%v <= hm%key(ind)%v) return
            call kv_swap(hm,c,ind)
            ind = c
        end do
    end subroutine


    subroutine hm_to_array(hm,k_ar,v_ar)
        class(heap_map_char_char):: hm
        integer(int32):: i
        character(*):: k_ar(hm%len), v_ar(hm%len)

        i=1
        do while(hm_remain(hm))
            call hm_pop(hm, k_ar(i), v_ar(i))
            i=i+1
        end do
    end subroutine


    subroutine heap_sort(k_ar,v_ar)
        type(heap_map_char_char):: hm
        character(*):: k_ar(:), v_ar(:)

        hm = heap_map_char_char(k_ar,v_ar)
        call hm_to_array(hm,k_ar,v_ar)
    end subroutine


    subroutine kv_swap(hm,i1,i2)
        type(heap_map_char_char):: hm
        integer(int32):: i1,i2
        
        call swapelem(hm%key(i1),hm%key(i2))
        call swapv(hm%val(i1),hm%val(i2))
    contains
        subroutine swapelem(x,y)
            type(elem):: x,y,t
            t=x; x=y; y=t
        end subroutine
    end subroutine
end module