module heap_map_int_char_mod
    ! This module include
    ! double_heap_sort(key_arr, value_arr) O(NlogN)
    ! heap map
    ! 最小値を素早く返します。
    ! 最大値を素早く返したい場合
    !  1. 値の符号を変えてpush
    !  2. popやtopで取得した値は符号を元に戻すのを忘れずに。
    use,intrinsic :: iso_fortran_env
    private
    public:: key_to_char
    type val
        character(:),allocatable:: v
    end type
    type, public:: heap_map_int_char
        integer(int32),private:: len
        integer(int64), allocatable,private:: key(:)
        type(val),allocatable:: val(:)
    contains
        procedure:: size => hm_size
        procedure:: remain => hm_remain
        procedure:: top => hm_top
        procedure:: push => hm_push
        procedure:: pop => hm_pop
        procedure:: to_array => hm_to_array
    end type
    interface heap_map_int_char
        module procedure init_hm, init_hm_with_len, init_hm_with_ar
    end interface
contains
    pure elemental function to_int64(num) result(ret)
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


    function to_val(in_v) result(v)
        class(*), intent(in):: in_v
        type(val):: v

        v = val(to_char(in_v))
    end function 
    


    function init_hm() result(hm)
        type(heap_map_int_char):: hm
        hm%len=0
        allocate(hm%key(1))
        allocate(hm%val(1))
    end function


    function init_hm_with_len(n) result(hm)
        integer(int32),intent(in):: n
        type(heap_map_int_char):: hm
        hm%len=0
        allocate(hm%key(n))
        allocate(hm%val(n))
    end function
    

    function init_hm_with_ar(k_ar, v_ar) result(hm)
        class(*),intent(in):: k_ar(:), v_ar(:)
        integer(int32):: i, l
        type(heap_map_int_char):: hm

        l = min(size(k_ar), size(v_ar))
        hm = heap_map_int_char(l)
        do i=1,l
            call hm_push(hm,to_int64(k_ar(i)), to_char(v_ar(i)))
        end do
    end function


    function hm_size(hm) result(ret)
        class(heap_map_int_char):: hm
        integer(int32):: ret

        ret = hm%len
    end function


    function hm_remain(hm) result(ret)
        class(heap_map_int_char):: hm
        logical:: ret

        ret = hm%size() > 0
    end function    


    subroutine hm_push(hm,k,v)
        class(heap_map_int_char):: hm
        class(*):: k,v

        if (.not. allocated(hm%key)) then
            hm%len=0
            allocate(hm%key(1))
            allocate(hm%val(1))
        end if

        if (hm%len+1 >= size(hm%key)) call add(hm)
        hm%len=hm%len+1
        hm%key(hm%len) = to_int64(k)
        hm%val(hm%len) = to_val(v)
        call heap_up(hm,hm%len)
    end subroutine


    subroutine hm_top(hm,k,v)
        class(heap_map_int_char):: hm
        integer(int64),intent(out):: k
        character(*),intent(out):: v

        k=hm%key(1)
        v=hm%val(1)%v
    end subroutine


    subroutine hm_pop(hm,k,v)
        class(heap_map_int_char):: hm
        integer(int64),intent(out):: k
        character(*),intent(out):: v

        k=hm%key(1)
        v=hm%val(1)%v
        hm%key(1) = hm%key(hm%len)
        hm%val(1) = hm%val(hm%len)
        hm%len=hm%len-1
        call heap_down(hm,1_int32)
    end subroutine


    subroutine add(hm)
        class(heap_map_int_char):: hm
        call add_array_k(hm%key)
        call add_array_v(hm%val)
    end subroutine


    subroutine add_array_v(ar)
        type(val),allocatable,intent(inout):: ar(:)
        type(val),allocatable:: tmp(:)
        integer(int32):: l

        l = size(ar)
        allocate(tmp(1:2*l))
        tmp(1:l) = ar(1:l)
        call move_alloc(tmp, ar)
    end subroutine


    subroutine add_array_k(ar)
        integer(int64),allocatable,intent(inout):: ar(:)
        integer(int64),allocatable:: tmp(:)
        integer(int32):: l

        l = size(ar)
        allocate(tmp(1:2*l))
        tmp(1:l) = ar(1:l)
        call move_alloc(tmp, ar)
    end subroutine


    subroutine heap_up(hm,ind)
        class(heap_map_int_char):: hm
        integer(int32),value:: ind
        integer(int32):: c

        do while(ind > 1)
            c = ind/2
            if (hm%key(ind) <= hm%key(c)) return
            call kv_swap(hm,ind,c)
            ind=c 
        end do
    end subroutine


    subroutine heap_down(hm,ind)
        class(heap_map_int_char):: hm
        integer(int32),value:: ind
        integer(int32):: c1,c2,c

        do while(ind*2 <= hm%len)
            c1 = ind*2; c2 = c1+1; c=c1
            if (c2 <= hm%len) c = merge(c1,c2,hm%key(c1) >= hm%key(c2))
            if (hm%key(c) <= hm%key(ind)) return
            call kv_swap(hm,c,ind)
            ind = c
        end do
    end subroutine


    subroutine hm_to_array(hm,k_ar,v_ar)
        class(heap_map_int_char):: hm
        integer(int32):: i
        integer(int64):: k_ar(hm%len)
        character(*):: v_ar(hm%len)

        i=1
        do while(hm_remain(hm))
            call hm_pop(hm,k_ar(i), v_ar(i))
            i=i+1
        end do
    end subroutine


    subroutine heap_sort(k_ar,v_ar)
        type(heap_map_int_char):: hm
        integer(int64):: k_ar(:)
        character(*):: v_ar(:)

        hm = heap_map_int_char(k_ar,v_ar)
        call hm_to_array(hm,k_ar,v_ar)
    end subroutine


    subroutine kv_swap(hm,i1,i2)
        type(heap_map_int_char):: hm
        integer(int32):: i1,i2
        
        call swapk(hm%key(i1),hm%key(i2))
        call swapv(hm%val(i1),hm%val(i2))
    contains
        subroutine swapv(x,y)
            type(val):: x,y,t
            t=x; x=y; y=t
        end subroutine
        subroutine swapk(x,y)
            integer(int64):: x,y
            x=xor(x,y)
            y=xor(x,y)
            x=xor(x,y)
        end subroutine
    end subroutine
end module