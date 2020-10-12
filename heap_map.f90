module heap_map_mod
    use,intrinsic :: iso_fortran_env
    private
    type, public:: heap_map
        integer(int32),private:: len
        integer(int32), allocatable,private:: key(:)
        integer(int32), allocatable,private:: val(:)
    contains
        procedure:: push => hm_push
        procedure:: pop => hm_pop
        procedure:: remain => hm_remain
        procedure:: to_array => hm_to_array
    end type
    interface heap_map
        module procedure init_hm, init_hm_with_len, init_hm_with_ar
    end interface
contains
    function init_hm() result(hm)
        type(heap_map):: hm
        hm%len=0
        allocate(hm%key(1))
        allocate(hm%val(1))
    end function


    function init_hm_with_len(n) result(hm)
        integer(int32),intent(in):: n
        type(heap_map):: hm
        hm%len=0
        allocate(hm%key(n))
        allocate(hm%val(n))
    end function
    

    function init_hm_with_ar(k_ar, v_ar) result(hm)
        integer(int32),intent(in):: k_ar(:), v_ar(:)
        integer(int32):: i
        type(heap_map):: hm

        hm = heap_map(size(k_ar))
        do i=1,size(k_ar)
            call hm_push(hm,k_ar(i),v_ar(i))
        end do
    end function


    function hm_remain(hm) result(ret)
        class(heap_map):: hm
        logical:: ret

        ret = hm%len > 0
    end function    


    subroutine hm_push(hm,k,v)
        class(heap_map):: hm
        integer(int32):: k,v
        if (hm%len+1 >= size(hm%key)) call add(hm)
        hm%len=hm%len+1
        hm%key(hm%len) = k
        hm%val(hm%len) = v
        call heap_up(hm,hm%len)
    end subroutine


    subroutine hm_pop(hm,k,v)
        class(heap_map):: hm
        integer(int32),intent(out):: k,v
        k=hm%key(1)
        v=hm%val(1)
        hm%key(1) = hm%key(hm%len)
        hm%val(1) = hm%val(hm%len)
        hm%len=hm%len-1
        call heap_down(hm,1_int32)
    end subroutine


    subroutine add(hm)
        class(heap_map):: hm
        call add_array(hm%key)
        call add_array(hm%val)
    end subroutine


    subroutine add_array(ar)
        integer(int32),allocatable,intent(inout):: ar(:)
        integer(int32),allocatable:: tmp(:)
        integer(int32):: l

        l = size(ar)
        allocate(tmp(1:2*l))
        tmp(1:l) = ar(1:l)
        call move_alloc(tmp, ar)
    end subroutine


    subroutine heap_up(hm,ind)
        class(heap_map):: hm
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
        class(heap_map):: hm
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
        class(heap_map):: hm
        integer(int32):: i,k_ar(hm%len),v_ar(hm%len)

        i=1
        do while(hm_remain(hm))
            call hm_pop(hm,k_ar(i), v_ar(i))
            i=i+1
        end do
    end subroutine


    subroutine heap_sort(k_ar,v_ar)
        type(heap_map):: hm
        integer(int32):: k_ar(:),v_ar(:)

        hm = heap_map(k_ar,v_ar)
        call hm_to_array(hm,k_ar,v_ar)
    end subroutine


    subroutine kv_swap(hm,x,y)
        type(heap_map):: hm
        integer(int32):: x,y
        
        call swap(hm%key(x),hm%key(y))
        call swap(hm%val(x),hm%val(y))
    contains
        subroutine swap(x,y)
            integer(int32):: x,y,t
            t=x; x=y; y=t
        end subroutine
    end subroutine
end module