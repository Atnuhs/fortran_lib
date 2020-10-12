module heap_mod
    use,intrinsic :: iso_fortran_env
    private
    type, public:: heap
        integer(int32),private:: len
        integer(int32), allocatable,private:: val(:)
    contains
        procedure:: push => h_push
        procedure:: pop => h_pop
        procedure:: remain => h_remain
        procedure:: to_array => h_to_array
    end type
    interface heap
        module procedure init_h, init_h_with_len, init_h_with_ar
    end interface
contains
    function init_h() result(h)
        type(heap):: h
        h%len=0
        allocate(h%val(1))
    end function


    function init_h_with_len(n) result(h)
        integer(int32),intent(in):: n
        type(heap):: h
        h%len=0
        allocate(h%val(n))
    end function
    

    function init_h_with_ar(v_ar) result(h)
        integer(int32),intent(in):: v_ar(:)
        integer(int32):: i
        type(heap):: h

        h = heap(size(v_ar))
        do i=1,size(v_ar)
            call h_push(h,v_ar(i))
        end do
    end function


    function h_remain(h) result(ret)
        class(heap):: h
        logical:: ret

        ret = h%len > 0
    end function    


    subroutine h_push(h,val)
        class(heap):: h
        integer(int32):: val
        if (h%len+1 >= size(h%val)) call add(h)
        h%len=h%len+1
        h%val(h%len) = val
        call heap_up(h,h%len)
    end subroutine


    function h_pop(h) result(val)
        class(heap):: h
        integer(int32):: val
        val=h%val(1)
        h%val(1) = h%val(h%len)
        h%len=h%len-1
        call heap_down(h,1_int32)
    end function


    subroutine add(h)
        class(heap):: h
        call add_array(h%val)
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


    subroutine heap_up(h,ind)
        class(heap):: h
        integer(int32),value:: ind
        integer(int32):: c

        do while(ind > 1)
            c = ind/2
            if (h%val(ind) <= h%val(c)) return
            call k_swap(h,ind,c)
            ind=c 
        end do
    end subroutine


    subroutine heap_down(h,ind)
        class(heap):: h
        integer(int32),value:: ind
        integer(int32):: c1,c2,c

        do while(ind*2 <= h%len)
            c1 = ind*2; c2 = c1+1; c=c1
            if (c2 <= h%len) c = merge(c1,c2,h%val(c1) >= h%val(c2))
            if (h%val(c) <= h%val(ind)) return
            call k_swap(h,c,ind)
            ind = c
        end do
    end subroutine


    function h_to_array(h) result(v_ar)
        class(heap):: h
        integer(int32):: i,v_ar(h%len)

        i=1
        do while(h_remain(h))
            v_ar(i) = h_pop(h)
            i=i+1
        end do
    end function


    function heap_sort(v_ar) result(ret)
        type(heap):: h
        integer(int32),intent(in):: v_ar(:)
        integer(int32):: ret(size(v_ar))

        h = heap(v_ar)
        ret = h_to_array(h)
    end function


    subroutine k_swap(h,i1,i2)
        type(heap):: h
        integer(int32):: i1,i2
        
        call swap(h%val(i1),h%val(i2))
    contains
        subroutine swap(x,y)
            integer(int32):: x,y,t
            t=x; x=y; y=t
        end subroutine
    end subroutine
end module