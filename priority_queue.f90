module heap_mod
    use,intrinsic :: iso_fortran_env
    private
    type, public:: heap
        integer(int64),private:: len
        integer(int64), allocatable,private:: key(:)
        integer(int64), allocatable,private:: val(:)
    contains
        procedure:: push => h_push
        procedure:: pop => h_pop
    end type
    interface heap
        module procedure init_heap, init_heap_with_len
    end interface
contains
    function init_heap() result(h)
        type(heap):: h
        h%len=0
        allocate(h%key(1))
        allocate(h%val(1))
    end function


    function init_heap_with_len(n) result(h)
        integer(int64),intent(in):: n
        type(heap):: h
        h%len=0
        allocate(h%key(n))
        allocate(h%val(n))
    end function


    subroutine h_push(h,k,v)
        class(heap):: h
        integer(int64):: k,v
        if (h%len+1 >= size(h%key)) call add(h)
        h%len=h%len+1
        h%key(h%len) = k
        h%val(h%len) = v
        call heap_up(h,h%len)
    end subroutine


    subroutine h_pop(h,k,v)
        class(heap):: h
        integer(int64),intent(out):: k,v
        k=h%key(1)
        v=h%val(1)
        h%key(1) = h%key(h%len)
        h%val(1) = h%val(h%len)
        h%len=h%len-1
        call heap_down(h,1_8)
    end subroutine


    subroutine add(h)
        class(heap):: h
        call add_array(h%key)
        call add_array(h%val)
    end subroutine


    subroutine add_array(ar)
        integer(int64),allocatable,intent(inout):: ar(:)
        integer(int64),allocatable:: tmp(:)
        integer(int64):: l

        l = size(ar)
        allocate(tmp(1:2*l))
        tmp(1:l) = ar(1:l)
        call move_alloc(tmp, ar)
    end subroutine


    subroutine heap_up(h,ind)
        class(heap):: h
        integer(int64),value:: ind
        integer(int64):: c

        do while(ind > 1)
            c = ind/2
            if (h%key(ind) <= h%key(c)) return
            call kv_swap(h,ind,c)
            ind=c 
        end do
    end subroutine


    subroutine heap_down(h,ind)
        class(heap):: h
        integer(int64),value:: ind
        integer(int64):: c1,c2,c

        do while(ind*2 <= h%len)
            c1 = ind*2; c2 = c1+1; c=c1
            if (c2 <= h%len) c = merge(c1,c2,h%key(c1) >= h%key(c2))
            if (h%key(c) <= h%key(ind)) return
            call kv_swap(h,c,ind)
            ind = c
        end do
    end subroutine


    subroutine kv_swap(h,x,y)
        type(heap):: h
        integer(int64):: x,y
        
        call swap(h%key(x),h%key(y))
        call swap(h%val(x),h%val(y))
    contains
        subroutine swap(x,y)
            integer(int64):: x,y,t
            t=x; x=y; y=t
        end subroutine
    end subroutine
end module