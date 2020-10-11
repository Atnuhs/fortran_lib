module heap_mod
    use,intrinsic :: iso_fortran_env
    private
    type, public:: heap
        integer(int64),private:: i
        integer(int64), allocatable,private:: key(:)
        integer(int64), allocatable,private:: val(:)
    contains
        procedure:: append => append_heap
        procedure:: pop => pop_heap
    end type
    interface heap
        module procedure init_heap
    end interface
contains
    function init_heap() result(h)
        type(heap):: h
        h%i=0
        allocate(h%key(1))
        allocate(h%val(1))
    end function


    subroutine append_heap(h,k,v)
        class(heap):: h
        integer(int64):: k,v
        if (h%i+1 >= size(h%key)) call add(h)
        h%i=h%i+1
        h%key(h%i) = k
        h%val(h%i) = v
        call heap_up(h,h%i)
    end subroutine


    subroutine pop_heap(h,k,v)
        class(heap):: h
        integer(int64),intent(out):: k,v
        k=h%key(1)
        v=h%val(1)
        h%key(1) = h%key(h%i)
        h%val(1) = h%val(h%i)
        h%i=h%i-1
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


    recursive subroutine heap_up(h,self)
        class(heap):: h
        integer(int64):: self
        if (self == 1) return
        if (h%key(self) > h%key(self/2))then
            call kv_swap(h,self,self/2)
            call heap_up(h,self/2)
        end if
    end subroutine


    recursive subroutine heap_down(h,self)
        class(heap):: h
        integer(int64):: self,c1,c2

        c1 = self*2
        c2 = c1+1
        if (c2 <= h%i) then
            if (h%key(c1) >= h%key(c2)) then
                if (h%key(c1) > h%key(self)) then
                    call kv_swap(h,c1,self)
                    call heap_down(h,c1)
                end if
            else
                if (h%key(c2) > h%key(self)) then
                    call kv_swap(h,c2,self)
                    call heap_down(h,c2)
                end if
            end if
        else if (c1 <= h%i) then
            if (h%key(c1) >= h%key(self)) then
                call kv_swap(h,c1,self)
                call heap_down(h,c1)
            end if
        else
            return
        end if
    end subroutine


    subroutine kv_swap(h,x,y)
        type(heap):: h
        integer(int64):: x,y
        
        call swap(h%key(x),h%key(y))
        call swap(h%val(x),h%val(y))
    end subroutine

    subroutine swap(x,y)
        integer(int64):: x,y,t
        t=x; x=y; y=t
    end subroutine
end module