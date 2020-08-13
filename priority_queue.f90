module heap_mod
    use,intrinsic :: iso_fortran_env
    type heap
        integer(int64):: i
        integer(int64), allocatable:: key(:)
        integer(int64), allocatable:: val(:)
    contains
        procedure:: append => append_heap
        procedure:: pop => pop_heap
    end type
    interface heap
        module procedure init_heap
    end interface
    private
    public:: heap
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

    subroutine add_array(x)
        integer(int64):: l
        integer(int64), allocatable:: tmp(:),x(:)
        l = size(x(:))
        allocate(tmp(l))
        tmp(:) = x(:)
        deallocate(x)
        allocate(x(l*2))
        x(1:l) = tmp(:)
        deallocate(tmp)
    end subroutine

    recursive subroutine heap_up(h,self)
        class(heap):: h
        integer(int64):: self
        if (self == 1) return

        if (h%key(self) > h%key(self/2))then
            call swap(h%key(self),h%key(self/2))
            call swap(h%val(self),h%val(self/2))
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
                    call swap(h%key(c1),h%key(self))
                    call swap(h%val(c1),h%val(self))
                    call heap_down(h,c1)
                end if
            else
                if (h%key(c2) > h%key(self)) then
                    call swap(h%key(c2),h%key(self))
                    call swap(h%val(c2),h%val(self))
                    call heap_down(h,c2)
                end if
            end if
        else if (c1 <= h%i) then
            if (h%key(c1) >= h%key(self)) then
                call swap(h%key(c1),h%key(self))
                call swap(h%val(c1),h%val(self))
                call heap_down(h,c1)
            end if
        else
            return
        end if
    end subroutine

    subroutine swap(x,y)
        integer(int64):: x,y,t
        t=x; x=y; y=t
    end subroutine
end module