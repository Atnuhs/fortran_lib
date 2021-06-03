module heap_int32_mod
    ! 最小値を素早く返します。
    ! 最大値を素早く返したい場合
    !  1. 値の符号を変えてpush
    !  2. popやtopで取得した値は符号を元に戻すのを忘れずに。
    use array_mod
    use,intrinsic :: iso_fortran_env
    private 
    type, public:: heap
        integer(int32):: len
        integer(int32),allocatable:: val(:)
    contains
        procedure:: size => h_size
        procedure:: remain => h_remain
        procedure:: top => h_top
        procedure:: push => h_push
        procedure:: pop => h_pop
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

        h%len = size(v_ar)
        allocate(h%val(h%len), source=v_ar)
        do i=h%len/2,1,-1
            call heap_down(h,i)
        end do
    end function

    function h_size(h) result(ret)
        class(heap),intent(in):: h
        integer(int32):: ret

        ret = h%len
    end function

    function h_remain(h) result(ret)
        class(heap),intent(in):: h
        logical:: ret

        ret = h%size() > 0
    end function    


    subroutine h_push(h,val)
        class(heap),intent(inout):: h
        integer(int32),intent(in):: val

        h%len=h%len+1
        if (h%len >= size(h%val)) call append_array(h%val)
        h%val(h%len) = val
        call heap_up(h,h%len)
    end subroutine


    function h_top(h) result(val)
        class(heap),intent(in):: h
        integer(int32):: val

        val=h%val(1)
    end function


    function h_pop(h) result(val)
        class(heap),intent(inout):: h
        integer(int32):: val
        val=h%val(1)
        h%val(1) = h%val(h%len)
        h%len=h%len-1
        print*, 'pop', val
        call heap_down(h,1_int32)
    end function


    subroutine heap_up(h,ind)
        class(heap),intent(inout):: h
        integer(int32),value:: ind
        integer(int32):: p

        do while(ind > 1)
            p = ind/2
            if (h%val(ind) >= h%val(p)) return
            call k_swap(h,ind,p)
            ind=p
        end do
    end subroutine


    subroutine heap_down(h,ind)
        class(heap),intent(inout):: h
        integer(int32),value:: ind
        integer(int32):: c1,c2,c

        do while(ind*2 <= h%len)
            c1 = ind*2; c2 = c1+1; c=c1
            if (c2 <= h%len) c = merge(c1,c2,h%val(c1) <= h%val(c2))
            if (h%val(c) >= h%val(ind)) return
            call k_swap(h,c,ind)
            ind = c
        end do
    end subroutine


    function h_to_array(h) result(v_ar)
        class(heap),intent(inout):: h
        integer(int32):: i,v_ar(h%len)

        v_ar(:)=0
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
        type(heap),intent(inout):: h
        integer(int32),intent(in):: i1,i2
        
        call swap(h%val(i1),h%val(i2))
    contains
        subroutine swap(x,y)
            integer(int32),intent(inout):: x, y
            integer(int32):: tx, ty

            tx = x; ty = y
            x = ty; y = tx
        end subroutine
    end subroutine
end module
