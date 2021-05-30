module heap_mod
    ! 最小値を素早く返します。
    ! 最大値を素早く返したい場合
    !  1. 値の符号を変えてpush
    !  2. popやtopで取得した値は符号を元に戻すのを忘れずに。
    use,intrinsic :: iso_fortran_env
    private
    integer(int32),private,parameter:: prec=int32
    type, public:: heap
        integer(prec),private:: len
        integer(prec), allocatable,private:: val(:)
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
        integer(prec),intent(in):: n
        type(heap):: h
        h%len=0
        allocate(h%val(n))
    end function
    

    function init_h_with_ar(v_ar) result(h)
        integer(prec),intent(in):: v_ar(:)
        integer(prec):: i
        type(heap):: h

        h = heap(size(v_ar))
        do i=1,size(v_ar)
            call h_push(h,v_ar(i))
        end do
    end function

    function h_size(h) result(ret)
        class(heap),intent(in):: h
        integer(prec):: ret

        ret = h%len
    end function

    function h_remain(h) result(ret)
        class(heap),intent(in):: h
        logical:: ret

        ret = h%size() > 0
    end function    


    subroutine h_push(h,val)
        class(heap),intent(inout):: h
        integer(prec),intent(in):: val
        if (h%len+1 >= size(h%val)) call add(h)
        h%len=h%len+1
        h%val(h%len) = val
        call heap_up(h,h%len)
    end subroutine


    function h_top(h) result(val)
        class(heap),intent(in):: h
        integer(prec):: val

        val=h%val(1)
    end function


    function h_pop(h) result(val)
        class(heap),intent(inout):: h
        integer(prec):: val
        val=h%val(1)
        h%val(1) = h%val(h%len)
        h%len=h%len-1
        print*, 'pop', val
        call heap_down(h,1_prec)
    end function


    subroutine add(h)
        class(heap),intent(inout):: h
        call add_array(h%val)
    end subroutine


    subroutine add_array(ar)
        integer(prec),allocatable,intent(inout):: ar(:)
        integer(prec),allocatable:: tmp(:)
        integer(prec):: l

        l = size(ar)
        allocate(tmp(1:2*l))
        tmp(1:l) = ar(1:l)
        call move_alloc(tmp, ar)
    end subroutine


    subroutine heap_up(h,ind)
        class(heap),intent(inout):: h
        integer(prec),value:: ind
        integer(prec):: p

        do while(ind > 1)
            p = ind/2
            if (h%val(ind) >= h%val(p)) return
            call k_swap(h,ind,p)
            ind=p
        end do
    end subroutine


    subroutine heap_down(h,ind)
        class(heap),intent(inout):: h
        integer(prec),value:: ind
        integer(prec):: c1,c2,c

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
        integer(prec):: i,v_ar(h%len)

        v_ar(:)=0
        i=1
        do while(h_remain(h))
            v_ar(i) = h_pop(h)
            i=i+1
        end do
    end function


    function heap_sort(v_ar) result(ret)
        type(heap):: h
        integer(prec),intent(in):: v_ar(:)
        integer(prec):: ret(size(v_ar))

        h = heap(v_ar)
        ret = h_to_array(h)
    end function


    subroutine k_swap(h,i1,i2)
        type(heap),intent(inout):: h
        integer(prec),intent(in):: i1,i2
        
        call swap(h%val(i1),h%val(i2))
    contains
        subroutine swap(x,y)
            integer(prec),intent(inout):: x, y
            integer(prec):: tx, ty

            tx = x; ty = y
            x = ty; y = tx
        end subroutine
    end subroutine
end module