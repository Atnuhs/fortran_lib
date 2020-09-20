module segment_tree_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    abstract interface
        function operator(x,y) result(ret)
            use, intrinsic:: iso_fortran_env
            integer(int32),intent(in):: x,y
            integer(int32):: ret
        end function
    end interface
    private
    public:: seg_tree, st_from_array
    type,public:: seg_tree
        integer(int32),private:: n, elnum
        integer(int32),allocatable,private:: v(:)
        procedure(operator),pointer,nopass,private:: op => null()
        integer(int32),private:: e
    contains
        procedure:: at => st_at
        procedure:: update => st_update
        procedure:: query => st_query
        procedure:: to_array => st_to_array
    end type

    interface seg_tree
        module procedure:: st_init
    end interface


contains
    function st_init(n, op, e) result(st)
        type(seg_tree):: st
        procedure(operator):: op
        integer(int32),intent(in):: n,e
        integer(int32):: x

        st%op => op
        st%e = e
        st%elnum = n
        x=1
        do while(n > x)
            x = 2*x
        end do
        st%n = x
        allocate(st%v(2*x-1), source=e)
    end function

    function st_from_array(ar, op, e) result(st)
        type(seg_tree):: st
        procedure(operator):: op
        integer(int32),intent(in):: ar(:),e
        integer(int32):: x

        st%op => op
        st%e = e
        st%elnum = size(ar)
        x=1
        do while(size(ar) > x)
            x = 2*x
        end do
        st%n = x
        allocate(st%v(2*x-1), source=e)
        st%v(x:x+st%elnum-1) = ar(:)
    end function

    
    function st_at(st,i) result(ret)
        class(seg_tree):: st
        integer(int32):: i,ret

        ret = st%v(st%n-1+i)
    end function

    subroutine st_update(st, i, x)
        class(seg_tree):: st
        integer(int32), intent(in):: i
        integer(int32):: ind
        integer(int32), intent(in):: x
        
        ind = i+st%n-1
        st%v(ind) = x
        do while(ind > 1)
            ind = ind/2
            st%v(ind) = st%op(st%v(2*ind), st%v(2*ind+1))
        end do
    end subroutine

    function st_query(st, a, b) result(ret)
        class(seg_tree):: st
        integer(int32), intent(in):: a,b
        integer(int32):: ret,l,r

        l=a+st%n-1; r=b+st%n-1
        ret = st%e
        print*, "a"
        do while (l <= r)
            if (      btest(l,0)) ret = st%op(st%v(l), ret)
            if (.not. btest(r,0)) ret = st%op(st%v(r), ret)
            l=(l+1)/2; r=(r-1)/2
        end do
    end function

    function st_to_array(st) result(ret)
        class(seg_tree):: st
        integer(int32):: ret(st%elnum)

        ret(:) = st%v(st%n:st%n+st%elnum-1)
    end function
end module


program main
    use,intrinsic :: iso_fortran_env
    use segment_tree_mod
    implicit none
    integer(int32):: n,i
    integer(int32), allocatable:: a(:)
    type(seg_tree):: st

    n=7
    allocate(a, source=[(10**i,i=0,n-1)])
    print'(*(i0,1x))', a
    st = st_from_array(a,op,10**8)
    print'(*(i0,1x))', st%to_array()
    print*, st%query(2,5)


contains
    function op(x,y) result(ret)
        integer(int32),intent(in)::x,y
        integer(int32):: ret
        ret = min(x,y)
    end function
end program main