program main
    use,intrinsic :: iso_fortran_env
    use array_mod
    implicit none

    print'(a)', 'append_array_test'
    if (.not. append_array_test()) error stop
    print'(a)', 'reduce_array_test'
    if (.not. reduce_array_test()) error stop
    print'(a)', 'resize_array_test'
    if (.not. resize_array_test()) error stop

contains
    function append_array_test() result(ret)
        integer(int32):: n,i
        integer(int32), allocatable::ar(:)
        logical::ret

        n=12345
        allocate(ar(n))
        call append_array(ar)
        print'(*(i0,1x))', n*2, size(ar)
        ret = size(ar) == n*2
    end function


    function reduce_array_test() result(ret)
        integer(int32):: n,i
        integer(int32), allocatable::ar(:)
        logical::ret

        n=12345
        allocate(ar(n))
        call reduce_array(ar)
        print'(*(i0,1x))', n/2, size(ar)
        ret = size(ar) == n/2
    end function


    function resize_array_test() result(ret)
        integer(int32):: i,l,r,nl,nr
        integer(int32), allocatable:: ar(:)
        logical:: ret

        l = 127
        r = 242
        nl = 178
        nr = 545
        print'("l,r      ", 2(i0,1x))',l,r 
        print'("nl,nr    ", 2(i0,1x))',nl,nr 

        allocate(ar(l:r), source=[(i,i=l,r)])
        print'("before ar", 2(i0,1x))',lbound(ar,1),ubound(ar,1)
        
        call resize_array(ar,nl,nr,max(l,nl), min(r,nr))
        
        ret = lbound(ar,1)==nl
        print'("lbound(ar,1) == nl ... ", L)', ret
        
        ret = ret .and. ubound(ar,1)==nr
        print'("ubound(ar,1) == nr ... ", L)', ret

        do i=max(l,nl),min(r,nr)
            ret = ret .and. i==ar(i)
        end do
    end function
end program main