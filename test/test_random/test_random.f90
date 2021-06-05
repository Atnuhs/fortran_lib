program test_random
    use,intrinsic :: iso_fortran_env
    use random_mod
    implicit none

    print'(a)', '### random_mod test'
    print'(a)', '# random01_test'
    if (.not. random01_limits()) error stop
    print'(a)', '# randrange_limits'
    if (.not. randrange_limits()) error stop
contains
    function random01_limits() result(ret)
        integer(int32):: n,i
        real(real64),allocatable:: a(:)
        real(real64):: amin, amax
        logical:: ret

        n=10000
        allocate(a(n), source=[(random01(),i=1,n)])
        amin=a(1)
        amax=a(1)
        do i=1,n
            if (a(i)<amin) then
                amin = a(i)
            else if (a(i)>amax) then
                amax = a(i)
            end if
        end do
        ret = 0d0<=amin .and. amax<=1d0
    end function


    function randrange_limits() result(ret)
        integer(int32):: n,i
        integer(int32):: imin,imax ! 乱数の理論下限/上限
        integer(int32):: amax,amin ! 乱数の実際下限/上限
        integer(int32), allocatable:: a(:)
        logical:: ret

        n=10000
        imin=0
        imax=100
        allocate(a(n), source=[(randrange(imin,imax),i=1,n)])
        amax = a(1)
        amin = a(1)
        do i=1,n
            amax = max(amax, a(i))
            amin = min(amin, a(i))
        end do

        ret = imin<=amin .and. amax<=imax
    end function
end program test_random