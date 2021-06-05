program test_random
    use,intrinsic :: iso_fortran_env
    implicit none

    print'(a)', 'random_mod test'
    call test_random_sub1
contains
    subroutine test_random_sub1()
        use random_mod
        integer(int32):: n,i,imin,imax,amax,amin
        integer(int32), allocatable:: a(:)

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
        print'(*(i0,1x))', amax, amin
        print'(*(i0,1x))', imin, imax
    end subroutine
end program test_random