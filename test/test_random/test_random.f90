program main
    use,intrinsic :: iso_fortran_env
    use random_mod
    implicit none
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
    print*, amax <= imax
    print*, amin >= imin
end program main