module test_random_mod
    use,intrinsic :: iso_fortran_env
    use,intrinsic:: iso_c_binding
    implicit none
    private
contains
    function resultval(flg) result(ret)
        integer(int32):: ret
        logical,intent(in):: flg

        ret = merge(0,1,flg)
    end function
    function test_random() result(ret) bind(c, name="test_random")
        use random_mod
        implicit none
        integer(int32):: n,i,imin,imax,amax,amin
        integer(int32), allocatable:: a(:)
        integer(c_int):: ret

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
        ret = resultval(amax <= imax .and. amin >= imin)
    end function
end module