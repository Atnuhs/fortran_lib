program main
    use,intrinsic :: iso_fortran_env
    use double_merge_sort_mod
    use random_mod
    implicit none
    if (.not. test_double_merge_sort_int32()) error stop
    if (.not. test_double_merge_sort_int64()) error stop
    if (.not. test_double_merge_sort_char()) error stop
contains
    function test_double_merge_sort_int32() result(ret)
        integer(int32):: n,i
        integer(int32), allocatable:: ar1(:), ar2(:), ar3(:)
        logical:: ret

        n = 10000
        allocate(ar1(n), source=[(randint(n), i=1,n)])
        allocate(ar2(n), source=[(i, i=1,n)])
        allocate(ar3(n), source=ar1)
        call double_merge_sort(ar1,ar2)
        ret = .true.
        do i=2,n
            ret = ret .and. ar1(i) >= ar1(i-1)
        end do
        ar3 = [(ar3(ar2(i)), i=1,n)]
        ret = ret .and. sum(ar3-ar1)==0
    end function


    function test_double_merge_sort_int64() result(ret)
        integer(int64):: n,i
        integer(int64), allocatable:: ar1(:), ar2(:), ar3(:)
        logical:: ret

        n = 10000
        allocate(ar1(n), source=[(randint(n), i=1,n)])
        allocate(ar2(n), source=[(i, i=1,n)])
        allocate(ar3(n), source=ar1)
        call double_merge_sort(ar1,ar2)
        ret = .true.
        do i=2,n
            ret = ret .and. ar1(i) >= ar1(i-1)
        end do
        ar3 = [(ar3(ar2(i)), i=1,n)]
        ret = ret .and. sum(ar3-ar1)==0
    end function


    function test_double_merge_sort_char() result(ret)
        integer(int32):: n,i
        character(:), allocatable:: ar1(:), ar3(:)
        integer(int32), allocatable:: ar2(:)
        logical:: ret
        n=6
        allocate(character(100):: ar1(n))
        ar1(1) = 'banana'
        ar1(2) = 'orange'
        ar1(3) = 'apple'
        ar1(4) = 'intel'
        ar1(5) = 'amd'
        ar1(6) = 'nvidia'
        allocate(ar2(n), source=[(i,i=1,n)])
        allocate(ar3, source=ar1)
        call double_merge_sort(ar1, ar2)
        print*, (trim(ar1(i))//' : ', i=1,n)
        ret = .true.
        do i=2,n
            ret = ret .and. ar1(i) >= ar1(i-1)
        end do
        ar3 = [(ar3(ar2(i)), i=1,n)]
        ret = ret .and. all(ar3==ar1)
    end function
end program main