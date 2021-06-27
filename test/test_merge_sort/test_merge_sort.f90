program main
    use,intrinsic :: iso_fortran_env
    use merge_sort_mod
    use random_mod
    implicit none
    if (.not. test_merge_sort_int32()) error stop
    if (.not. test_merge_sort_int64()) error stop
    if (.not. test_merge_sort_char()) error stop
contains
    function test_merge_sort_int32() result(ret)
        integer(int32):: n,i
        integer(int32), allocatable:: ar(:)
        logical:: ret

        n = 10000
        allocate(ar(n), source=[(randint(n), i=1,n)])
        call merge_sort(ar)
        ret = .true.
        do i=2,n
            ret = ret .and. ar(i) >= ar(i-1)
        end do
    end function


    function test_merge_sort_int64() result(ret)
        integer(int64):: n,i
        integer(int64), allocatable:: ar(:)
        logical:: ret

        n = 10000
        allocate(ar(n), source=[(randint(n), i=1,n)])
        call merge_sort(ar)
        ret = .true.
        do i=2,n
            ret = ret .and. ar(i) >= ar(i-1)
        end do
    end function


    function test_merge_sort_char() result(ret)
        integer(int32):: n,i
        character(:), allocatable:: ar(:)
        logical:: ret
        n=6
        allocate(character(100):: ar(n))
        ar(1) = 'banana'
        ar(2) = 'orange'
        ar(3) = 'apple'
        ar(4) = 'intel'
        ar(5) = 'amd'
        ar(6) = 'nvidia'
        call merge_sort(ar)
        print*, (trim(ar(i))//' : ', i=1,n)
        ret = .true.
        do i=2,n
            ret = ret .and. ar(i) >= ar(i-1)
        end do
    end function
end program main