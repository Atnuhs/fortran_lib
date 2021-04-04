module z_algorithm_mod
    use,intrinsic :: iso_fortran_env
    implicit none

contains
    function zalgo(str) result(ret)
        ! input => str
        ! output => ret  O(N)
        ! ex) str => a  a b a a b a a b a a b
        !     ret => 12 1 0 9 1 0 6 1 0 3 1 0
        ! ret(i) => 文字列の先頭からと、i文字目からで、何文字分共通か。
        character(*):: str
        integer(int32):: n,i,same, from, last
        integer(int32), allocatable:: ret(:)

        n = len_trim(str)
        allocate(ret(n),source=0)
        from=0; last=0
        ret(1) = n
        do i=2,n
            same = ret(i)
            if (from /= 0) then
                same = min(ret(i-from+1), last-i)
                same = max(same,0)
                ret(i) = same
            end if
            do while(str(same+1:same+1) == str(same+i:same+i))
                same=same+1
                if (same+i > n) exit
            end do
            ret(i) = same
            if (last < i+same) then
                last = i+same-1
                from = i
            end if
        end do
    end function
end module