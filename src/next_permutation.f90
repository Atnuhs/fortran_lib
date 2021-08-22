module next_permutation_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function next_permutation(ar) result(next_ar)
        integer(int32), intent(in):: ar(:)
        integer(int32):: next_ar(size(ar))
        integer(int32):: i,l,r,n,tmpl, tmpr

        n = size(ar)
        next_ar(:) = ar(:)
        l=-1
        
        ! search l
        do i=n-1,1,-1
            if (next_ar(i) >= next_ar(i+1)) cycle
            l = i
            exit
        end do

        if (l==-1) return
        r=-1

        ! search r
        do i=n,l+1,-1
            if (next_ar(l) >= next_ar(i)) cycle
            r = i
            exit
        end do

        ! swap lr
        tmpl = next_ar(l)
        tmpr = next_ar(r)
        next_ar(l) = tmpr
        next_ar(r) = tmpl

        ! inverse l+1:n
        next_ar(l+1:n) = next_ar(n:l+1:-1)
    end function
end module next_permutation_int32_mod



module next_permutation_int64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function next_permutation(ar) result(next_ar)
        integer(int64), intent(in):: ar(:)
        integer(int64):: next_ar(size(ar))
        integer(int64):: i,l,r,n,tmpl, tmpr

        n = size(ar)
        next_ar(:) = ar(:)
        l=-1
        
        ! search l
        do i=n-1,1,-1
            if (next_ar(i) >= next_ar(i+1)) cycle
            l = i
            exit
        end do

        if (l==-1) return
        r=-1

        ! search r
        do i=n,l+1,-1
            if (next_ar(l) >= next_ar(i)) cycle
            r = i
            exit
        end do

        ! swap lr
        tmpl = next_ar(l)
        tmpr = next_ar(r)
        next_ar(l) = tmpr
        next_ar(r) = tmpl

        ! inverse l+1:n
        next_ar(l+1:n) = next_ar(n:l+1:-1)
    end function
end module next_permutation_int64_mod



module next_permutation_character_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function next_permutation(ar) result(next_ar)
        character(*), intent(in):: ar(:)
        character(len(ar(1))):: next_ar(size(ar))
        character(len(ar(1))):: tmpl, tmpr
        integer(int32):: i,l,r,n

        n = size(ar)
        next_ar(:) = ar(:)
        l=-1
        
        ! search l
        do i=n-1,1,-1
            if (next_ar(i) >= next_ar(i+1)) cycle
            l = i
            exit
        end do

        if (l==-1) return
        r=-1

        ! search r
        do i=n,l+1,-1
            if (next_ar(l) >= next_ar(i)) cycle
            r = i
            exit
        end do

        ! swap lr
        tmpl = next_ar(l)
        tmpr = next_ar(r)
        next_ar(l) = tmpr
        next_ar(r) = tmpl

        ! inverse l+1:n
        next_ar(l+1:n) = next_ar(n:l+1:-1)
    end function
end module next_permutation_character_mod



module next_permutation_mod
    use,intrinsic :: iso_fortran_env
    use next_permutation_int32_mod, next_permutation_int32 => next_permutation
    use next_permutation_int64_mod, next_permutation_int64 => next_permutation
    use next_permutation_character_mod, next_permutation_char => next_permutation
    implicit none

    interface next_permutation
        procedure:: next_permutation_int32
        procedure:: next_permutation_int64
        procedure:: next_permutation_char
    end interface
end module