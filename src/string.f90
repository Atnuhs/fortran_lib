module string_mod
    use,intrinsic :: iso_fortran_env
    public
    integer(int32),private:: d = ichar("a") - ichar("A")
    integer(int32),private:: c0 = ichar("0")

contains
    function toupper(s) result(big_s)
        character(*),intent(in):: s
        character(1):: c(len_trim(s))
        character(len_trim(s)):: big_s

        c = transfer(trim(s),c)
        where("a"<=c .and. c<="z") c = char(ichar(c)-d)
        big_s = transfer(c,repeat(" ", size(c)))
    end function


    function tolower(s) result(big_s)
        character(*),intent(in):: s
        character(1):: c(len_trim(s))
        character(len_trim(s)):: big_s

        c = transfer(trim(s),c)
        where("A"<=c .and. c<="Z") c = char(ichar(c)+d)
        big_s = transfer(c,repeat(" ", size(c)))
    end function

    function toint(s) result(v)
        character(*),intent(in):: s
        integer(int32):: v,i
        v=0
        do i=len_trim(s),1,-1
            v=v+ichar(s(i:i))-c0
            v=v*10
        end do
    end function

    function read_grid(w,h) result(s)
        ! how to use => "s = read_grid(w,h)"
        integer(int32),intent(in):: w,h
        character(1):: s(w,h)
        character(w):: lines(h)
        integer(int32):: i

            read*, (lines(i), i=1,h)
            s = reshape([(transfer(lines(i),s(:,i)), i=1,h)], [w,h])
    end function
end module