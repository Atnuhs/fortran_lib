module math_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    ! integer:: int32=4, int64=8, real32=4,real64=8
contains
    function calc_v(ar) result(v)
        real(real64),intent(in):: ar(:)
        real(real64):: v
        real(real64):: ave

        ave = sum(ar)/dble(size(ar))
        v = sum((ar-ave)**2)/dble(size(ar))
    end function

    function calc_average(ar) result(ave)
        real(real64),intent(in):: ar(:)
        real(real64):: ave

        ave = sum(ar)/dble(size(ar))
    end function
end module