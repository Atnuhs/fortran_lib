module representative_value_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),parameter:: prec=real64
contains
    function mean(arr, v, sd, se) result(ave)
        ! solve:: mean of arr
        ! input:: arr
        ! output:: ave, (v, sd, se) <- optional
        real(prec),intent(in):: arr(:)
        real(prec),optional,intent(out):: v, sd, se
        real(prec):: ave, len_arr, cv, csd

        len_arr = dble(size(arr))
        ave = sum(arr)/len_arr
        cv = sum((arr-ave)*(arr-ave))/len_arr
        csd = sqrt(cv)
        if (present(v)) v = cv
        if (present(sd)) sd = csd
        if (present(se)) se = sd/sqrt(len_arr)
    end function
end module