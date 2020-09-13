

recursive function qsort(m) result(res)
    integer, intent(in) :: m(:)
    integer :: res(size(m)) 
    if (size(m) <= 1) then 
        res = m
    else    
        res = [qsort( pack(m(2:), m(2:) >= m(1)) ), m(1), qsort( pack(m(2:), m(2:) < m(1)) )]
    end if
end function qsort