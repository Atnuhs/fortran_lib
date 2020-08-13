! frac, ifrac, md を用意!
function comb(a,b,md) result(ret)
    integer(int64):: a,b,md
    integer(int64):: ret
    ret = mod(mod(frac(a)*ifrac(b),md)*ifrac(a-b),md)
end function

function inv(x,md) result(ret)
    integer(int64),intent(in):: x,md
    integer(int64):: ret
    ret = factorial(x,md-2,md)
end function

function factorial(a,b,md) result(ret)
    integer(int64):: a,b,md
    integer(int64):: aa,bb,ret
    aa = a
    bb = b
    ret = 1
    do while(bb > 0)
        if (mod(bb,2_8)/=0) ret=mod(ret*aa,md)
        aa=mod(aa*aa,md)
        bb=bb/2
    end do
end function