contains
function lcm(x,y) result(ret)
    integer(int32):: x,y,ret
    ret=x*y/gcd(x,y)
end function
recursive function gcd(x,y) result(ret)
    integer(int32):: x,y,ret

    if (mod(x,y) == 0) then
        ret = y
        return
    end if

    ret = gcd(y,mod(x,y))
end function