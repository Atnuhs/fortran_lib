module matrix_multiplier_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function matmuler(a,e,md) result(ret)
        integer(int64),intent(in):: a(:,:),md
        integer(int64),value:: e
        integer(int64), allocatable:: ma(:,:), ret(:,:)
        integer(int32):: i


        allocate(ma, source=a)
        allocate(ret, mold=a)
        ret(:,:) = 0
        do i=1,size(ret(:,1))
            ret(i,i) = 1
        end do
        do while(e>0)
            if (btest(e,0)) then
                ret = matmul(ret, ma)
            end if
            print*, '---',e
            e = shiftr(e,1)
            ma = matmul(ma,ma)
            do i=1,size(ma(:,1))
                print'(*(i0,1x))', ma(:,i)
            end do
        end do
    end function
end module
