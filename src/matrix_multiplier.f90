module matrix_multiplier_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function matmul_mod(a,b,md) result(ret)
        integer(int64),intent(in):: a(:,:),b(:,:),md
        integer(int64),allocatable:: ret(:,:)
        integer(int32):: i,j,k,ni,nj,nk
 
        ni = size(a(1,:))
        nj = size(b(:,1))
        nk = size(a(:,1))
        allocate(ret(ni,nj), source=0_int64)
 
        do i=1,ni
            do j=1,nj
                do k=1,nk
                    ret(j,i) = mod(ret(j,i)+mod(a(k,i)*b(j,k),md),md)
                end do
            end do
        end do
    end function
 
 
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
                ret = matmul_mod(ret, ma, md)
            end if
            e = shiftr(e,1)
            ma = matmul_mod(ma,ma, md)
        end do
    end function
end module