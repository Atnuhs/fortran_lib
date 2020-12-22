module fft_2_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    real(real64),parameter:: pi = acos(-1d0), pi2 = 2*pi
    private
    public:: fft_2, ifft_2
    public:: convolution, liner_convolution
    public:: correlation, liner_correlation
    public:: auto_correlation_function
    
contains
    subroutine fft_2(re_x, im_x)
        real(real64), intent(inout):: re_x(:)
        real(real64), intent(inout):: im_x(:)
        integer(int32):: n,i
        real(real64):: inv_n
        real(real64), allocatable:: re_w(:), im_w(:)

        ! n == 2^i

        n = size(re_x)
        allocate(re_w(n/2), im_w(n/2))

        inv_n = 1d0/dble(n)
        do i=1,n/2
            re_w(i) = cos(pi2*dble(i-1)*inv_n)
            im_w(i) = -sin(pi2*dble(i-1)*inv_n)
        end do

        call fft_2_calc(re_x, im_x, re_w, im_w, n)
        call fft_2_bit_rev(re_x, im_x, n)
    end subroutine


    subroutine ifft_2(re_x, im_x)
        real(real64), intent(inout):: re_x(:)
        real(real64), intent(inout):: im_x(:)
        integer(int32):: n,i
        real(real64):: inv_n
        real(real64), allocatable:: re_w(:), im_w(:)

        ! n == 2^i

        n = size(re_x)
        allocate(re_w(n/2), im_w(n/2))

        inv_n = 1d0/dble(n)
        do i=1,n/2
            re_w(i) = cos(pi2*dble(i-1)*inv_n)
            im_w(i) = sin(pi2*dble(i-1)*inv_n)
        end do

        call fft_2_calc(re_x, im_x, re_w, im_w, n)
        call fft_2_bit_rev(re_x, im_x, n)
        re_x = re_x/n
    end subroutine


    recursive subroutine fft_2_calc(re_x, im_x, re_w, im_w, n)
        integer(int32),intent(in):: n
        real(real64), intent(inout):: re_x(:), im_x(:)
        real(real64), intent(in):: re_w(:), im_w(:)
        real(real64):: re_b1, im_b1, re_b2, im_b2
        integer(int32):: hn,i,j

        if (n==1) return
        hn = n/2

        do i=1,hn
            j = hn+i
            re_b1 = re_x(i); im_b1 = im_x(i)
            re_b2 = re_x(j); im_b2 = im_x(j)

            re_x(i) = re_b1+re_b2
            im_x(i) = im_b1+im_b2

            re_x(j) = (re_b1-re_b2)*re_w(i) - (im_b1-im_b2)*im_w(i)
            im_x(j) = (re_b1-re_b2)*im_w(i) + (im_b1-im_b2)*re_w(i)
        end do

        call fft_2_calc(re_x(1:hn), im_x(1:hn), re_w(1:hn:2), im_w(1:hn:2), hn)
        call fft_2_calc(re_x(hn+1:n), im_x(hn+1:n), re_w(1:hn:2), im_w(1:hn:2), hn)
    end subroutine


    subroutine fft_2_bit_rev(re_x, im_x, n)
        integer(int32), intent(in):: n
        real(real64), intent(inout):: re_x(:), im_x(:)

        call generate_bit_rev(re_x, n)
        call generate_bit_rev(im_x, n)
    end subroutine


    recursive subroutine generate_bit_rev(bit_rev,n)
    integer(int32), intent(in):: n
        real(real64), intent(inout):: bit_rev(:)
        real(real64):: tmp(n)
        integer(int32):: hn,i,j
        hn = n/2
        if (hn > 2) then
            call generate_bit_rev(bit_rev(1:hn), hn)
            call generate_bit_rev(bit_rev(hn+1:n), hn)
        end if
        tmp(:) = bit_rev(:)
        do i=1,hn
            j=hn+i
            bit_rev(2*i-1) = tmp(i)
            bit_rev(2*i) = tmp(j)
        end do
    end subroutine


    subroutine convolution(f,g,x)
        real(real64), intent(inout):: f(:),g(:),x(:)
        real(real64), allocatable:: rf(:),rg(:),rx(:)
        integer(int32):: n

        n = size(f)
        allocate(rf(n), rg(n), rx(n), source=0d0)
        call fft_2(f,rf)
        call fft_2(g,rg)
        x(:) = f(:)*g(:) - rf(:)*rg(:)
        rx(:) = f(:)*rg(:) + rf(:)*g(:)
        call ifft_2(x,rx)
    end subroutine


    subroutine liner_convolution(f,g,x)
        real(real64), intent(in):: f(:),g(:)
        real(real64), intent(out):: x(:)
        real(real64), allocatable:: f2(:), g2(:), x2(:)
        integer(int32):: n

        n = 2
        do while(n < size(f))
            n=n*2
        end do

        allocate(f2(2*n), g2(2*n), x2(2*n), source=0d0)
        f2(1:size(f)) = f(:)
        g2(1:size(g)) = g(:)

        call convolution(f2,g2,x2)
        x(:) = x2(1:size(x))
    end subroutine


    subroutine correlation(f,g,x)
        real(real64), intent(inout):: f(:),g(:)
        real(real64), intent(out):: x(:)
        real(real64), allocatable:: rf(:),rg(:),rx(:)
        integer(int32):: n

        n = size(f)
        allocate(rf(n), rg(n), rx(n), source=0d0)

        call fft_2(f,rf)
        call fft_2(g,rg)
        rf(:) = -rf(:)
        x(:) = f(:)*g(:) - rf(:)*rg(:)
        rx(:) = f(:)*rg(:) + rf(:)*g(:)
        call ifft_2(x,rx)
    end subroutine


    subroutine liner_correlation(f,g,x)
        real(real64), intent(in):: f(:),g(:)
        real(real64), intent(out):: x(:)
        real(real64), allocatable:: f2(:), g2(:), x2(:)
        integer(int32):: n

        n = 2
        do while(n < size(f))
            n=n*2
        end do

        allocate(f2(2*n), g2(2*n), x2(2*n), source=0d0)
        f2(1:size(f)) = f(:)
        g2(1:size(g)) = g(:)

        call correlation(f2,g2,x2)
        x(:) = x2(1:size(x))
    end subroutine


    function auto_correlation_function(a,n) result(x)
        real(real64),intent(in):: a(:)
        integer(int32),intent(in):: n
        real(real64),allocatable:: x(:), x2(:), rx2(:), a2(:), ra2(:)

        allocate(a2(2*n),ra2(2*n),source=0d0)
        a2(1:size(a)) = a(:)
        call fft_2(a2,ra2)
        allocate(x2, source=a2(:)*a2(:) + ra2(:)*ra2(:))
        deallocate(a2,ra2)
        allocate(rx2(2*n), source=0d0)
        call ifft_2(x2, rx2)
        allocate(x, source=x2(1:size(a)))
    end function
end module fft_2_mod


program main
    use fft_2_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    real(real64), allocatable:: a(:),b(:),c(:)
    integer(int32):: i,n,nn

    print*, 'please input array length'
    read*, n
    allocate(a(n), source=[(real(i,kind=real64),i=1,n)])
    allocate(b(n),c(n))
    print'(a)', repeat("=",20), "input", repeat("=",20)
    print'(*(e10.3))', a(:)
    nn=2
    do while(nn < n)
        nn = nn*2
    end do

    b = auto_correlation_function(a,nn) ! 整数nnはsize(a)より大きい最小の2のべき乗
    c = auto_correlation_function_non_fft(a)
    if (n < 100) then
        print'(a)', repeat("=",20), "result-fft", repeat("=",20)
        print'(*(e10.3))', b(:)
        print'(a)', repeat("=",20), "result-non-fft", repeat("=",20)
        print'(*(e10.3))', c(:)
    end if
    print'(a)', repeat("=",20), "diff sum", repeat("=",20)
    print*, sum(b(:)-c(:))
    
contains
    function auto_correlation_function_non_fft(a) result(x)
        real(real64),intent(in):: a(:)
        real(real64),allocatable:: x(:)
        integer(int32):: t,n,tau

        n = size(a)
        allocate(x(0:n-1), source=0d0)

        do tau=0,n-1
            do t=1,n-tau
                x(tau) = x(tau) + a(t)*a(t+tau)
            end do
        end do
    end function
end program main