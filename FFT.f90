module fft_2_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    real(real64),parameter:: pi = acos(-1d0), pi2 = 2*pi
    private
    public:: fft_2, ifft_2
    public:: convolution, liner_convolution
    public:: correlation, liner_correlation
    
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
        do while(n <= size(f))
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
        do while(n <= size(f))
            n=n*2
        end do

        allocate(f2(2*n), g2(2*n), x2(2*n), source=0d0)
        f2(1:size(f)) = f(:)
        g2(1:size(g)) = g(:)

        call correlation(f2,g2,x2)
        x(:) = x2(1:size(x))
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

        call fft_2_calc(re_x(1:hn), im_x(1:hn), re_w(1:n:2), im_w(1:n:2), hn)
        call fft_2_calc(re_x(hn+1:n), im_x(hn+1:n), re_w(1:n:2), im_w(1:n:2), hn)
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
end module fft_2_mod


program debug_module_fft_2
    use,intrinsic :: iso_fortran_env
    use fft_2_mod
    implicit none
    integer(int32),parameter:: n=8, f=7
    integer(int32)::i
    real(real64):: a(n)=0,b(n)=0,c(n)=0

    a(:) = [1,2,3,4,5,6,7,8]
    b(:) = [1,2,3,4,5,6,7,8]

    c(:) = 0
    open(11,file='./fft_source.txt', status='replace')
    do i=1,n
        write(11,'(i0,1x,f0.7)')i, a(i)
    end do
    close(11)
    
    call liner_correlation(a,b,c)

    open(12,file='./fft_result.txt', status='replace')
    do i=1,n
        write(12,'(i0,1x,f0.8,1x,f0.8)')i, a(i), c(i)
    end do
    close(12)
end program debug_module_fft_2