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
    subroutine swap(x,y)
        real(real64),intent(inout):: x,y
        real(real64):: t
        t=x; x=y; y=t
    end subroutine

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

        call fft_2_bit_rev(re_x, im_x, n)
        call fft_2_calc(re_x, im_x, re_w, im_w, n)
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

        call fft_2_bit_rev(re_x, im_x, n)
        call fft_2_calc(re_x, im_x, re_w, im_w, n)
        re_x = re_x/n
    end subroutine


    recursive subroutine fft_2_calc(re_x, im_x, re_w, im_w, n)
        integer(int32),intent(in):: n
        real(real64), intent(inout):: re_x(:), im_x(:)
        real(real64), intent(in):: re_w(:), im_w(:)
        real(real64):: re_s, im_s, re_t, im_t, re_wv, im_wv
        integer(int32):: b,j,k,i1,i2

        b=1
        print*, '--'
        do while(b < n)! half block size
            do j=1,b
                re_wv = re_w(n/(2*b)*(j))
                im_wv = im_w(n/(2*b)*(j))
                do k=0,n-1,2*b
                    print*, j+k, j+k+b
                    i1 = j+k; i2 = j+k+b
                    re_s=re_x(i1);       im_s=im_x(i1)
                    re_t=re_x(i2)*re_wv; im_t=im_x(i2)*im_wv
                    re_x(i1)=re_s+re_t;  im_x(i1)=im_s+im_t
                    re_x(i2)=re_s-re_t;  im_x(i2)=im_s-im_t
                end do
            end do
            b=b*2
        end do
    end subroutine


    subroutine fft_2_bit_rev(re_x, im_x, n)
        integer(int32), intent(in):: n
        real(real64), intent(inout):: re_x(:), im_x(:)
        integer(int32)::i,j,k,h

        h=0
        do while(lshift(1,h) < n)
            h=h+1
        end do
        do i=0,n-1
            j=0
            do k=0,h-1
                j = ieor(j, (lshift(iand(rshift(i,k),1),h-1-k)))
            end do
            if (i < j)then
                call swap(re_x(i+1),re_x(j+1))
                call swap(im_x(i+1),im_x(j+1))
            end if
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
        real(real64),allocatable:: x(:), a2(:), ra2(:)
        integer(int32):: n2,i

        n2 = 2*n
        allocate(a2, source=[a,(0d0,i=1,n2-size(a))])
        allocate(ra2(n2),source=0d0)
        call fft_2(a2,ra2)
        a2(:) = a2(:)*a2(:) + ra2(:)*ra2(:)
        ra2(:) = 0d0
        call ifft_2(a2, ra2)
        allocate(x(size(a)), source=a2(1:size(a)))
    end function
end module fft_2_mod
