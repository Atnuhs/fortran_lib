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

    subroutine fft_2(ar, ai)
        real(real64), intent(inout):: ar(:)
        real(real64), intent(inout):: ai(:)

        call fft_2_calc(ar, ai, .false.)
        call fft_2_bit_rev(ar, ai)
    end subroutine


    subroutine ifft_2(ar, ai)
        real(real64), intent(inout):: ar(:)
        real(real64), intent(inout):: ai(:)

        call fft_2_calc(ar, ai, .true.)
        call fft_2_bit_rev(ar, ai)
        ar = ar/dble(size(ar))
    end subroutine


    subroutine fft_2_calc(ar, ai, inv)
        real(real64), intent(inout):: ar(:), ai(:)
        real(real64):: theta
        real(real64):: b0r,b0i,b1r,b1i,b2r,b2i,b3r,b3i
        real(real64):: cr, ci
        real(real64):: w1r,w1i,w2r,w2i,w3r,w3i
        integer(int32):: n,j,mq,m
        integer(int32):: i0,i1,i2,i3
        logical:: inv

        print'(a)', '##### fft_2_calc'
        n = size(ar)
        m = n
        mq = rshift(m,2)
        theta = -pi2/m

        do while(mq >= 1)! half block size

            print'(a)', repeat("=",5)
            print*, m,mq
            do j=0,mq-1
                w1r = cos(theta*j)
                w1i = sin(theta*j)
                w2r = cos(2*theta*j)
                w2i = sin(2*theta*j)
                w3r = cos(3*theta*j)
                w3i = sin(3*theta*j)

                if (inv)then
                    w1i = -w1i
                    w2i = -w2i
                    w3i = -w3i
                end if

                do i0=j+1, n, m
                    i1 = i0+mq
                    i2 = i1+mq
                    i3 = i2+mq
                    print*, i0,i1,i2,i3

                    b0r = ar(i0)+ar(i2)
                    b0i = ai(i0)+ai(i2)
                    b1r = ar(i0)-ar(i2)
                    b1i = ai(i0)-ai(i2)
                    b2r = ar(i1)+ar(i3)
                    b2i = ai(i1)+ai(i3)
                    b3r = ai(i3)-ai(i1)
                    b3i = ar(i1)-ar(i3)
                    
                    ar(i0) = b0r+b2r
                    ai(i0) = b0i+b2i
                    cr = b0r-b2r
                    ci = b0i-b2i
                    ar(i1) = cr*w2r-ci*w2i
                    ai(i1) = cr*w2i+ci*w2r
                    cr = b1r+b3r
                    ci = b1i+b3i
                    ar(i2) = cr*w1r-ci*w1i
                    ai(i2) = cr*w1i+ci*w1r
                    cr = b1r-b3r
                    ci = b1i-b3i
                    ar(i3) = cr*w3r-ci*w3i
                    ai(i3) = cr*w3i+ci*w3r

                    call swap_comp(ar(i1),ai(i1),ar(i2),ai(i2))
                end do
            end do
            theta=theta*4d0
            m=mq
            mq = rshift(m,2)
        end do
        print*, mq, m
        if (m == 2) then
            print'(a)', "+++++++++++"
            do i0=1,n,2
                i1 = i0+1
                print*, i0, i1
                b0r = ar(i0)-ar(i1)
                b0i = ai(i0)-ai(i1)
                ar(i0) = ar(i0)+ar(i1)
                ai(i0) = ai(i0)+ai(i1)
                ar(i1) = b0r
                ai(i1) = b0i
            end do
        end if
    end subroutine
    
    subroutine fft_2_bit_rev(ar, ai)
        real(real64), intent(inout):: ar(:), ai(:)
        integer(int32)::n,i,j,k,h

        n=size(ar)
        h=0
        do while(lshift(1,h) < n)
            h=h+1
        end do
        do i=0,n-1
            j=0
            do k=0,h-1
                j = ieor(j, (lshift(iand(rshift(i, k), 1), h-1-k)))
            end do
            if (i < j)then
                call swap_comp(ar(i+1),ai(i+1),ar(j+1),ai(j+1))
            end if
        end do
    end subroutine

    subroutine swap_comp(xr, xi, yr, yi)
        real(real64),intent(inout):: xr,xi,yr,yi
        real(real64):: tr,ti

        tr=xr; ti=xi
        xr=yr; xi=yi
        yr=tr; yi=ti
    end subroutine


    subroutine convolution(fr,gr,xr)
        real(real64), intent(inout):: fr(:),gr(:),xr(:)
        real(real64), allocatable:: fi(:),gi(:),xi(:)
        integer(int32):: n

        n = size(fr)
        allocate(fi(n), gi(n), xi(n), source=0d0)
        call fft_2(fr,fi)
        call fft_2(gr,gi)
        xr(:) = fr(:)*gr(:) - fi(:)*gi(:)
        xi(:) = fr(:)*gi(:) + fi(:)*gr(:)
        call ifft_2(xr,xi)
    end subroutine


    subroutine liner_convolution(fr,gr,xr)
        real(real64), intent(in):: fr(:),gr(:)
        real(real64), intent(out):: xr(:)
        real(real64), allocatable:: fr2(:), gr2(:), xr2(:)
        integer(int32):: n

        n = 1
        do while(n < size(fr))
            n=lshift(n,1)
        end do

        allocate(fr2(2*n), gr2(2*n), xr2(2*n), source=0d0)
        fr2(1:size(fr)) = fr(:)
        gr2(1:size(gr)) = gr(:)

        call convolution(fr2,gr2,xr2)
        xr(:) = xr2(1:size(xr))
    end subroutine


    subroutine correlation(fr,gr,xr)
        real(real64), intent(inout):: fr(:),gr(:)
        real(real64), intent(out):: xr(:)
        real(real64), allocatable:: fi(:),gi(:),xi(:)
        integer(int32):: n

        n = size(fr)
        allocate(fi(n), gi(n), xi(n), source=0d0)

        call fft_2(fr,fi)
        call fft_2(gr,gi)
        xr(:) = fr(:)*gr(:) + fi(:)*gi(:)
        xi(:) = fr(:)*gi(:) - fi(:)*gr(:)
        call ifft_2(xr,xi)
    end subroutine


    subroutine liner_correlation(fr,gr,xr)
        real(real64), intent(in):: fr(:),gr(:)
        real(real64), intent(out):: xr(:)
        real(real64), allocatable:: fr2(:), gr2(:), xr2(:)
        integer(int32):: n

        n = 1
        do while(n < size(fr))
            n=lshift(n,1)
        end do

        allocate(fr2(2*n), gr2(2*n), xr2(2*n), source=0d0)
        fr2(1:size(fr)) = fr(:)
        gr2(1:size(gr)) = gr(:)

        call correlation(fr2,gr2,xr2)
        xr(:) = xr2(1:size(xr))
    end subroutine


    function auto_correlation_function(ar,n) result(xr)
        real(real64),intent(in):: ar(:)
        integer(int32),intent(in):: n
        real(real64):: xr(size(ar)), ar2(n*2), ai2(n*2)
        integer(int32):: n2

        n2 = 2*n
        ar2(1:size(ar)) = ar(:)
        ar2(size(ar)+1:) = 0d0
        ai2(:) = 0d0
        call fft_2(ar2, ai2)
        ar2(:) = ar2(:)*ar2(:) + ai2(:)*ai2(:)
        ai2(:) = 0d0
        call ifft_2(ar2, ai2)
        xr(:) = ar2(1:size(ar))
    end function
end module fft_2_mod