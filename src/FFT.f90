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
        real(real64):: xr, xi, wr, wi, theta
        integer(int32):: n,dist,j,i1,i2
        logical:: inv

        n = size(ar)
        dist=n/2
        theta = pi2/(dist*2)
        do while(dist >= 1)! half block size
            do j=1,dist
                wr = cos(theta*(j-1))
                wi = sin(theta*(j-1))
                if (inv) wi = -wi
                do i1=j, n-1, lshift(dist,1)
                    i2 = i1+dist
                    
                    xr = ar(i1) - ar(i2)
                    xi = ai(i1) - ai(i2)

                    ar(i1) = ar(i1) + ar(i2)
                    ai(i1) = ai(i1) + ai(i2)
                    ar(i2) = wr*xr - wi*xi
                    ai(i2) = wr*xi + wi*xr
                end do
            end do
            theta=theta*2d0
            dist=rshift(dist,1)
        end do
    end subroutine


    subroutine fft_2_bit_rev(ar, ai)
        real(real64), intent(inout):: ar(:), ai(:)
        integer(int32)::n,i,j,k,h
        real(real64):: t

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
                t = ar(i+1)
                ar(i+1)=ar(j+1)
                ar(j+1)=t

                t = ai(i+1)
                ai(i+1) = ai(j+1)
                ai(j+1)=t
            end if
        end do
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
        real(real64),allocatable:: xr(:), ar2(:), ai2(:)
        integer(int32):: n2

        n2 = 2*n
        allocate(ar2(n2))
        ar2(1:size(ar)) = ar(:); ar2(size(ar)+1:) = 0d0
        allocate(ai2(n2), source=0d0)
        call fft_2(ar2, ai2)
        ar2(:) = ar2(:)*ar2(:) + ai2(:)*ai2(:)
        ai2(:) = 0d0
        call ifft_2(ar2, ai2)
        allocate(xr(size(ar)), source=ar2(1:size(ar)))
    end function
end module fft_2_mod