program main
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),allocatable:: ar(:)
    integer(int32):: nl,nr,l,r,i

    read*, l,r
    allocate(ar(l:r), source=[(i,i=l,r)])
    print'(*(i0,1x))', lbound(ar,1),ubound(ar,1)
    print'(*(i0,1x))', ar(l:r)
    read*, nl,nr
    call resize_array(ar,nl,nr,l,r)
    print'(*(i0,1x))', lbound(ar,1),ubound(ar,1)
    print'(*(i0,1x))', ar(nl:nr)
contains
    subroutine resize_array(arr, nl, nr, dl, dr)
        integer(int32),intent(inout),allocatable:: arr(:)
        integer(int32),intent(in):: nl,nr ! 新しい配列の両端
        integer(int32),intent(in):: dl,dr ! 引き継ぐデータの両端
        integer(int32),allocatable:: tmp(:)

        allocate(tmp(nl:nr))
        print'(*(i0,1x))', lbound(arr,1),ubound(arr,1)
        tmp(dl:dr) = arr(dl:dr)
        call move_alloc(tmp,arr)
    end subroutine resize_array
end program main