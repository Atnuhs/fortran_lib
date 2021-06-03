module array_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    pure subroutine append_array(ar)
        ! append array size to double
        integer(int32), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        integer(int32),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:2*r))
        tmp(l:r) = ar(l:r)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine reduce_array(ar)
        ! reduce array size to half
        integer(int32), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        integer(int32),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:r/2))
        tmp(l:r/2) = ar(l:r/2)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine resize_array(arr, nl, nr, dl, dr)
        integer(int32),intent(inout),allocatable:: arr(:)
        integer(int32),intent(in):: nl,nr ! 新しい配列の両端
        integer(int32),intent(in):: dl,dr ! 引き継ぐデータの両端
        integer(int32),allocatable:: tmp(:)

        allocate(tmp(nl:nr))
        tmp(dl:dr) = arr(dl:dr)
        call move_alloc(tmp,arr)
    end subroutine resize_array
end module


module array_int64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    pure subroutine append_array(ar)
        ! append array size to double
        integer(int64), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        integer(int64),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:2*r))
        tmp(l:r) = ar(l:r)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine reduce_array(ar)
        ! reduce array size to half
        integer(int64), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        integer(int64),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:r/2))
        tmp(l:r/2) = ar(l:r/2)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine resize_array(arr, nl, nr, dl, dr)
        integer(int64),intent(inout),allocatable:: arr(:)
        integer(int32),intent(in):: nl,nr ! 新しい配列の両端
        integer(int32),intent(in):: dl,dr ! 引き継ぐデータの両端
        integer(int64),allocatable:: tmp(:)

        allocate(tmp(nl:nr))
        tmp(dl:dr) = arr(dl:dr)
        call move_alloc(tmp,arr)
    end subroutine resize_array
end module


module array_real64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    pure subroutine append_array(ar)
        ! append array size to double
        real(real64), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        real(real64),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:2*r))
        tmp(l:r) = ar(l:r)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine reduce_array(ar)
        ! reduce array size to half
        real(real64), allocatable,intent(inout):: ar(:)
        integer(int32):: l,r ! original array size
        real(real64),allocatable:: tmp(:)
        
        l = lbound(ar,1)
        r = ubound(ar,1)
        allocate(tmp(l:r/2))
        tmp(l:r/2) = ar(l:r/2)
        call move_alloc(tmp, ar)
    end subroutine


    pure subroutine resize_array(arr, nl, nr, dl, dr)
        real(real64),intent(inout),allocatable:: arr(:)
        integer(int32),intent(in):: nl,nr ! 新しい配列の両端
        integer(int32),intent(in):: dl,dr ! 引き継ぐデータの両端
        real(real64),allocatable:: tmp(:)

        allocate(tmp(nl:nr))
        tmp(dl:dr) = arr(dl:dr)
        call move_alloc(tmp,arr)
    end subroutine resize_array
end module


module array_mod
    use,intrinsic :: iso_fortran_env
    ! using int32 module
    use array_int32_mod, appendint32 => append_array
    use array_int32_mod, reduceint32 => reduce_array
    use array_int32_mod, resizeint32 => resize_array
    ! using int64 module
    use array_int64_mod, appendint64 => append_array
    use array_int64_mod, reduceint64 => reduce_array
    use array_int64_mod, resizeint64 => resize_array
    ! using real64 module
    use array_real64_mod, appendreal64 => append_array
    use array_real64_mod, reducereal64 => reduce_array
    use array_real64_mod, resizereal64 => resize_array
    implicit none
    private
    interface append_array
        module procedure:: appendint32
        module procedure:: appendint64
        module procedure:: appendreal64
    end interface

    interface reduce_array
        module procedure:: reduceint32
        module procedure:: reduceint64
        module procedure:: reducereal64
    end interface

    interface resize_array
        module procedure:: resizeint32
        module procedure:: resizeint64
        module procedure:: resizereal64
    end interface
    public:: append_array
    public:: reduce_array
    public:: resize_array
end module