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
    use array_int32_mod, append_array_int32 => append_array
    use array_int32_mod, reduce_array_int32 => reduce_array
    use array_int32_mod, resize_array_int32 => resize_array
    ! using int64 module
    use array_int64_mod, append_array_int64 => append_array
    use array_int64_mod, reduce_array_int64 => reduce_array
    use array_int64_mod, resize_array_int64 => resize_array
    ! using real64 module
    use array_real64_mod, append_array_real64 => append_array
    use array_real64_mod, reduce_array_real64 => reduce_array
    use array_real64_mod, resize_array_real64 => resize_array
    implicit none
    private
    public:: append_array
    public:: reduce_array
    public:: resize_array
    interface append_array
        module procedure:: append_array_int32
        module procedure:: append_array_int64
        module procedure:: append_array_real64
    end interface

    interface reduce_array
        module procedure:: reduce_array_int32
        module procedure:: reduce_array_int64
        module procedure:: reduce_array_real64
    end interface

    interface resize_array
        module procedure:: resize_array_int32
        module procedure:: resize_array_int64
        module procedure:: resize_array_real64
    end interface
end module