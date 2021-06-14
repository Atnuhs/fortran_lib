module a_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type,abstract:: vec_base
        integer(int32):: l=0
    end type
    type, public, extends(vec_base):: vec_int32
        integer(int32):: l=0
        integer(int32),allocatable:: ar(:)
    contains
        procedure:: push_back
    end type

    type, public:: vec_int64
        integer(int32):: l=0
        integer(int64),allocatable:: ar(:)
    contains
        procedure:: push_back
    end type

    interface
        module subroutine push_back(vec,x)
            class(vec_base):: vec
            integer(int32):: x
            integer(int32):: l

        end subroutine

        module subroutine push_back(vec,x)
            class(vec_base):: vec
            integer(int64):: x
            integer(int32):: l

        end subroutine
    end interface
end module

submodule(a_mod) a_submodule
contains
    module procedure push_back_int32
        l=l+1
        vec%ar(l) = x
    end procedure
end submodule




program main
    use,intrinsic :: iso_fortran_env
    use a_mod
    implicit none
    
end program main