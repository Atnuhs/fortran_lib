module fenwic_tree_int32_mod
    use vector_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type,public:: fenwic
        type(vector_int32):: vec
    contains
        procedure:: push => fw_push
        procedure:: prefix_sum => fw_prefix_sum
        procedure:: sec_sum => fw_sum
        procedure:: add => fw_add
    end type
contains
    function lsb(x) result(ret)
        ! x & -x => 立っている最下位ビットを返す
        integer(int32),intent(in):: x
        integer(int32):: ret

        ret = iand(x,-x)
    end function

    subroutine fw_push(fw,x)
        class(fenwic),intent(inout):: fw
        integer(int32),intent(in):: x
        integer(int32):: bx,k
        integer(int32):: i,n

        i=1
        bx=x
        n=fw%vec%l+1
        k=lsb(n)

        do while(i /= k)
            bx=bx+fw%vec%array(n-i)
            i=i*2
        end do

        call fw%vec%push_back(bx)
    end subroutine


    function fw_prefix_sum(fw,i) result(ret)
        class(fenwic):: fw
        integer(int32),intent(in):: i
        integer(int32):: ret
        integer(int32):: bi

        ret=0; bi=int(i,kind=int32)
        do while(bi>0)
            ret=ret+fw%vec%array(bi)
            bi=bi-lsb(bi)
        end do
    end function


    function fw_sum(fw,l,r) result(ret)
        class(fenwic):: fw
        integer(int32),intent(in):: l,r
        integer(int32):: ret

        ret = fw%prefix_sum(r) - fw%prefix_sum(l-1)
    end function


    subroutine fw_add(fw,i,x)
        class(fenwic):: fw
        integer(int32),intent(in):: i,x
        integer(int32):: bi

        bi = int(i,int32)
        do while (bi <= fw%vec%l)
            fw%vec%array(bi) = fw%vec%array(bi)+x
            bi=bi+lsb(bi)
        end do
    end subroutine


    subroutine fw_from_array(fw,ar)
        type(fenwic):: fw
        integer(int32):: ar(:)
        integer(int32):: n,i

        n=size(ar)
        do i=1,n
            call fw_push(fw,ar(i))
        end do
    end subroutine
end module


module fenwic_tree_int64_mod
    use vector_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type,public:: fenwic
        type(vector_int64):: vec
    contains
        procedure:: push => fw_push
        procedure:: prefix_sum => fw_prefix_sum
        procedure:: sec_sum => fw_sum
        procedure:: add => fw_add
    end type
contains
    function lsb(x) result(ret)
        ! x & -x => 立っている最下位ビットを返す
        integer(int32),intent(in):: x
        integer(int32):: ret

        ret = iand(x,-x)
    end function

    subroutine fw_push(fw,x)
        class(fenwic),intent(inout):: fw
        integer(int64),intent(in):: x
        integer(int64):: bx,k
        integer(int32):: i,n

        i=1
        bx=x
        n=fw%vec%l+1
        k=lsb(n)

        do while(i /= k)
            bx=bx+fw%vec%array(n-i)
            i=i*2
        end do
        
        call fw%vec%push_back(bx)
    end subroutine


    function fw_prefix_sum(fw,i) result(ret)
        class(fenwic):: fw
        integer(int64),intent(in):: i
        integer(int64):: ret
        integer(int32):: bi

        ret=0; bi=int(i,kind=int32)
        do while(bi>0)
            ret=ret+fw%vec%array(bi)
            bi=bi-lsb(bi)
        end do
    end function


    function fw_sum(fw,l,r) result(ret)
        class(fenwic):: fw
        integer(int64),intent(in):: l,r
        integer(int64):: ret

        ret = fw%prefix_sum(r) - fw%prefix_sum(l-1)
    end function


    subroutine fw_add(fw,i,x)
        class(fenwic):: fw
        integer(int64),intent(in):: i,x
        integer(int32):: bi

        bi = int(i,int32)
        do while (bi <= fw%vec%l)
            fw%vec%array(bi) = fw%vec%array(bi)+x
            bi=bi+lsb(bi)
        end do
    end subroutine


    subroutine fw_from_array(fw,ar)
        type(fenwic):: fw
        integer(int64):: ar(:)
        integer(int32):: n,i

        n=size(ar)
        do i=1,n
            call fw_push(fw,ar(i))
        end do
    end subroutine
end module


module fenwic_tree_real64_mod
    use vector_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type,public:: fenwic
        type(vector_real64):: vec
    contains
        procedure:: push => fw_push
        procedure:: prefix_sum => fw_prefix_sum
        procedure:: sec_sum => fw_sum
        procedure:: add => fw_add
    end type
contains
    function lsb(x) result(ret)
        ! x & -x => 立っている最下位ビットを返す
        integer(int32),intent(in):: x
        integer(int32):: ret

        ret = iand(x,-x)
    end function

    subroutine fw_push(fw,x)
        class(fenwic),intent(inout):: fw
        real(real64),intent(in):: x
        real(real64):: bx,k
        integer(int32):: i,n

        i=1
        bx=x
        n=fw%vec%l+1
        k=lsb(n)

        do while(i /= k)
            bx=bx+fw%vec%array(n-i)
            i=i*2
        end do

        call fw%vec%push_back(bx)
    end subroutine


    function fw_prefix_sum(fw,i) result(ret)
        class(fenwic):: fw
        real(real64),intent(in):: i
        real(real64):: ret
        integer(int32):: bi

        ret=0; bi=int(i,kind=int32)
        do while(bi>0)
            ret=ret+fw%vec%array(bi)
            bi=bi-lsb(bi)
        end do
    end function


    function fw_sum(fw,l,r) result(ret)
        class(fenwic):: fw
        real(real64),intent(in):: l,r
        real(real64):: ret

        ret = fw%prefix_sum(r) - fw%prefix_sum(l-1)
    end function


    subroutine fw_add(fw,i,x)
        class(fenwic):: fw
        real(real64),intent(in):: i,x
        integer(int32):: bi

        bi = int(i,int32)
        do while (bi <= fw%vec%l)
            fw%vec%array(bi) = fw%vec%array(bi)+x
            bi=bi+lsb(bi)
        end do
    end subroutine


    subroutine fw_from_array(fw,ar)
        type(fenwic):: fw
        real(real64):: ar(:)
        integer(int32):: n,i

        n=size(ar)
        do i=1,n
            call fw_push(fw,ar(i))
        end do
    end subroutine
end module



module fenwic_tree_mod
    use fenwic_tree_int32_mod, fenwic_int32 => fenwic
    use fenwic_tree_int64_mod, fenwic_int64 => fenwic
    use fenwic_tree_real64_mod, fenwic_real64 => fenwic
    implicit none
    private
    public:: fenwic_int32
    public:: fenwic_int64
    public:: fenwic_real64
end module
