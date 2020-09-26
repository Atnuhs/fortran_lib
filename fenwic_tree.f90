! use with vector_int32
module fenwic_tree_mod
    use vector_int32_mod
    use,intrinsic :: iso_fortran_env
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
        integer(int32):: x,ret

        ret = iand(x,-x)
    end function


    function fw_size(fw) result(ret)
        type(fenwic),intent(in):: fw
        integer(int32):: ret
        
        ret = vec_size(fw%vec)
    end function

    subroutine fw_push(fw,x)
        class(fenwic),intent(inout):: fw
        integer(int32),intent(in):: x
        integer(int32):: i,bx,k,n

        i=1; bx=x; n=fw_size(fw)+1; k=lsb(n)
        do while(i /= k)
            bx=bx+fw%vec%at(n-i)
            i=i*2
        end do
        call fw%vec%push_back(bx)
    end subroutine


    function fw_prefix_sum(fw,i) result(ret)
        class(fenwic):: fw
        integer(int32),intent(in):: i
        integer(int32):: bi,ret

        ret=0; bi=i
        do while(bi>0)
            ret=ret+fw%vec%at(bi)
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

        bi = i
        do while (bi <= fw_size(fw))
            fw%vec%array(bi) = fw%vec%array(bi) + x
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


    subroutine fw_debug_print(fw)
        type(fenwic):: fw

        print'(*(i0,1x))', vec_to_array(fw%vec)
    end subroutine
end module


program main
    use,intrinsic :: iso_fortran_env
    use fenwic_tree_mod
    implicit none
    type(fenwic):: fw
    integer(int32):: i,n

    n = 6
    do i=0,n
        call fw_push(fw,10**i)
        call fw_debug_print(fw)
    end do
end program main