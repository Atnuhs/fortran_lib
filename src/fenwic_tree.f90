module fenwic_tree_mod
    use vector_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    integer(int32),private,parameter:: prec = int32
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
        integer(prec):: x,ret

        ret = iand(x,-x)
    end function

    subroutine fw_push(fw,x)
        class(fenwic),intent(inout):: fw
        integer(prec),intent(in):: x
        integer(prec):: i,bx,k,n

        i=1; bx=x; n=fw%vec%l+1; k=lsb(n)
        do while(i /= k)
            bx=bx+fw%vec%array(n-i)
            i=i*2
        end do
        call fw%vec%push_back(bx)
    end subroutine


    function fw_prefix_sum(fw,i) result(ret)
        class(fenwic):: fw
        integer(prec),intent(in):: i
        integer(prec):: bi,ret

        ret=0; bi=i
        do while(bi>0)
            ret=ret+fw%vec%array(bi)
            bi=bi-lsb(bi)
        end do
    end function


    function fw_sum(fw,l,r) result(ret)
        class(fenwic):: fw
        integer(prec),intent(in):: l,r
        integer(prec):: ret

        ret = fw%prefix_sum(r) - fw%prefix_sum(l-1)
    end function


    subroutine fw_add(fw,i,x)
        class(fenwic):: fw
        integer(prec),intent(in):: i,x
        integer(prec):: bi

        bi = i
        do while (bi <= fw%vec%l)
            fw%vec%array(bi) = fw%vec%array(bi)+x
            bi=bi+lsb(bi)
        end do
    end subroutine


    subroutine fw_from_array(fw,ar)
        type(fenwic):: fw
        integer(prec):: ar(:)
        integer(prec):: n,i

        n=size(ar)
        do i=1,n
            call fw_push(fw,ar(i))
        end do
    end subroutine


    subroutine fw_debug_print(fw)
        type(fenwic):: fw

        print'(*(i0,1x))', fw%vec%array(:fw%vec%l)
    end subroutine
end module