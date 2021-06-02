! fenwic treeでバグらない要注意
module vector_int32_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: vector
        integer(int32),allocatable:: array(:)
        integer(int32),private:: l=0
    contains
        procedure:: push_back=>vec_push_back, insert=>vec_insert
        procedure:: pop_back=>vec_pop_back, pop=>vec_pop, erase => vec_erase
        procedure:: at=>vec_at, back=>vec_back, head=>vec_head
        procedure:: to_array => vec_to_array, size => vec_size
        procedure:: max => vec_max, min => vec_min
        procedure:: extrema => vec_extrema
        procedure:: reverse => vec_reverse
        procedure:: update => vec_update
    end type
    interface vector
        module procedure vec_init_from_len, vec_init_from_array
    end interface
contains
    function vec_init_from_len(n,ini) result(ret)
        type(vector):: ret
        integer(int32):: n
        integer(int32),optional:: ini
        integer(int32):: x

        x=0
        if (present(ini)) x=ini
        allocate(ret%array(n), source=x)
        ret%l = n
    end function


    function vec_init_from_array(ar) result(ret)
        type(vector):: ret
        integer(int32):: ar(:)

        allocate(ret%array, source=ar)
        ret%l = size(ar)
    end function


    pure function vec_size(vec) result(ret)
        class(vector),intent(in):: vec
        integer(int32):: ret

        ret = vec%l
    end function

    pure function vec_max(vec) result(ret)
        class(vector),intent(in):: vec
        integer(int32):: ret

        ret = maxval(vec%array)
    end function


    pure function vec_min(vec) result(ret)
        class(vector),intent(in):: vec
        integer(int32):: ret

        ret = minval(vec%array)
    end function

    
    pure subroutine vec_extrema(vec, mi, ma) 
        class(vector),intent(in):: vec
        integer(int32),intent(out):: mi, ma
        integer(int32):: i

        mi = vec%array(1)
        ma = vec%array(1)
        do i=1,size(vec%array)
            mi = min(mi, vec%array(i))
            ma = max(ma, vec%array(i))
        end do
    end subroutine

    pure subroutine check_array_allocation(vec)
        type(vector),intent(inout):: vec

        if (.not. allocated(vec%array)) allocate(vec%array(1))
    end subroutine


    function vec_at(vec,i) result(ret)
        class(vector),intent(inout):: vec
        integer(int32):: i
        integer(int32):: ret

        call check_array_allocation(vec)
        ret = vec%array(i)
    end function


    subroutine vec_update(vec, i, x)
        class(vector),intent(inout):: vec
        integer(int32),intent(in):: i, x

        call check_array_allocation(vec)
        vec%array(i) = x
    end subroutine


    function vec_back(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int32):: ret
        
        ret = vec%at(vec%l)
    end function


    function vec_head(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int32):: ret
        
        ret = vec%at(1_int32)
    end function


    pure subroutine vec_append_array(vec,l,r)
        type(vector),intent(inout):: vec
        integer(int32),intent(in):: l,r
        integer(int32),allocatable:: tmp(:)
        
        allocate(tmp(l:2*r))
        tmp(l:r) = vec%array(l:r)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine vec_reduce_array(vec,l,r)
        type(vector),intent(inout):: vec
        integer(int32),intent(in):: l,r
        integer(int32),allocatable:: tmp(:)
        
        allocate(tmp(l:r/2))
        tmp(l:r/2) = vec%array(l:r/2)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine check_allocation_size(vec)
        type(vector),intent(inout):: vec
        integer(int32):: len_alloc

        call check_array_allocation(vec)
        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) then
            call vec_append_array(vec,1_int32,len_alloc)
        else if (vec%l <= len_alloc/2) then
            call vec_reduce_array(vec,1_int32,len_alloc)
        end if
    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector),intent(inout):: vec
        integer(int32),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    pure subroutine vec_insert(vec,i,v)
        class(vector),intent(inout):: vec
        integer(int32),intent(in)::i, v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(i+1:vec%l+1) = vec%array(i:vec%l)
        vec%array(i) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int32):: ret

        ret = vec%back()
        vec%l=vec%l-1
        call check_allocation_size(vec)
    end function


    function vec_pop(vec,i) result(ret)
        class(vector),intent(inout):: vec
        integer(int32),intent(in):: i
        integer(int32):: ret

        ret = vec%at(i)
        vec%l=vec%l-1
        vec%array(i:vec%l) = vec%array(i+1:vec%l+1)
        call check_allocation_size(vec)
    end function


    subroutine vec_erase(vec,i)
        class(vector):: vec
        integer(int32),intent(in):: i
        integer(int32):: dmp

        dmp = vec%pop(i)
    end subroutine


    subroutine vec_reverse(vec)
        class(vector):: vec
        integer(int32):: i, j, n, t1, t2

        n = vec%size()
        do i=1,n/2
            j = n+1-j
            t1 = vec%array(i)
            t2 = vec%array(j)
            vec%array(j) = t1
            vec%array(i) = t2
        end do
    end subroutine


    function vec_to_array(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int32):: ret(1:vec%l)

        call check_array_allocation(vec)
        ret = vec%array(1:vec%l)
    end function
end module


! fenwic treeでバグらない要注意
module vector_int64_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    type,public:: vector
        integer(int64),allocatable:: array(:)
        integer(int64),private:: l=0
    contains
        procedure:: push_back=>vec_push_back, insert=>vec_insert
        procedure:: pop_back=>vec_pop_back, pop=>vec_pop, erase => vec_erase
        procedure:: at=>vec_at, back=>vec_back, head=>vec_head
        procedure:: to_array => vec_to_array, size => vec_size
        procedure:: max => vec_max, min => vec_min
        procedure:: extrema => vec_extrema
        procedure:: reverse => vec_reverse
        procedure:: update => vec_update
    end type
    interface vector
        module procedure vec_init_from_len, vec_init_from_array
    end interface
contains
    function vec_init_from_len(n,ini) result(ret)
        type(vector):: ret
        integer(int64):: n
        integer(int64),optional:: ini
        integer(int64):: x

        x=0
        if (present(ini)) x=ini
        allocate(ret%array(n), source=x)
        ret%l = n
    end function


    function vec_init_from_array(ar) result(ret)
        type(vector):: ret
        integer(int64):: ar(:)

        allocate(ret%array, source=ar)
        ret%l = size(ar)
    end function


    pure function vec_size(vec) result(ret)
        class(vector),intent(in):: vec
        integer(int64):: ret

        ret = vec%l
    end function

    pure function vec_max(vec) result(ret)
        class(vector),intent(in):: vec
        integer(int64):: ret

        ret = maxval(vec%array)
    end function


    pure function vec_min(vec) result(ret)
        class(vector),intent(in):: vec
        integer(int64):: ret

        ret = minval(vec%array)
    end function

    
    pure subroutine vec_extrema(vec, mi, ma) 
        class(vector),intent(in):: vec
        integer(int64),intent(out):: mi, ma
        integer(int64):: i

        mi = vec%array(1)
        ma = vec%array(1)
        do i=1,size(vec%array)
            mi = min(mi, vec%array(i))
            ma = max(ma, vec%array(i))
        end do
    end subroutine

    pure subroutine check_array_allocation(vec)
        type(vector),intent(inout):: vec

        if (.not. allocated(vec%array)) allocate(vec%array(1))
    end subroutine


    function vec_at(vec,i) result(ret)
        class(vector),intent(inout):: vec
        integer(int64):: i
        integer(int64):: ret

        call check_array_allocation(vec)
        ret = vec%array(i)
    end function


    subroutine vec_update(vec, i, x)
        class(vector),intent(inout):: vec
        integer(int64),intent(in):: i, x

        call check_array_allocation(vec)
        vec%array(i) = x
    end subroutine


    function vec_back(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int64):: ret
        
        ret = vec%at(vec%l)
    end function


    function vec_head(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int64):: ret
        
        ret = vec%at(1_int64)
    end function


    pure subroutine vec_append_array(vec,l,r)
        type(vector),intent(inout):: vec
        integer(int64),intent(in):: l,r
        integer(int64),allocatable:: tmp(:)
        
        allocate(tmp(l:2*r))
        tmp(l:r) = vec%array(l:r)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine vec_reduce_array(vec,l,r)
        type(vector),intent(inout):: vec
        integer(int64),intent(in):: l,r
        integer(int64),allocatable:: tmp(:)
        
        allocate(tmp(l:r/2))
        tmp(l:r/2) = vec%array(l:r/2)
        call move_alloc(tmp, vec%array)
    end subroutine


    pure subroutine check_allocation_size(vec)
        type(vector),intent(inout):: vec
        integer(int64):: len_alloc

        call check_array_allocation(vec)
        len_alloc = size(vec%array)
        if (vec%l >= len_alloc) then
            call vec_append_array(vec,1_int64,len_alloc)
        else if (vec%l <= len_alloc/2) then
            call vec_reduce_array(vec,1_int64,len_alloc)
        end if
    end subroutine


    pure subroutine vec_push_back(vec, v)
        class(vector),intent(inout):: vec
        integer(int64),intent(in):: v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(vec%l) = v
    end subroutine


    pure subroutine vec_insert(vec,i,v)
        class(vector),intent(inout):: vec
        integer(int64),intent(in)::i, v

        vec%l=vec%l+1
        call check_allocation_size(vec)
        vec%array(i+1:vec%l+1) = vec%array(i:vec%l)
        vec%array(i) = v
    end subroutine


    function vec_pop_back(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int64):: ret

        ret = vec%back()
        vec%l=vec%l-1
        call check_allocation_size(vec)
    end function


    function vec_pop(vec,i) result(ret)
        class(vector),intent(inout):: vec
        integer(int64),intent(in):: i
        integer(int64):: ret

        ret = vec%at(i)
        vec%l=vec%l-1
        vec%array(i:vec%l) = vec%array(i+1:vec%l+1)
        call check_allocation_size(vec)
    end function


    subroutine vec_erase(vec,i)
        class(vector):: vec
        integer(int64),intent(in):: i
        integer(int64):: dmp

        dmp = vec%pop(i)
    end subroutine


    subroutine vec_reverse(vec)
        class(vector):: vec
        integer(int64):: i, j, n, t1, t2

        n = vec%size()
        do i=1,n/2
            j = n+1-j
            t1 = vec%array(i)
            t2 = vec%array(j)
            vec%array(j) = t1
            vec%array(i) = t2
        end do
    end subroutine


    function vec_to_array(vec) result(ret)
        class(vector),intent(inout):: vec
        integer(int64):: ret(1:vec%l)

        call check_array_allocation(vec)
        ret = vec%array(1:vec%l)
    end function
end module


module vector_mod
    use vector_int32_mod, vector_int32 => vector
    use vector_int64_mod, vector_int64 => vector
end module