module lazy_segtree_operators
    use,intrinsic :: iso_fortran_env
    use number_theory_mod
    implicit none
    integer(int64),parameter:: md = 998244353
    type s_elem
        ! 要素の構造体(モノイド)

    contains
        procedure,private,pass:: s_equals
        generic:: operator(==) => s_equals
    end type
    type f_elem
        ! 作用の構造体(モノイド)

    contains
        procedure,private,pass:: f_equals
        generic:: operator(==) => f_equals
    end type
contains
    function s_equals(s1,s2) result(isEqual)
        ! 要素のイコールの定義
        class(s_elem),intent(in):: s1,s2
        logical:: isEqual

    end function


    function f_equals(f1,f2) result(isEqual)
        ! 作用のイコールの定義
        class(f_elem),intent(in):: f1,f2
        logical:: isEqual

    end function


    function e()
        ! 要素の単位元を返すだけ
        type(s_elem):: e

        e = s_elem()
    end function


    function id()
        ! 作用の単位元を返すだけ
        type(f_elem):: id

        id = f_elem()
    end function


    function op(a,b)
        ! 要素のマージ(min >min max->max, sum->a+b)
        ! bが単位元で正しく計算できない場合は場合分けをする
        type(s_elem),intent(in):: a,b
        type(s_elem):: op
        

    end function


    function mapping(s,f) result(new_s)
        ! 作用を要素に適応
        type(s_elem),intent(in):: s
        type(f_elem),intent(in):: f
        type(s_elem):: new_s

        if (f == id()) then
            new_s = s
        else

        end if
    end function


    function composition(old_f,f) result(new_f)
        ! 作用のアップデート
        type(f_elem),intent(in):: old_f,f
        type(f_elem):: new_f

        if (f == id()) then
            new_f = f
        else

        end if
    end function
end module

module lazy_segtree_mod
    use,intrinsic :: iso_fortran_env
    use lazy_segtree_operators
    implicit none
    private

    type,public:: lazy_segtree
        type(s_elem),allocatable:: d(:)
        type(f_elem),allocatable:: lz(:)
        integer(int32):: len
    contains
        procedure:: leaf => lst_leaf
        procedure:: update => lst_update, lst_update_one
        procedure:: query => lst_query, lst_query_one
        procedure:: to_array => lst_to_array
        procedure:: set => lst_set
    end type
    interface lazy_segtree
        module procedure:: lst_init
    end interface
contains
    function lst_init(n) result(lst)
        ! 要素と作用を持つ配列を割り当て
        ! 長さを与える
        type(lazy_segtree):: lst
        integer(int32),intent(in):: n
        integer(int32):: x

        lst%len = n

        x=1
        do while(n > x)
            x = 2*x
        end do
        allocate(lst%d(2*x-1), source=e())
        allocate(lst%lz(2*x-1),source=id())
    end function


    function lst_leaf(lst)
        ! 葉の数
        class(lazy_segtree):: lst
        integer(int32):: lst_leaf

        lst_leaf = size(lst%d)/2+1
    end function


    subroutine lst_set(lst,i,s)
        ! i番目の葉に要素sを与える
        ! セグ木の上までマージ
        class(lazy_segtree),intent(inout):: lst
        type(s_elem),intent(in):: s
        integer(int32),value:: i

        i=i+lst%leaf()-1
        lst%d(i) = s
        i=rshift(i,1)
        do while(i > 0)
            lst%d(i) = op(lst%d(i*2), lst%d(i*2+1))
            i=rshift(i,1)
        end do
    end subroutine


    subroutine lst_update_one(lst,i,f)
        ! 作用による一点更新の場合のラッパー
        class(lazy_segtree),intent(inout):: lst
        type(f_elem),intent(in):: f
        integer(int32),intent(in):: i

        call lst_update_sub(lst,i,i,f,1,lst%leaf(),1)
    end subroutine


    subroutine lst_update(lst, l,r,f)
        ! 作用による区間更新の場合のラッパー
        class(lazy_segtree),intent(inout):: lst
        type(f_elem),intent(in):: f
        integer(int32),intent(in):: l,r
        
        call lst_update_sub(lst,l,r,f,1,lst%leaf(),1)
    end subroutine


    recursive subroutine lst_update_sub(lst,ql,qr,f,nl,nr,i)
        ! 作用による区間更新の処理ルーチン
        ! 今ある作用を要素に作用
        ! 新たな作用をマージ
        ! 作用があった要素のマージ
        class(lazy_segtree),intent(inout):: lst
        integer(int32),intent(in):: ql,qr,nl,nr,i
        type(f_elem),intent(in):: f
        integer(int32):: nm
        
        call eval(lst,i)
        if (ql <= nl .and. nr <= qr) then
            lst%lz(i) = composition(lst%lz(i), f)
            call eval(lst,i)
        else if (ql <= nr .and. nl <= qr) then
            nm = (nl+nr)/2
            call lst_update_sub(lst,ql,qr,f,nl,  nm,i*2  )
            call lst_update_sub(lst,ql,qr,f,nm+1,nr,i*2+1)
            lst%d(i) = op(lst%d(i*2), lst%d(i*2+1))
        end if
    end subroutine


    subroutine eval(lst,i)
        ! 遅延の解消(子に伝播させ、要素に作用させ、単位元にする)
        class(lazy_segtree),intent(inout):: lst
        integer(int32):: i

        if (f_equals(lst%lz(i),id())) return
        if (i < lst%leaf()) then
            ! 子がいる場合は子に伝播させる
            lst%lz(i*2) = composition(lst%lz(i*2), lst%lz(i))
            lst%lz(i*2+1) = composition(lst%lz(i*2+1), lst%lz(i))
        end if
        lst%d(i) = mapping(lst%d(i),lst%lz(i))
        lst%lz(i) = id()
    end subroutine


    subroutine correct_laziness(lst)
        ! 全要素で遅延を解消させる。(to_arrayで正しい値を返すため)
        class(lazy_segtree):: lst
        integer(int32):: i
        
        do i=1,size(lst%d)
            call eval(lst,i)
        end do
    end subroutine


    function lst_query_one(lst,i) result(ret)
        ! クエリ一点取得のラッパー
        class(lazy_segtree),intent(inout):: lst
        integer(int32),intent(in):: i
        type(s_elem):: ret

        ret = lst_query_sub(lst,i,i,1,lst%leaf(),1)
    end function


    function lst_query(lst,l,r) result(ret)
        ! クエリ取得のラッパー
        class(lazy_segtree),intent(inout):: lst
        integer(int32), intent(in):: l,r
        type(s_elem):: ret

        ret = lst_query_sub(lst,l,r,1,lst%leaf(),1)
    end function


    recursive function lst_query_sub(lst,ql,qr,nl,nr,i) result(ret)
        ! クエリ取得の処理部
        ! 作用を要素に作用
        ! 要素を子からマージしていく
        class(lazy_segtree),intent(inout):: lst
        integer(int32),intent(in):: ql,qr,nl,nr,i
        integer(int32):: nm
        type(s_elem):: ret,r1,r2
        
        call eval(lst,i)
        if (nr < ql .or. qr < nl) then
            ret = e()
        else if (ql <= nl .and. nr <= qr) then
            ret = lst%d(i)
        else
            nm = (nl+nr)/2
            r1 = lst_query_sub(lst,ql,qr,nl,  nm,i*2  )
            r2 = lst_query_sub(lst,ql,qr,nm+1,nr,i*2+1)
            ret = op(r1,r2)
        end if
    end function


    function lst_to_array(lst) result(ret)
        ! 全要素の遅延解消の後、元の配列を返す
        class(lazy_segtree):: lst
        type(s_elem):: ret(lst%len)

        call correct_laziness(lst)
        ret(:) = lst%d(lst%leaf():lst%leaf()+lst%len-1)
    end function
end module