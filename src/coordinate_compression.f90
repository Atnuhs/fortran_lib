module array_lib_mod
    use,intrinsic :: iso_fortran_env
    use merge_sort_mod
    implicit none
    
contains
    function coordinate_compression(a) result(rank_a)

        ! 座標圧縮:: 配列aの要素a(i)はaの中で何番目に大きいかをia(i)に格納して返す
        ! 愚直だとO(max(a)-min(a))となるが、この関数ではO(length(a))となる
        integer(int32),intent(in):: a(:)
        integer(int32), allocatable:: b(:)
        integer(int32):: len_a, ia, rnk
        integer(int32):: rank_a(size(a)), tmp(size(a))
 
        ! a(i) は何番目か？ => [a(i)はbの何番目か?] <- 二分探索
        ! aを重複除去ソートした配列bに対して二分探索
 
        ! 1.1 aを非破壊ソート(tmp)
        len_a = size(a)
        tmp = a
        call merge_sort(tmp)
 
        ! 1.2 aをソートした配列(tmp)の重複要素を取り除いた配列(b)を生成
        b = uniq(tmp)
        ! 1.3 aのi番目の値が何番目に小さいかが入っている配列rank_aを作成
         do ia=1,len_a
            rnk = lower_bound(b, a(ia))
 
            rank_a(ia) = rnk
         end do
    end function
 
 
    function uniq(a) result(b)
        ! sort済みの配列aの重複要素を除いた配列bを返す
        integer(int32),intent(in):: a(:)
        integer(int32),allocatable:: b(:)
        integer(int32):: len_a, len_b, ia, ib
 
        ! 1. bの要素数をカウント
        len_a = size(a)
        len_b = len_a
        do ia=2,len_a
            ! 重複要素を見つけるたびにlen_bから1引く
            if (a(ia) == a(ia-1)) len_b = len_b - 1
        end do
 
        allocate(b(len_b))
        ! 2. 重複を除去した配列(b)の生成
        b(1) = a(1)
        ib = 2
        do ia=2,len_a
            if (a(ia) == a(ia-1)) cycle
            ! a(ia)は重複していない要素である。
            b(ib) = a(ia)
            ib=ib+1
        end do
    end function
 
 
    function lower_bound(a, x) result(ret)
        ! ソート済みの配列aの中でa(i-1) < x <= a(i)となるようなiを返す
        ! ex) a = [4 7 7]
        ! ex1) x = 1 => ret = 1 | [T T T]
        ! ex2) x = 4 => ret = 1 | [T T T]
        ! ex3) x = 5 => ret = 2 | [F T T]
        ! ex4) x = 7 => ret = 2 | [F T T]
        ! ex5) x = 8 => ret = 4 | [F F F]
        integer(int32),intent(in):: a(:), x
        integer(int32):: len_a, ret, ng, ok, m
 
        len_a = size(a)
        ng = 0
        ok = len_a+1
 
        do while(ok-ng>1)
            m = (ok+ng)/2
            if (a(m) >= x) then
                ok = m
            else
                ng = m
            end if
        end do
        ret = ok
    end function
end module array_lib_mod