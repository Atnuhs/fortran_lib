module eratosthenes_sieve_mod
    use,intrinsic :: iso_fortran_env
    use vector_mod
    implicit none
    public
    integer(int32),private,parameter:: prec=int32
    type:: factor_pair
        integer(prec):: val
        integer(prec):: cnt
    end type
    integer(prec), allocatable:: sieve(:)
    type(vector):: primes

contains
    subroutine make_sieve(n)
        ! 初期化 O(n)
        ! 1. エラトステネスの篩の表を作る。
        ! 2. 素数集合Vectorを作る。
        integer(prec),intent(in):: n
        integer(prec):: i,j
        
        allocate(sieve(n),source=0_prec)
        do i=2,n
            if (sieve(i)/=0) cycle
            call primes%push_back(i)
            sieve(i) = i
            do j= i*i, n, i
                if (sieve(j)==0) sieve(j) = i
            end do
        end do
    end subroutine


    function prime_factor_vec(x) result(ret)
        ! xの素因数をvectorで返す。ex) x=24 -> vector[2,2,2,3]
        !　NEED => vector構造, sieve(初期化)
        integer(prec),value:: x
        type(vector):: ret

        ! if(x==1) call ret%push_back(1_prec)
        do while(x/=1)
            call ret%push_back(sieve(x))
            x=x / sieve(x)
        end do
    end function


    function factor(x) result(ret)
        ! xの素因数の集合とそのカウントをfactor_pairの配列で返す。
        ! ex) x=720 (= 2^4 * 3^2 *5^1)
        !     i | 1 2 3 (ただの添え字)
        !   val | 2 3 5 (素因数の重複なし集合)
        !   cnt | 4 2 1 (素因数の計数)
        !
        ! NEED -> vector構造, sieve(初期化), factor_pair構造
        ! i番目の素因数の値　　　 -> ret(i)%val
        ! i番目の素因数のカウント -> ret(i)%num
        ! 素因数の重複なし集合 -> ret(:)%val

        integer(prec),intent(in):: x
        integer(prec):: i,j
        type(vector):: fv
        type(factor_pair),allocatable:: tmp(:), ret(:)
        fv = prime_factor_vec(x)

        if (fv%size() == 0) then
            allocate(ret(0))
            return
        end if

        allocate(tmp(fv%size()), source=factor_pair(0,0))
        tmp(1)%val = fv%at(1_prec); tmp(1)%cnt = 1
        j=1
        do i=2,fv%size()
            if (fv%at(i) == fv%at(i-1)) then
                tmp(j)%cnt = tmp(j)%cnt + 1
            else
                j=j+1
                tmp(j)%val = fv%at(i)
                tmp(j)%cnt = 1
            end if
        end do
        ret = tmp(1:j)
    end function


    function is_prime(x) result(ret)
        ! xが素数なら.true. そうでないなら.false.を返す
        ! 1は素数ではないので, 1 => .false.となる。
        ! NEED　-> sieve(初期化)
        integer(prec),intent(in):: x
        logical:: ret

        ret = sieve(x) == x
    end function
end module