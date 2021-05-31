module djkstra_mod
    use,intrinsic :: iso_fortran_env
    use heap_map_int_int_mod
    use vector_mod
    implicit none
contains
    function djkstra(graph, cost, start) result(dist)
        ! start頂点からの各頂への最短距離をdjkstraで計算、distに格納し、返却
        integer(int64):: start
        integer(int64), allocatable:: dist(:)
        integer(int64),parameter:: inf = 10_int64**18
        type(vector):: graph(:), cost(:)
        type(heap_map_int_int):: que

        allocate(dist(size(graph)), source=inf)
        dist(start) = 0
        call que%push(0_int64, start)

        do while(que%remain())
            block
                integer(int64):: d, from, i
                call que%pop(d, from)
                if (dist(from) < d) cycle
                
                do i=1,graph(from)%size()
                    block
                        integer(int64):: to, c
                        
                        to = graph(from)%at(i)
                        c = cost(from)%at(i)
                        if (dist(from) + c < dist(to)) then
                            dist(to) = dist(from) + c
                            call que%push(dist(to), to)
                        end if
                    end block
                end do
                
            end block
        end do

    end function
end module