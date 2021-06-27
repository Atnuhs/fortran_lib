program linked_list
    implicit none
    type node
      type (node), pointer :: next_node
      integer value
    end type node
    integer num
    type (node), pointer :: first_node, new_node, p
  
    nullify (first_node)
  
    do
      print *, 'Input a positive number: (Enter negative number to end)'
      read *, num
      if (num<0) exit
  
      ! リストの最初に新しい値を追加
      allocate (new_node)
      new_node%value = num
      first_node = new_node
      
  
    end do
  
    p => first_node
    print *, "================================"
    do while (associated(p))
      print *, p%value
      p => p%next_node
    end do
  
  end program linked_list