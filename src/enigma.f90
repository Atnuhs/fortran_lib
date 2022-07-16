! use random_mod

! rnd()          => real64,         0 <= v < 1
! randint(n)     => int32 or int64, 0 <= v <= n
! randrange(l,r) => int32 or int64, l <= v <= r

module random01_mod
    use,intrinsic :: iso_fortran_env
    implicit none
contains
    function random01() result(v)
        real(real64):: v

        call random_number(v)
    end function
end module


module random_int32
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        integer(int32),intent(in):: l,r
        integer(int32):: v

        v = int(rnd()*(r-l+1)) + l
    end function

    function randint(n) result(v)
        integer(int32),intent(in):: n
        integer(int32):: v

        v = randrange(0_int32, n)
    end function
end module


module random_int64
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        integer(int64),intent(in):: l,r
        integer(int64):: v

        v = int(rnd()*(r-l+1)) + l
    end function

    function randint(n) result(v)
        integer(int64),intent(in):: n
        integer(int64):: v

        v = randrange(0_int64, n)
    end function
end module


module random_real64
    use,intrinsic :: iso_fortran_env
    use random01_mod,only: rnd => random01
    implicit none
    private
    public:: randrange, randint
contains
    function randrange(l,r) result(v)
        real(real64),intent(in):: l,r
        real(real64):: v

        v = rnd()*(r-l) + l
    end function

    function randint(n) result(v)
        real(real64),intent(in):: n
        real(real64):: v

        v = rnd()*n
    end function
end module random_real64



module random_mod
    use,intrinsic :: iso_fortran_env
    use random01_mod, only: random01
    use random_int32, rri32=>randrange, rii32=>randint
    use random_int64, rri64=>randrange, rii64=>randint
    use random_real64, rrr64=>randrange, rir64=>randint
    
    interface randrange
        module procedure rri32, rri64, rrr64
    end interface

    interface randint
        module procedure rii32, rii64, rir64
    end interface

    interface random01
        module procedure random01
    end interface

    public:: random01, randrange, randint
end module

module str_conv
    use,intrinsic:: iso_fortran_env
    implicit none
contains
    function int_to_char(i) result(ret)
        integer(int32):: i
        character(1):: ret

        ret = char(i + ichar('a') - 1)
    end function
    
    function char_to_int(c) result(ret)
        character(1):: c
        integer(int32):: ret
        
        ret = ichar(c) - ichar('a') + 1
    end function
end module


module enigma_rotor_mod
    use,intrinsic:: iso_fortran_env
    use random_mod
    use str_conv
    implicit none
    private
    type,public:: rotor
        integer(int32):: wiring_to(26), wiring_rev_to(26)
        integer(int32):: angle, initial_angle
    contains
        procedure:: describe
        procedure:: rotate
        procedure:: set_angle
        procedure:: pass_forward_order
        procedure:: pass_reverse_order
    end type
    

    interface rotor
        module procedure:: gen_wiring
    end interface
contains
    function gen_wiring() result(rot)
        type(rotor):: rot
        integer(int32):: i,j,tmp
        
        print'(a)', "!! GENERATE ROTOR"
        rot%wiring_to(:) = [(i, i=1,26)]
        do i=1,26
                j = randrange(1,26)
                ! swap the i-th and j-th of rot%wiring_to
                tmp = rot%wiring_to(i)
                rot%wiring_to(i) = rot%wiring_to(j)
                rot%wiring_to(j) = tmp
        end do
        
        do i=1,26
            rot%wiring_rev_to(rot%wiring_to(i)) = i
        end do
        
        print"(a)", "!! SUCCESS GENERATE ROTER"
        print"(a)", "!!"
    end function

    subroutine set_angle(rot, angle)
        class(rotor),intent(inout):: rot
        integer(int32):: angle
    
        rot%angle = angle
        rot%initial_angle = angle
    end subroutine
    

    function rotate(rot, angle) result(one_turned)
        class(rotor),intent(inout):: rot
        integer(int32):: angle
        integer(int32):: before_num_of_rotation
        integer(int32):: after_num_of_rotation
        logical:: one_turned
    
        before_num_of_rotation = number_of_rotation(rot%angle - rot%initial_angle)
        rot%angle= rot%angle + angle
        after_num_of_rotation = number_of_rotation(rot%angle - rot%initial_angle)
        one_turned = after_num_of_rotation > before_num_of_rotation
    end function


    function pass_forward_order(rot, i) result(ret)
        class(rotor),intent(inout):: rot
        integer(int32),intent(in):: i
        integer(int32):: idx, ret

        idx = modulo(i-rot%angle-1, 26) + 1
        ret = modulo(rot%wiring_to(idx) + rot%angle - 1, 26) + 1
    end function


    function pass_reverse_order(rot, i) result(ret)
        class(rotor),intent(inout):: rot
        integer(int32),intent(in):: i
        integer(int32):: idx, ret

        idx = modulo(i-rot%angle-1, 26) + 1
        ret = modulo(rot%wiring_rev_to(idx) + rot%angle -1, 26) + 1
    end function


    function number_of_rotation(angle) result(ret)
        integer(int32),intent(in):: angle
        integer(int32):: ret

        ret = angle / 26
    end function
    

    subroutine describe(rot)
        class(rotor),intent(in):: rot
        integer(int32):: i
        
        print'(a)', "$ DESCRIBE ROTOR WIRING"
        print'("ANGLE::", i3.2, ", INITIAL_ANGLE::", i3.2)', rot%angle, rot%initial_angle 
        print"('$ INDEX          :: $', 26(i3.2), ' $' )", [(i,i=1,26)]
        print"('$ FORWARD ORDER:: $', 26(i3.2), ' $' )", rot%wiring_to(:)
        print"('$ REVERSE ORDER:: $', 26(i3.2), ' $' )", rot%wiring_rev_to(:)
        print'(a)', "$"
    end subroutine
end module



module plug_board_mod
    use,intrinsic:: iso_fortran_env
    implicit none
    private
    type, public:: plug_board
        integer(int32):: wiring_to(26)
        integer(int32):: wiring_rev_to(26)
    contains
        procedure:: change_wiring_with_daily_key
        procedure:: describe
        procedure:: pass_forward_order
        procedure:: pass_reverse_order
    end type
    

    interface plug_board
        module procedure:: gen_wiring
    end interface
contains 
    function gen_wiring() result(ret)
        type(plug_board):: ret
        integer(int32):: i
        
        ret%wiring_to = [(i, i=1,26)]
        ret%wiring_rev_to = [(i, i=1,26)]
    end function
    
    subroutine change_wiring_with_daily_key(pb, i,j)
        class(plug_board):: pb
        integer(int32):: i, j, tmp
        
        tmp = pb%wiring_rev_to(i)
        pb%wiring_rev_to(i) = pb%wiring_rev_to(j)
        pb%wiring_rev_to(j) = tmp
        
        pb%wiring_to(pb%wiring_rev_to(i)) = i
        pb%wiring_to(pb%wiring_rev_to(j)) = j
    end subroutine


    function pass_forward_order(pb, i) result(ret)
        class(plug_board):: pb
        integer(int32),intent(in):: i
        integer(int32):: ret

        ret = pb%wiring_to(i)
    end function
    
    
    function pass_reverse_order(pb, i) result(ret)
        class(plug_board):: pb
        integer(int32),intent(in):: i
        integer(int32):: ret

        ret = pb%wiring_rev_to(i)
    end function


    subroutine describe(pb)
        class(plug_board):: pb
        
        print'(a)', "$ DESCRIBE PLUG BOARD WIRING"
        print"('$ FORWARD ORDER:: $', 26(i3.2), ' $' )", pb%wiring_to(:)
        print"('$ REVERSE ORDER:: $', 26(i3.2), ' $' )", pb%wiring_rev_to(:)
        print'(a)', "$"
    end subroutine
end module


module enigma_mod
    use,intrinsic:: iso_fortran_env
    use random_mod
    use enigma_rotor_mod
    use plug_board_mod
    use str_conv
    implicit none
    integer(int32),parameter:: num_rotors = 3
    integer(int32),parameter:: num_encrypting_phase = 4 + num_rotors*2
    type enigma_machine
        type(rotor):: rotors(num_rotors)
        type(plug_board):: pb
        integer(int32):: order_of_rotors(num_rotors)
    contains
        procedure:: set_daily_key
        procedure:: type_key
        procedure:: encrypt
        procedure:: decrypt
    end type

    interface enigma_machine
        module procedure:: init_enigma_machine
    end interface
contains
    function init_enigma_machine() result(ret)
        type(enigma_machine):: ret

        ! rotorsの結線指定

        print'(a)', "!!!! ENIGMA INITIALIZE"
        ret%rotors = make_rotors_wiring()
        print'(a)', "!!!! SUCCESS ENIGMA INITIALIZE"
        print*, ""
    end function


    function make_rotors_wiring() result(ret)
        type(rotor):: ret(num_rotors)
        integer(int32):: i
        
        print'(a)', "!!! SET ENIGMA ROTORS"
        do i=1,num_rotors
            ret(i) = rotor()
            call ret(i)%describe()
        end do
        print'(a)', "!!! SUCCESS SET ENIGMA ROTORS"
        print'(a)', "!!!"
    end function
    

    subroutine set_daily_key(em, plug_board_swap_list, order_of_rotors, initial_angle_of_rotors)
        class(enigma_machine):: em
        character(1),intent(in):: plug_board_swap_list(:,:)
        integer(int32),intent(in):: order_of_rotors(num_rotors)
        integer(int32),intent(in):: initial_angle_of_rotors(num_rotors)
        integer(int32):: i

        
        print'(a)', "!!!! SET DAILY KEY"

        ! set plug_board
        print'(a)', "!!! SET PLUG BOARD"
        em%pb = plug_board()
        do i=1,size(plug_board_swap_list,2)
            print'("!! SET WIRING OF PLUG BOARD SETTING NO. ", i0)', i
            call em%pb%change_wiring_with_daily_key(char_to_int(plug_board_swap_list(1,i)), char_to_int(plug_board_swap_list(2,i)))
        end do
        call em%pb%describe()
        print'(a)', "!!! SUCCESS SET PLUG BOARD"
        print'(a)', "!!!"
        
        ! set order_of_rotors
        print'(a)', "!!! SET ORDER OF ROTORS"
        em%order_of_rotors(:) = order_of_rotors(:)
        print'(a)', "!!! SUCCESS SET ORDER OF ROTORS"
        print'(a)', "!!!"

        ! set initial_angle_of_rotors
        print'(a)', "!!! SET INITIAL ANGLE OF ROTORS"
        do i=1,num_rotors
            print'("!! SET ANGLE OF ROTOR NO. ", i0)', i
            call em%rotors(em%order_of_rotors(i))%set_angle(initial_angle_of_rotors(i))
            call em%rotors(em%order_of_rotors(i))%describe()
        end do
        print'(a)', "!!! SUCCESS SET INITIAL ANGLE OF ROTORS"
        print'(a)', "!!!"
        print'(a)', "!!!! SUCCESS SET DAILY KEY"
        print*, ""
        
    end subroutine


    function type_key(em, c) result(encrypted_c)
        class(enigma_machine):: em
        character(1),intent(in):: c
        character(1):: encrypted_c
        integer(int32):: i, encrypting_data(num_encrypting_phase)
        logical:: one_turned

        ! character to integer
        encrypting_data(1) = char_to_int(c)

        ! pass plub board forward order
        encrypting_data(2) = em%pb%pass_forward_order(encrypting_data(1))

        ! pass rotors forward order
        do i=1,num_rotors
            encrypting_data(i+2) = em%rotors(em%order_of_rotors(i))%pass_forward_order(encrypting_data(i+1))
        end do
        ! pass reflector
        encrypting_data(num_rotors+3) = pass_reflector(encrypting_data(num_rotors+2))

        ! pass rotor reverse order
        do i=num_rotors,1,-1
            encrypting_data(2*num_rotors-i+4) &
                = em%rotors(em%order_of_rotors(i))%pass_reverse_order(encrypting_data(2*num_rotors-i+3))
        end do
        
        encrypting_data(num_encrypting_phase) = em%pb%pass_reverse_order(encrypting_data(num_encrypting_phase-1))
        
        print'("!!! ENCRYPTING DATA:: ", *(i3.2))', encrypting_data
        encrypted_c = int_to_char(encrypting_data(num_encrypting_phase))
        ! rotate rotors
        one_turned = em%rotors(1)%rotate(1)
        do i=2,num_rotors
            if (.not. one_turned) exit
            one_turned = em%rotors(i)%rotate(1)
        end do
    end function


    function pass_reflector(i) result(ret)
        integer(int32),intent(in):: i
        integer(int32):: ret

        ret = 27 - i
    end function
    
    function encrypt(em, communication_key, message) result(encrypted_message)
        class(enigma_machine):: em
        character(num_rotors*2),intent(in):: communication_key
        character(*),intent(in):: message
        character(num_rotors*2):: encrypted_communication_key = ' '
        character(:),allocatable:: encrypted_message, encrypted_message_sub
        integer(int32):: i
        integer(int32):: num_message

        print'("!!!! ENCRYPTING")'
        num_message = len_trim(message)
        allocate(character(num_message+1+2*num_rotors):: encrypted_message)
        allocate(character(num_message):: encrypted_message_sub)

        ! 1. Encryption of communication keys.
        do i=1,2*num_rotors 
            encrypted_communication_key(i:i) = em%type_key(communication_key(i:i))
        end do
        print'("!!! ENCRYPTED COMMUNICATION KEY:: before:: ", a, " after:: ", a)', communication_key, encrypted_communication_key
        print'(a)', "!!!"

        ! 2. Set the angle of rotors.
        do i=1,num_rotors
            call em%rotors(i)%set_angle(char_to_int(communication_key(i:i)))
            call em%rotors(i)%describe()
        end do


        ! 3. Encrypt message
        do i=1,num_message
            encrypted_message_sub(i:i) = em%type_key(message(i:i))
        end do
        print'("!!! ENCRYPTED MESSAGE:: before:: ", a, " after:: ", a)', message, encrypted_message_sub
        print'(a)', "!!!"
        
        ! 4. Combine encrypted communication key and encrypted message.
        encrypted_message = trim(encrypted_message_sub) // " " // trim(encrypted_communication_key) 
        print'("!!! COMBINED MESSAGE:: ", a)', encrypted_message 
        print'(a)', "!!!"
        print'("!!!! SUCCESS ENCRYPTING")', 
        print'(a)', ""
        end function

    function decrypt(em,encrypted_message) result(decrypted_message)
        class(enigma_machine):: em
        character(*),intent(in):: encrypted_message
        character(:),allocatable:: decrypted_message
        character(2*num_rotors):: encrypted_communication_key, decrypted_communication_key
        integer(int32):: num_decrypted_message, num_encrypted_message
        integer(int32):: i
        

        print'("!!!! DECRYPTING")'
        ! 0. DIVIDE ENCRYPTED_MESSAGE TO COMMUNICATION KEY
        num_encrypted_message = len_trim(encrypted_message)
        num_decrypted_message = num_encrypted_message - 2*num_rotors - 1
        allocate(character(num_decrypted_message):: decrypted_message)
        print'("NUM_ENCRYPTED_MESSAGE:: ", i0)', num_encrypted_message
        print'("NUM_DECRYPTED_MESSAGE:: ", i0)', num_decrypted_message
        
        encrypted_communication_key = encrypted_message(num_decrypted_message+2:num_encrypted_message)

        ! 1. Decryption of communication keys.
        do i=1,2*num_rotors 
            decrypted_communication_key(i:i) = em%type_key(encrypted_communication_key(i:i))
        end do
        print'("!!! DECRYPTED COMMUNICATION KEY:: before:: ", a, " after:: ", a)', &
            encrypted_communication_key, decrypted_communication_key
        print'(a)', "!!!"
        ! 2. Set the angle of rotors.
        do i=1,num_rotors
            call em%rotors(i)%set_angle(char_to_int(decrypted_communication_key(i:i)))
            call em%rotors(i)%describe()
        end do


        ! 3. Encrypt message
        do i=1,num_decrypted_message
            decrypted_message(i:i) = em%type_key(encrypted_message(i:i))
        end do
        print'("!!! DECRYPTED MESSAGE:: before:: ", a, " after:: ", a)', encrypted_message, decrypted_message
        print'(a)', "!!!"
        
        print'("!!!! SUCCESS DECRYPTING")', 
        print'(a)', ""
    end function
end module

program main
    use enigma_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type(enigma_machine):: em
    integer(int32):: i, n
    integer(int32):: order_of_rotors(3) = [1,2,3]
    integer(int32):: initial_angle_of_rotors(3) = [0,0,0]
    character(1):: plug_board_wiring(2,2)
    character(:),allocatable:: message, encrypted_message, decrypted_message

    plug_board_wiring(:,1) = ['a', 'c']
    plug_board_wiring(:,2) = ['c', 'd']
    
    em = init_enigma_machine()
    call em%set_daily_key(plug_board_wiring, order_of_rotors, initial_angle_of_rotors)

    ! read*, n
    n = 10
    allocate(character(n):: message, encrypted_message)
    ! read*, message
    message = 'abcdefghij'
    encrypted_message = em%encrypt('refref', message)
    
    call em%set_daily_key(plug_board_wiring, order_of_rotors, initial_angle_of_rotors)
    decrypted_message = em%decrypt(encrypted_message)
end program main
