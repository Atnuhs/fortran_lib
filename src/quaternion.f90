module quaternion_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    real(real64),parameter:: dzero=0d+0, done=1d+1, dhalf=5d-1
    integer(int32),parameter:: izero=0, ione=1, n=4
    type quaternion_type
        real(real64):: val(4)
    contains
        procedure:: incremental_matrix
        procedure:: coordinate_inv_rotation_matrix
        procedure:: coordinate_rotation_matrix
        procedure:: absolute_value
        procedure:: constraint
    end type

    public:: quaternion_type
    public:: from_zxz_euler_angle
    public:: mul
contains
    function from_zxz_euler_angle(phi, theta, psi) result(quaternion)
        ! z, x', z" -> phi, theta, psi
        real(real64),intent(in):: phi, theta, psi
        real(real64):: sinterm, costerm, plusterm, diffterm
        type(quaternion_type):: quaternion

        sinterm = sin(theta*dhalf)
        costerm = cos(theta*dhalf)
        plusterm = (psi+phi)*dhalf
        diffterm = (psi-phi)*dhalf
        quaternion%val(:) = [sinterm*sin(diffterm), sinterm*cos(diffterm), costerm*sin(plusterm), costerm*cos(plusterm)]
        call quaternion%constraint()
    end function

    function mul(q,p) result(ret)
        type(quaternion_type),intent(in):: q,p
        type(quaternion_type):: ret
        real(real64):: v1,v2,v3,v4

        associate(q1=>q%val(1), q2=>q%val(2),q3=>q%val(3),q4=>q%val(4),&
        p1=>p%val(1), p2=>p%val(2),p3=>p%val(3),p4=>p%val(4))
            v1 = q4*p1 - q3*p2 + q2*p3 + q1*p4
            v2 = q3*p1 + q4*p2 - q1*p3 + q2*p4
            v3 = - q2*p1 - q1*p2 + q4*p3 + q3*p4
            v4 = - q1*p1 - q2*p2 - q3*p3 + q4*p4
        end associate
        ret = quaternion_type([v1,v2,v3,v4])
        call ret%constraint()
    end function

    function inv(q) result(ret)
        type(quaternion_type),intent(in):: q
        type(quaternion_type):: ret

        ret = q
        ret%val(2) = -ret%val(2)
        ret%val(3) = -ret%val(3)
        ret%val(4) = -ret%val(4)
    end function

    function incremental_matrix(q) result(ret)
        class(quaternion_type),intent(in):: q
        real(real64):: ret(4,4)

        associate(xi=>q%val(1), eta=>q%val(2), zeta=>q%val(3), chi=>q%val(4))
            ret(:,1) = [-zeta, chi,  xi,  -eta]
            ret(:,2) = [-chi, -zeta,  eta,  xi]
            ret(:,3) = [ eta, -xi, chi,  -zeta]
            ret(:,4) = [ xi,  eta,  zeta,  chi]
        end associate
    end function

    function coordinate_rotation_matrix(q) result(ret)
        class(quaternion_type),intent(in):: q
        real(real64):: ret(3,3),t11, t12, t13, t14, t22, t23, t24, t33, t34, t44

        associate(xi=>q%val(1), eta=>q%val(2), zeta=>q%val(3), chi=>q%val(4))
            t11 = xi*xi;     t12 = xi*eta;   t13 = xi*zeta; t14 = xi*chi
            t22 = eta*eta;   t23 = eta*zeta; t24 = eta*chi
            t33 = zeta*zeta; t34 = zeta*chi
            t44 = chi*chi
        end associate
        ret(:,1) = [-t11+t22-t33+t44, -2d0*(t12+t34), 2d0*(t23-t14)]
        ret(:,2) = [ 2d0*(t34-t12), t11-t22-t33+t44, -2d0*(t13+t24)]
        ret(:,3) = [ 2d0*(t23+t14), 2d0*(t24-t13), -t11-t22+t33+t44]
    end function

    function coordinate_inv_rotation_matrix(q) result(ret)
        class(quaternion_type),intent(in):: q
        real(real64):: ret(3,3),t11, t12, t13, t14, t22, t23, t24, t33, t34, t44

        associate(xi=>q%val(1), eta=>q%val(2), zeta=>q%val(3), chi=>q%val(4))
            t11 = xi*xi;     t12 = xi*eta;   t13 = xi*zeta; t14 = xi*chi
            t22 = eta*eta;   t23 = eta*zeta; t24 = eta*chi
            t33 = zeta*zeta; t34 = zeta*chi
            t44 = chi*chi
        end associate
        ret(:,1) = [-t11+t22-t33+t44, 2d0*(t34-t12), 2d0*(t23+t14)]
        ret(:,2) = [-2d0*(t34+t12), t11-t22-t33+t44, 2d0*(t24-t13)]
        ret(:,3) = [2d0*(t23-t14), -2d0*(t13+t24), -t11-t22+t33+t44]
    end function

    function absolute_value(q) result(ret)
        class(quaternion_type),intent(in):: q
        real(real64):: ret
        integer(int32):: i
        
        ret = dzero
        do i=ione,n
            ret=ret+q%val(i)*q%val(i)
        end do
    end function

    subroutine constraint(q)
        class(quaternion_type),intent(inout):: q

        q%val(:) = q%val(:)/sqrt(q%absolute_value())
    end subroutine
end module


program main
    use,intrinsic :: iso_fortran_env
    use quaternion_mod
    implicit none
    real(real64),parameter:: pi=acos(-1d0)
    real(real64):: euler(3) = [pi/3d0, pi/4d0, pi/2d0]
    real(real64):: p(3) = [1d0, 2d0, 3d0]
    type(quaternion_type):: q,q1,q2,q3

    q = from_zxz_euler_angle(euler(1), euler(2), euler(3))
    q1 = from_zxz_euler_angle(euler(1), 0d0, 0d0)
    q2 = from_zxz_euler_angle(0d0, euler(2), 0d0)
    q3 = from_zxz_euler_angle(0d0, 0d0, euler(3))
    print*, 'qaternion: ', q1%val
    print*, 'qaternion: ', q2%val
    print*, 'qaternion: ', q3%val
    print*, 'qaternion: ', q%val
    print*, 'qaternion: ', mul(mul(q1,q2),q3)
    print*, 'rotation: ', matmul(q%coordinate_rotation_matrix(), p)
    print*, 'inv_rotation: ', matmul(q%coordinate_inv_rotation_matrix(), p)
end program main