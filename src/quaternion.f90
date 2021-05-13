module quaternion_mod
    use,intrinsic :: iso_fortran_env
    implicit none
    type quaternion_type
        real(real64):: val(4)
    contains
        procedure:: incremental_matrix
        procedure:: coordinate_inv_rotation_matrix
        procedure:: coordinate_rotation_matrix
        procedure:: norm
        procedure:: constraint
    end type
contains
    function from_zxz_euler_angle(phi, theta, psi) result(quaternion)
        ! z, x', z" -> phi, theta, psi
        real(real64),intent(in):: phi, theta, psi
        real(real64):: sinterm, costerm, plusterm, diffterm
        type(quaternion_type):: quaternion

        sinterm = sin(theta*0.5d0)
        costerm = cos(theta*0.5d0)
        plusterm = (psi+phi)*0.5d0
        diffterm = (psi-phi)*0.5d0
        quaternion%val(:) = [sinterm*sin(diffterm), sinterm*cos(diffterm), costerm*sin(plusterm), costerm*cos(plusterm)]
    end function

    function incremental_matrix(q) result(ret)
        class(quaternion_type):: q
        real(real64):: ret(4,4)

        associate(xi=>q%val(1), eta=>q%val(2), zeta=>q%val(3), chi=>q%val(4))
            ret(:,1) = [-zeta, chi,  xi,  -eta]
            ret(:,2) = [-chi, -zeta,  eta,  xi]
            ret(:,3) = [ eta, -xi, chi,  -zeta]
            ret(:,4) = [ xi,  eta,  zeta,  chi]
        end associate
    end function

    function coordinate_rotation_matrix(q) result(ret)
        class(quaternion_type):: q
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
        class(quaternion_type):: q
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

    function norm(q) result(ret)
        class(quaternion_type):: q
        real(real64):: ret
        
        ret = dot_product(q%val(:),q%val(:))
    end function

    subroutine constraint(q)
        class(quaternion_type):: q

        q%val(:) = q%val(:)/sqrt(q%norm())
    end subroutine
end module


program main
    use,intrinsic :: iso_fortran_env
    use quaternion_mod
    implicit none
    real(real64),parameter:: pi=acos(-1d0)
    real(real64):: euler(3) = [0d0, 0d0, pi/2d0]
    real(real64):: p(3) = [1d0, 2d0, 3d0]
    type(quaternion_type):: q

    q = from_zxz_euler_angle(euler(1), euler(2), euler(3))
    print*, 'qaternion: ', q%val
    print*, 'rotation: ', matmul(q%coordinate_rotation_matrix(), p)
    print*, 'inv_rotation: ', matmul(q%coordinate_inv_rotation_matrix(), p)
end program main