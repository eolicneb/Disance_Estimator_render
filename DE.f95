module DistanceEstimator
  use DEparam

  private
  public DE

  contains
  real*8 function DE(p, pixel)
    ! This function is the only responsible for the 3D modeling
    implicit none
    real*8, intent(in)  :: p(3)
    real, optional      :: pixel(3)
    real                :: f
    real*8              :: q(3), r, dr, a1(3), a2(3), a3(3), a4(3), c(3)
    real*8              :: dist, d, factor
    integer             :: n, iter

    !forma=3

    select case (forma)
    case (1)
      DE=(p(3)+0.8) ! plane in z=-0.8
      if (present(pixel)) pixel=(/.9,1.0,1.0/)
      d=(p(2)+3.8)
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/.0,.2,.0/)
      end if
      d=(p(1)+3.8)
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/.0,.0,.2/)
      end if
      ! Spheres
      q=p
      if (abs(q(1))>1.) q(1) = q(1)-2.*q(1)/abs(q(1))
      if (abs(q(2))>1.) q(2) = q(2)-2.*q(2)/abs(q(2))
      d=sqrt(sum(q**2))-0.8
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/1.,1.,1./)
      end if
      ! Floating little sphere
      q=p-(/2.,2.,2./)
      d=sqrt(sum(q**2))-0.4
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/1.,1.,0./)
      end if
    case (2) ! SIERPINSKI'S PYRAMID
      q=p
      factor=2.3
      iter=10
      a1=(/1,1,1/)
      a2=(/-1,-1,1/)
      a3=(/1,-1,-1/)
      a4=(/-1,1,-1/)
      do n=1,iter
        c=a1;dist=sqrt(sum((q - a1)**2))
        d=sqrt(sum((q-a2)**2)); if(d<dist)then;c=a2;dist=d;end if
        d=sqrt(sum((q-a3)**2)); if(d<dist)then;c=a3;dist=d;end if
        d=sqrt(sum((q-a4)**2)); if(d<dist)c=a4
        q=factor*q-c*(factor-1.)
      end do
      DE=0.7*sqrt(sum(q**2))*factor**(-iter)
      DE=min(p(3)+1.2,DE)
      if (present(pixel)) pixel=(/1,1,1/)
    case (3)
      DE=(p(3)+0.8) ! plano en z=-0.8
      if (present(pixel)) then
        pixel=(/.9,1.0,.0/)
        if (mod(abs(p(1)),1.)>100*minD) pixel(1)=0
        if (mod(abs(p(2)),1.)>100*minD) pixel(2)=0
      end if
      d=(p(2)+1.8)
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/.0,.2,.0/)
      end if
      d=(p(1)+2.2)
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/.0,.0,.2/)
      end if
      ! Spheres
      q=p-(/-.1, -.1, 0./)
      d=sqrt(sum(q**2))-1.8
      if (d<DE) then
        DE=d
        if (present(pixel)) then
          f=1.
          if (q(1)**2+q(3)**2<1.) f=0.
          pixel=(/1.,f,f/)
        end if
      end if
      ! Floating little sphere
      q=p -(/-0.2,-0.1,2./)
      d=sqrt(sum(q**2))-0.4
      if (d<DE) then
        DE=d
        if (present(pixel)) pixel=(/1.,1.,0./)
      end if
    end select
  end function DE

end module DistanceEstimator
