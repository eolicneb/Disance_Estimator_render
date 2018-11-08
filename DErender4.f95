module DErender4
  use DistanceEstimator
  use DEparam

  private
  public trace, shadow, ambocl, colour, addColour, &
         reflected, mines, maxes, reflection, combo, normalVersor

  contains

  subroutine reflected(dir, normal, towards)
    implicit none
    real*8, intent(in)    :: dir(3), normal(3)
    real*8, intent(out)   :: towards(3)
      ! Each reflected ray is computed from
      ! the incident ray's direction and the
      ! normal to the surface of reflection.
      towards = dir-(2*sum(normal*dir))*normal

  end subroutine

  subroutine combo(from, dir, luz, pixel, point, normal)
    ! The whole process from the initial point of view and
    ! ray direction all the way to the final pixel color
    ! is organized from this subroutine
    implicit none
    real*8, intent(in)     :: from(3), dir(3), luz(3)
    real*8, intent(out)    :: point(3), normal(3)
    real, intent(inout)    :: pixel(3)

      call trace(from, dir, pixel, point, normal)
      call colour(point, normal, luz, pixel)
      call reflection(point, normal, dir, luz, pixel)

  end subroutine combo

  subroutine maxes(arr1, arr2, arrOut)
    implicit none
    real        :: arr1(3), arr2(3), arrOut(3)

      WHERE (arr1 > arr2)
        arrOut = arr1
      ELSEWHERE
        arrOut = arr2
      END WHERE
  end subroutine

  subroutine mines(arr1, arr2, arrOut)
    implicit none
    real        :: arr1(3), arr2(3), arrOut(3)

      WHERE (arr1 < arr2)
        arrOut = arr1
      ELSEWHERE
        arrOut = arr2
      END WHERE
  end subroutine

  subroutine addColour(col1, k1, col2, k2)
    ! pixel colors col1 and col2 are added in proportion
    ! k1 and k2. The result placed back in col1, trimmed
    ! to a max value of 1. in each element.
    implicit none
    real, intent(in)    :: col2(3), k1, k2
    real, intent(inout) :: col1(3)
    real :: temp(3)
      call mines( (/ 1.,1.,1. /), real(k1*col1 + k2*col2), col1 )

  end subroutine addColour

  subroutine reflection(point, normal, dir, luz, pixel)
    implicit none
    real*8, intent(in) :: point(3), normal(3), dir(3), luz(3)
    real               :: pixel(3), pix2(3), pix3(3)
    real*8             :: reflex(3), p2(3), nor2(3) ! For reflected ray trace
    real*8             :: dumP(3), dumN(3), dumR(3)
    logical            :: hit, inReflection
    integer            :: i

      inReflection = .true.
      dumP=point
      dumN=normal
      call reflected(dir, dumN, reflex)

      do i=1, Reflections
        call trace(dumP, reflex, pix2, p2, nor2, hit, inReflection)
        if (hit) then
          call colour(p2, nor2, luz, pix2)
          call addColour(pixel,1.,pix2, &
                        (Kreflect**i))!*max(0.,real(sum(dumN*reflex))))
        else
          exit
        end if
        dumP=p2
        dumR=reflex
        dumN=nor2
        call reflected(dumR,dumN,reflex)
      end do

  end subroutine reflection

  subroutine colour(point, normal, luz, pixel)
    implicit none
    real*8, intent(in) :: point(3), normal(3), luz(3)
    real               :: pixel(3), shade(3)

      shade = focLt*max(0., sum(normal*luz))*shadow(point, luz, shadLvl) ! Directed light and casted shadow
      shade = shade + ambLt*ambocl(point, normal, ambOclu) ! Ambient light and oclusion
      pixel=pixel*shade

  end subroutine colour

 ! subroutine reflection2()

  subroutine trace(from, direction, pixel, point, normal, hit, inReflection)

    implicit none
    real                            :: pixel(3), px(3)
    real*8, intent(out)             :: point(3), normal(3)
    logical, optional, intent(out)  :: hit
    logical, optional               :: inReflection
    logical                         :: reflectionState
    real*8                          :: from(3), direction(3), p(3), q(3)
    real*8                          :: totalDistance, distance, t
    integer                         :: steps

    if (present(inReflection)) then
      reflectionState = inReflection
    else
      reflectionState = .false.
    end if
    totalDistance = 0   ! Distance to current point in space
    steps=0
    p=from
    distance=DE(p)
    ! If the ray is a reflection, first it
    ! marches away from reflection surface
    if (reflectionState) then
      do while(distance<minD .and. steps<MaxRaySteps/10)
        steps=steps+1
        totalDistance = totalDistance+distance  ! Advance the distance to the nearest object
        p = from + totalDistance * direction ! The current point in the space
        distance = DE(p)
      end do
      if (distance<minD) then ! Couldn't march away from the surface
        pixel=0.
        if (present(hit)) hit=.false.
      else
        reflectionState=.false.
      end if
    end if

    ! If march-away has worked, the inReflection state
    ! is left and thus the ray trace can go on
    if (.not. reflectionState) then
      do while (((distance)>minD).and.(steps<MaxRaySteps)&
        .and.(totalDistance<maxD))
        steps=steps+1
        totalDistance = totalDistance+distance  ! Advance the distance to the nearest object
        p = from + totalDistance * direction ! The current point in the space
        distance = DE(p, pixel) ! Find the distance to the nearest object, no matter in which direction
      end do
      if (distance>minD)then  ! Nothing found
        pixel=0.
        if (present(hit)) hit=.false.
      else
        if (present(hit)) hit=.true.
        call normalVersor(p,normal)
        ! In order for the next reflections to work point
        ! must be stepped away from the surface.
        point=p+normal*minD
      end if
    end if

  end subroutine trace

  subroutine normalVersor(p, n)
    implicit none
    real*8, intent(in)  :: p(3)
    real*8, intent(out) :: n(3)
    ! Use finite difference to get the normal to the surface in a point
    n=(/ DE(p+DerivStep*(/1,0,0/))-DE(p-DerivStep*(/1,0,0/)),&
      DE(p+DerivStep*(/0,1,0/))-DE(p-DerivStep*(/0,1,0/)),&
        DE(p+DerivStep*(/0,0,1/))-DE(p-DerivStep*(/0,0,1/)) /)
    n=n/sqrt(sum(n**2))
  end subroutine normalVersor

  real function shadow(p, direction, nivel)
    real, parameter :: minDs=0.001, maxDs=20.
    real*8          :: direction(3), p(3), dist
    real            :: t, nivel
  !   integer :: s

  !   s=0
    t=20*minDs
    dist=DE(p+direction*t)
    shadow=1.

    do while ((t<maxDs).and.(dist>minDs))
      dist=DE(p+direction*t)
      t=t+dist
      !write(*,*) dist, t
      if (dist<minDs) then
        !write(*,*) "Se hallo shadow",p
        shadow=0.
      else
        !write(*,*) "Nada de shadow"
        shadow=min(shadow,nivel*dist/t)
      end if
    end do
    shadow=max(shadow,0.)

  end function shadow

  real function ambocl(p, normal, k)
    real, parameter :: minDo=0.1, maxDo=100.
    real*8  :: p(3)
    real*8  :: normal(3)
    real    :: k
    integer :: i

    ambocl=0. !sqrt(sum((p-normal*0.8)**2))
    do i=1,5
      ambocl = ambocl+(i*minDo-DE(p+normal*i*minDo))/2**i
    end do
    ambocl=max(0.,1.-k*ambocl)

  end function ambocl

end module DErender4
