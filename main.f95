program DErender
  use DEparam
  use bmp
  use DErender4

  implicit none
  integer*2,dimension(1:2048,1:2048,1:3):: out
  integer*4 :: hor,ver,i ,j, k, h
  real*8    :: direction(3), dir3
  real*8    :: luz(3)
  real*8    :: dir(3)
  real*8    :: point(3), normal(3)
  real*8    :: dirx(3), diry(3), ojoX(3)
  real*8    :: eye(3)
  real      :: pixel(3), io, jo, iD, jD, p3D(3)
  integer*2 :: side, step, coef3D, completed=1

  direction = targ - from ! the main axis of the pyramidal ray-marching
  if (in3D) then
    coef3D=2
    dirx(:2)=(/ -direction(2), direction(1) /) !Este vector después se puede girar para rotar la cámara
    dirx=-dirx/sqrt(sum(dirx**2))
    ojoX = dirx*ojos/2
    if (eyesCrossed) ojoX = -ojoX
  else
    coef3D=1
    ojoX=(/0,0,0/)
  end if

  hor=horResolution*coef3D
  ver=verResolution

  luz=focLt
  luz=luz/sqrt(sum(luz**2))

  out=0
  do side=-1,2*coef3D-3,2
    ! Left and right eye positions in space
    eye=from+side*ojoX
    direction = targ - eye
    ! dirx and diry are the spatial versors of the x and y
    ! axis of the screen placed in the space
    dirx(:2)=(/ -direction(2), direction(1) /)
    diry=(/ direction(1), direction(2), -(direction(1)**2+direction(2)**2)/direction(3) /)
    direction=direction/sqrt(sum(direction**2))
    dirx=-dirx/sqrt(sum(dirx**2))*viewAngle*coef3D/hor
    diry=diry/sqrt(sum(diry**2))*viewAngle*coef3D/hor
    jo=ver/2.
    io=hor/2./coef3D
    do j=1,ver
      do i=1,hor/coef3D
        p3D=0
        do k=0,(subSample-1)
          do h=0,(subSample-1)
            if (subSample==1)then
              iD=0; jD=0
            else
              iD=k/(subSample-1.)-0.5; jD=h/(subSample-1.)-0.5
            end if
            dir = direction + (i-io+iD)*dirx + (j-jo+jD)*diry
            dir = dir/sqrt(sum(dir**2))
            call combo(eye, dir, luz, pixel, point, normal)
            p3D = p3D + pixel/subSample**2
      !     write(*,*)i,j,dir
          end do !h
        end do !k
        out(j,i+hor/4*(side+1),:)=int(p3D*255)
      end do !i
      ! Progress indicator
      if (j*10/ver>=completed) then
        write(*,*)10*completed,"% completed."
        completed=completed+1
      end if
    end do !j
  end do

  call saveBMP("render.bmp",out,hor,ver,1,1,1)

end program DErender
