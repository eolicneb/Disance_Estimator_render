module DEparam

  !
  logical, parameter    :: in3D=.false., eyesCrossed=.true.
  !
  integer, parameter    :: forma=2
  ! Image dimensions in pixels
  integer*4, parameter  :: horResolution=1000
  integer*4, parameter  :: verResolution=1200
  ! subSample*subSample aditional points will be sampled
  integer*4, parameter  :: subSample=3
  ! Viewport width to distance-to-observer ratio
  real, parameter       :: viewAngle=0.6
  ! Observer's viewpoint and between-eyes distance
  real*8, parameter     :: ojos=.5 ! Meassured in spatial units (for 3D only)
  real*8, parameter     :: from(3)=(/4., .3, 4./)
  real*8, parameter     :: targ(3)=(/0., 0., 0./)
  ! Rendering parameters
  integer, parameter    :: Reflections=1
  real, parameter       :: Kreflect=0.3
  real, parameter       :: shadLvl=10. ! Shadow cone sharpness
  real, parameter       :: ambOclu=1.5 ! Ambient oclusion level
  real, parameter       :: ambLt=0.25   ! Ambient light
  real, parameter       :: focLt(3)=(/.6,.8,1./)*0.7 ! light from source
  ! Ray marching parameters
  real, parameter       :: minD=0.001, maxD=100., DerivStep=0.0001 ! meassured in spatial units
  integer, parameter    :: MaxRaySteps=200

end module DEparam
