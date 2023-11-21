! Randomly smears the momentum of the particle depending on the id
! for hadrons it uses a resolution  0.8/sqrt(pt)+0.024
! for electrons and photon it uses  0.028/sqrt(E)+0.12/E+0.003
! for muons                         0.02 + pt * 0.04/1000
subroutine smearmom(p,id)
  implicit none
  real * 8 p(4)
  integer id
  real * 8 pt, e, smear
  pt=sqrt(p(1)**2+p(2)**2)
  e=p(4)
  smear=rangauss()
  select case(abs(id))
  case (11,22)
     smear = smear * resecal(pt)
  case (13)
     smear = smear * resmusys(pt)
  case default
     ! hadrons
     smear = smear * reshcal(pt)
  end select
  p=p*(1+smear)
contains

  function resecal(e) result(res)
    real * 8 e,res
    res = sqrt((0.028/sqrt(e))**2+(0.12/e)**2+(0.003)**2)
  end function resecal

  function reshcal(pt) result(res)
    real * 8 pt,res
! Rough fit of fig. 38 in CMS 1607.03663    
    res = 0.8/sqrt(pt)+0.024
  end function reshcal

  function resmusys(ppt) result(res)
    real * 8 ppt,res
    real * 8 pt(12),resolution(12)
    integer j
    data (pt(j),resolution(j),j=1,12)/7.3, 9.4e-3,13.8,9.9e-3,24.2,1.1e-2,34.2,1.13e-2,&
         &44.6,1.25e-2,60.8,1.41e-2,86.8,1.56e-2,121,2.16e-2,171,2.75e-2,&
         &257,3.32e-2,410,3.82e-2,902,5.76e-2/
    ! Fig 9 of CMS 1804.04528v2

    if (ppt<pt(1)) then
       res=1d-5
       return
    endif
    
    do j=1,12
       if(ppt<pt(j)) exit
    enddo
    j=min(j,12)
    res=resolution(j-1)+(resolution(j)-resolution(j-1))*(ppt-pt(j-1))/(pt(j)-pt(j-1))
  end function resmusys

  function rangauss() result(res)
    implicit none
    real * 8 res,r2,theta,v1,v2
    real * 8, parameter :: pi=3.141592653589793d0
    call random_number(v1)
    call random_number(v2)
    r2=-2*log(v1)
    theta=2*pi*v2
    res = sqrt(r2)*sin(theta)
    if(abs(res)>4) then
       ! we limit to no more than 4 standard deviations (we never know)
       res=0
    endif
  end function rangauss

end subroutine smearmom
