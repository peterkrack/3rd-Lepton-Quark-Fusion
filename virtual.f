c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
      subroutine setvirtual(p,vflav,virtual)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'PhysPars.h'
      include 'pwhg_physpar.h'
      include 'pwhg_kn.h'
      include 'pwhg_flg.h'
      real * 8 p(0:3,nlegborn)
      integer vflav(nlegborn)
      integer i,j
      real * 8 virtual,mU,g4,betaL,betaR
      real * 8 born,dummy(0:3,0:3)
      real *8 s,dotp
      external dotp

      if (abs(vflav(1)) .eq. 11 .or. abs(vflav(2)) .eq. 11) then
         j = 1
      elseif (abs(vflav(1)) .eq. 13 .or. abs(vflav(2)) .eq. 13) then
         j = 2
      elseif (abs(vflav(1)) .eq. 15 .or. abs(vflav(2)) .eq. 15) then
         j = 3
      elseif (abs(vflav(1)) .eq. 0 .or. abs(vflav(2)) .eq. 0) then
         g4 = 0d0
      else
         WRITE(*,*) "Coupling is set to zero for this process.", vflav
      endif

      if (abs(vflav(1)) .eq. 1 .or. abs(vflav(2)) .eq. 1) then
         i = 1
      elseif (abs(vflav(1)) .eq. 2 .or. abs(vflav(2)) .eq. 2) then
         i = 1
      elseif (abs(vflav(1)) .eq. 3 .or. abs(vflav(2)) .eq. 3) then
         i = 2
      elseif (abs(vflav(1)) .eq. 4 .or. abs(vflav(2)) .eq. 4) then
         i = 2
      elseif (abs(vflav(1)) .eq. 5 .or. abs(vflav(2)) .eq. 5) then
         i = 3
      elseif (abs(vflav(1)) .eq. 6 .or. abs(vflav(2)) .eq. 6) then
         i = 3
      elseif (abs(vflav(1)) .eq. 0 .or. abs(vflav(2)) .eq. 0) then
         g4 = 0d0
      else
         write(*,*) "Coupling is set to zero for this process.", vflav
      endif

      mU = ph_mU
      g4 = ph_g4

      betaL=ph_betaL(i,j)
      betaR=ph_betaR(i,j)

      if (flg_QEDonly) then
c     if only QED, virtual is zero
         virtual = 0d0

      else

         if (ph_BWgen_finitewidth) then
            s = p(0,3)**2-p(1,3)**2-p(2,3)**2-p(3,3)**2
            virtual= 2d0*g4**2/3d0 * s *(13d0/12d0 - 4d0*pi/sqrt(3d0)
     1           +pi*pi/2d0 - (5d0/2d0)*dlog(st_muren2/s) -
     2           (0.5d0*(dlog(st_muren2/s))**2) )

c     correct for 'running' width         
            virtual = virtual * s / ph_mU**2

         else
            s = p(0,3)**2-p(1,3)**2-p(2,3)**2-p(3,3)**2
            virtual= 2d0*g4**2/3d0 * (mU**2) *(13d0/12d0 - 4d0*pi/sqrt(3d0)
     1           +pi*pi/2d0 - (5d0/2d0)*dlog(st_muren2/s) -
     2           (0.5d0*(dlog(st_muren2/s))**2) )
         endif

c     add BR into all lepton-jet channels
         virtual = virtual * ph_wLQ_ljall/ph_wLQ
c     add the coupling
         virtual = virtual *(betaL**2+betaR**2)/2d0

      endif

      end
