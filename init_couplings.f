      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'nlegborn.h'
      include 'pwhg_kn.h'
      include 'pwhg_physpar.h'
      include 'pwhg_em.h'
      include 'pwhg_flst.h'
      real * 8 mass_low,mass_high,tmpwidth
      real * 8 powheginput
      integer i,j
      external powheginput
      logical verbose
      parameter(verbose=.true.)
c     Just a value
      st_alpha=0.12
      em_alpha = 1/137.035999084d0
      kn_ktmin=0
      
      ph_mU=powheginput("#mU")
      ph_LQc=powheginput("#charge")
      if (ph_mU.lt.0) ph_mU = 500d0
      write(*,*) 'leptoquark mass set to ',ph_mU
      physpar_phspmasses(42)=ph_mU

      ph_g4=powheginput("#g4")
      if (ph_g4.lt.0) ph_g4 = 1d0
      write(*,*) 'Coupling g4 set to',ph_g4


      ph_LQmasslow = powheginput("#LQmasslow") 
      ph_LQmasshigh= powheginput("#LQmasshigh") 
      if (ph_LQmasslow<0d0) ph_LQmasslow = 1d0 
      if (ph_LQmasshigh<0d0) ph_LQmasslow = 2*ph_mU 
                     

      if (powheginput("#BWgen") == 1) then
         ph_BWgen_finitewidth = .true.
      else
         ph_BWgen_finitewidth = .false.
      endif

c      Read betaL and betaR from the runcard
      ph_betaL(1,1)=powheginput("#betaL_1e")
      ph_betaL(2,1)=powheginput("#betaL_2e")
      ph_betaL(3,1)=powheginput("#betaL_3e")
      ph_betaL(1,2)=powheginput("#betaL_1m")
      ph_betaL(2,2)=powheginput("#betaL_2m")
      ph_betaL(3,2)=powheginput("#betaL_3m")
      ph_betaL(1,1)=powheginput("#betaL_1t")
      ph_betaL(2,3)=powheginput("#betaL_2t")
      ph_betaL(3,3)=powheginput("#betaL_3t")
 
      ph_betaR(1,1)=powheginput("#betaR_1e")
      ph_betaR(2,1)=powheginput("#betaR_2e")
      ph_betaR(3,1)=powheginput("#betaR_3e")
      ph_betaR(1,2)=powheginput("#betaR_1m")
      ph_betaR(2,2)=powheginput("#betaR_2m")
      ph_betaR(3,2)=powheginput("#betaR_3m")
      ph_betaR(1,1)=powheginput("#betaR_1t")
      ph_betaR(2,3)=powheginput("#betaR_2t")
      ph_betaR(3,3)=powheginput("#betaR_3t")

c     the user can provide the LQ width by theirself
c     otherwise it is computed without assuming any SU(2) relations
c     Calculate the total decay width.
      ph_wLQ_ljall = 0d0
c     Decay to d,s,b
      do i=1,3
         do j=1,3
c     LO partial width
            ph_wLQ_ljall = ph_wLQ_ljall 
     1           +ph_g4**2 *(ph_betaL(i,j)**2 
     2           +ph_betaR(i,j)**2)/3d0 *ph_mU/16/pi
         enddo
      enddo
c     Decay to u,c,t quarks
      do i=1,3
         do j=1,3
c     LO partial width
            ph_wLQ_ljall = ph_wLQ_ljall +
     1           ph_g4**2 *(ph_betaL(i,j)**2)/3d0 *ph_mU/16/pi
         enddo
      enddo
      ph_wLQ=powheginput("#widthLQ")
      if (ph_wLQ < 0) then
         ph_wLQ = ph_wLQ_ljall
      endif

      write(*,*) 'leptoquark width set to ',ph_wLQ
      physpar_phspwidths(42)=ph_wLQ

c      quark and lepton masses

      physpar_mq(1:3) = 0d0
      physpar_mq(4) = 1.5d0
      physpar_mq(5) = 4.75d0
      physpar_mq(6) = 172.0d0
      physpar_ml(1) = 0.000511d0
      physpar_ml(2) = 0.105d0
      physpar_ml(3) = 1.77d0

c     number of light flavors
      st_nlight = 5
      flst_ncollparticles = 12
      flst_collparticles(1:flst_ncollparticles)=
     1     (/ 0,1,2,3,4,5,6,11,13,15,21,22 /)
      end
