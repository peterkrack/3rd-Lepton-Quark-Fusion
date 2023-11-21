      subroutine born_phsp(xborn)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'pwhg_math.h'
      include 'pwhg_physpar.h'
      include 'PhysPars.h'
      real * 8 xborn(*)
      real * 8 m2, xjac, tau, y, beta, vec(3), s, z, zhigh, zlow
      integer i
      integer, parameter:: lflav=3
      integer flav(lflav),res(lflav)

      real *8 mass,mass2,width,smin,smax,zmin,zmax,mgamma,wt,tanz
      
c     incoming flavours are irrelevant here.
c     the 42 code is necessary to get the mass of the
c     leptoquark from the physpar_phspmasses(42),
c     and  physpar_phspwidth(42)
c     that is set in init_processes

      flav = flst_born(:,1)
      res = [0,0,0]

      kn_masses = 0d0

      wt=1d0
      
      if (ph_BWgen_finitewidth) then

         mass = ph_mU
         mass2 = mass**2
         width = ph_wLQ

         smin = ph_LQmasslow**2
         smax = ph_LQmasshigh**2
         
         mgamma = mass * width

         zmin = atan((smin-mass2)/mgamma)
         zmax = atan((smax-mass2)/mgamma)
         z  = zmin + (zmax-zmin) * xborn(1)
         wt = (zmax - zmin)/pi
         tanz =  tan(z)
         s = tanz * mgamma + mass2
         
         kn_masses(3) = dsqrt(s)
         kn_minmass = dsqrt(s)

         physpar_phspmasses(42) = dsqrt(s)
                  
         call genphasespace(xborn(2:3),lflav,flav,res,kn_beams,
     1        kn_jacborn,kn_xb1,kn_xb2,kn_sborn,kn_cmpborn, kn_pborn)

         kn_jacborn = wt*kn_jacborn

      else

         kn_masses(3) = physpar_phspmasses(42)
         kn_minmass = physpar_phspmasses(42)
         
         call genphasespace(xborn,lflav,flav,res,kn_beams,
     1        kn_jacborn,kn_xb1,kn_xb2,kn_sborn,kn_cmpborn, kn_pborn)
         
      endif
      
      
      end

      subroutine born_suppression(fact)
      implicit none
      double precision fact
c Not necessary for this process
      fact = 1
      end


      subroutine set_fac_ren_scales(muf,mur)
      implicit none
      include 'pwhg_physpar.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
c      include 'pwhg_em.h'
      include 'PhysPars.h'
      real * 8 muf,mur
c      real * 8 muf,mur,alphaqed
      logical ini
      data ini/.true./
      real *8 muref
      real *8 dotp
      external dotp
      logical runningscales
      save runningscales
c      logical runningscales,fixed_alphaQED
c      save runningscales,fixed_alphaQED
      real * 8 pt2
      real * 8 powheginput
      external powheginput
      character * 30 proc
      common/cproc/proc
      if(ini) then
c default is true
         if(powheginput('#runningscale').eq.0) then
            runningscales=.false.
         else
            runningscales=.true.
         endif

c$$$         fixed_alphaQED = powheginput('#fixed_alphaQED').eq.1
         
      endif
      if (runningscales) then
c         muref=sqrt(2*dotp(kn_pborn(:,1),kn_pborn(:,2)))/2
         muref=sqrt(2*dotp(kn_pborn(:,1),kn_pborn(:,2)))
      else
         muref= ph_mU
      endif
      muf=muref
      mur=muref

c$$$c     set QED coupling
c$$$c     notice that here we assume the same mur for QCD and QED
c$$$      if ( .not. fixed_alphaQED ) then 
c$$$         em_alpha = alphaqed(mur**2)
c$$$      endif

      end
      
