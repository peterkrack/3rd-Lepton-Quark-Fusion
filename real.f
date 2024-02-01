      subroutine setreal(p,rflav,amp2real)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_math.h'
      include 'pwhg_st.h'
      include 'pwhg_em.h'
      include 'pwhg_physpar.h'
      include 'PhysPars.h'
      real * 8 msq
      real * 8 p(0:3,nlegreal)
      real * 8 p1(0:3),k(0:3),q(0:3), p2(0:3)
      integer rflav(nlegreal),mcfmflav(nlegreal)
      real * 8 amp2real
      real * 8 dotp
      external dotp
      real * 8 s,t,u
      real * 8 g4,mU2,betaL,betaR
C      real * 8 kq,kp1,kp2,p1p2,qp1
      real * 8 qlq,ql,qq
      integer i,j
      logical initialgluon,finalgluon,photon
      data initialgluon/.false./
      save initialgluon
      data finalgluon/.false./
      save finalgluon
      data photon/.false./
      save photon

      photon = .false.
      initialgluon = .false.
      finalgluon = .false.

      if (ph_BWgen_finitewidth) then
         mU2=p(0,3)**2-p(1,3)**2-p(2,3)**2-p(3,3)**2
      else
         mU2 = ph_mU**2
      endif
      if(any(abs(rflav) .eq. 1)) i = 1
      if(any(abs(rflav) .eq. 2)) i = 1
      if(any(abs(rflav) .eq. 3)) i = 2
      if(any(abs(rflav) .eq. 4)) i = 2
      if(any(abs(rflav) .eq. 5)) i = 3
      if(any(abs(rflav) .eq. 6)) i = 3
      if(any(abs(rflav) .eq. 11)) j = 1
      if(any(abs(rflav) .eq. 13)) j = 2
      if(any(abs(rflav) .eq. 15)) j = 3
      
      betaL = ph_betaL(i,j)
      betaR = ph_betaR(i,j)
c There are two NLO QCD corrections involving gluons
c q(p1)+l(p2) -> g(k)+U(q) soft gluon emission
c g(p1)+l(p2) -> q(k)+U(q) gluon initiated process
c There is one NLO QED correction
c gamma(p1)+q(p2) -> l(k)+U(q)
c Assing the momenta the values from the paper
      do j=1,nlegreal
         if (rflav(j) .eq. 0) then
            if (j .gt. 2) then
               finalgluon = .true.
            else
               initialgluon = .true.
            endif
         endif
         if (rflav(j) .eq. 22) then
            if (j .le. 2) then 
               photon = .true.
            else
               write(*,*) "Photon found in final state. Exiting."
               call exit(-1)
            endif
         endif
      enddo

      if (finalgluon.and.initialgluon) then
         write(*,*) "Gluon found in initial and final state."
         call exit(-1)
      endif
      if (finalgluon.and.photon) then
         write(*,*) "Gluon and photon found in the same process."
         call exit(-1)
      endif
      if (initialgluon.and.photon) then
         write(*,*) "Gluon and photon found in the same process."
         call exit(-1)
      endif

      if (finalgluon) then
         do j=1,nlegreal 
            if (rflav(j).eq.0) k(:)=p(:,j)
            if (rflav(j).eq.42) q(:)=p(:,j)
            if (abs(rflav(j)).le.6.and.rflav(j).ne.0) p1(:)=p(:,j)
            if (abs(rflav(j)).le.16.and.abs(rflav(j)).ge.11) then
               p2(:)=p(:,j)
            endif
         enddo
      endif
      if (initialgluon) then
         do j=1,nlegreal 
            if (rflav(j).eq.0) p1(:)=p(:,j)
            if (rflav(j).eq.42) q(:)=p(:,j)
            if (abs(rflav(j)).le.6.and.rflav(j).ne.0) k(:)=p(:,j)
            if (abs(rflav(j)).le.16.and.abs(rflav(j)).ge.11) then
               p2(:)=p(:,j)
            endif
         enddo
      endif
      if (photon) then
         do j=1,nlegreal 
            if (rflav(j).eq.22) p1(:)=p(:,j)
            if (rflav(j).eq.42) q(:)=p(:,j)
            if (abs(rflav(j)).le.6.and.rflav(j).ne.0) p2(:)=p(:,j)
            if (abs(rflav(j)).le.16.and.abs(rflav(j)).ge.11) k(:)=p(:,j)
         enddo
      endif


c Determine the Mandelstam variables and compute the correct real
c amplitude for the process.
      call mandelstam(p1,p2,k,q,s,t,u)
      g4 = ph_g4

      if (finalgluon) then 
         amp2real=32d0/3d0*pi**2d0*g4**2*u
     1             *(u**2+t**2+2*s*mU2)/(t*(u+t)**2)
      endif
      if (initialgluon) then 
         amp2real=-4d0*pi**2d0*g4**2*s
     1             *(s**2+t**2+2*u*mU2)/(t*(s+t)**2)
      endif

      if (photon) then
         do j=1, nlegreal
            if (abs(rflav(j)) .le. 6) then
               if((abs(rflav(j)) .eq. 2) .or. (abs(rflav(j)) .eq. 4)) then
                  qq = 2d0/3d0
               else
                  qq = -1d0/3d0
               endif
               if(rflav(j) .lt. 0) qq = -1d0 * qq
            endif
            if (abs(rflav(j)) .gt. 6 .and. (abs(rflav(j)) .le. 15)) then
               if (rflav(j) .lt. 0) then
                  ql = 1d0
               else
                  ql = -1d0                                 
               endif
            endif
         enddo
         qlq = qq -ql
         amp2real = -8d0*pi**2 *g4**2*(ql*s+qq*t)**2 
     1              *(s**2+t**2 +2d0*u*mU2)/(s*t*(s+t)**2)
         amp2real = amp2real * em_alpha /st_alpha
      endif

c        correct for running width if BW generation is on
      if (ph_BWgen_finitewidth) then
         amp2real = amp2real * dotp(q,q)/ph_mU**2
      endif

c        add BR into all lepton-jet channels
      amp2real = amp2real * ph_wLQ_ljall/ph_wLQ
      amp2real=(betaL**2+betaR**2)*amp2real/2d0
      end

      subroutine regularcolour_lh
      write(*,*) ' regularcolour_lh: there are no regulars in this
     1  process'
      write(*,*) ' exiting ...'
      call exit(-1)
      end

      subroutine mandelstam(p1,p2,k,q,s,t,u)
      implicit none
      real * 8 p1(0:3),k(0:3),q(0:3), p2(0:3)
      real * 8 s,t,u
      real * 8 dotp
      external dotp
      s = 2*dotp(p1,p2)
      t = -2*dotp(p1,k)
      u = -2*dotp(p1,q)+dotp(q,q)
      end
