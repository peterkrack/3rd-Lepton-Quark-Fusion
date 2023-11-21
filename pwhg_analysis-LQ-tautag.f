c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  bookup  : opens histograms
c  filld   : fills histograms with data

c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  'LesHouches.h'
      include  'pwhg_math.h'
      character * 30 proc
      common/cproc/proc
      real * 8 ptcutjet,ptcutlep,etacutjet,etacutlep,
     1         mLQcuthi,mLQcutlo,phirec,dphilptmisscut,drljetcut,
     2         ptmisscut,tageff
      common/ccuts/ptcutjet,ptcutlep,etacutjet,etacutlep,
     1             mLQcuthi,mLQcutlo,phirec,dphilptmisscut,drljetcut,
     2             ptmisscut,tageff
      real * 8 powheginput
      logical flagsmear
      common/cflagsmear/flagsmear
      logical ptmissh
      common/cptmissh/ptmissh
      integer lqlep
      common/clqlep/lqlep
      logical recombination
      common/crecombination/recombination

c$$$      call powheginputstring('whichproc',proc)

c$$$      if(proc == 'LQumu') then
c$$$         lqlep = 13
c$$$         if(powheginput('#ingamma')==1) lqlep=-lqlep
c$$$      elseif(proc == 'LQue') then
c$$$         lqlep = 11
c$$$         if(powheginput('#ingamma')==1) lqlep=-lqlep
c$$$      endif
      recombination = .not. (powheginput('#recombination') .eq. 0) 
      flagsmear = .not. (powheginput('#smear') == 0)
      ptmissh   = .not. (powheginput('#ptmissh') == 0)
      call inihists

      ptcutlep=powheginput('#ptcutlep')
      if (ptcutlep .lt. 0) ptcutlep=500d0

      ptcutjet=powheginput('#ptcutjet')
      if (ptcutjet .lt. 0) ptcutjet=500d0

      etacutlep=powheginput('#etacutlep')
      if (etacutlep .lt. 0) etacutlep=2.5d0

      etacutjet=powheginput('#etacutjet')
      if (etacutjet .lt. 0) etacutjet=2.5d0

      mLQcuthi=powheginput('#mLQcuthi')
      if (mLQcuthi .lt. 0) mLQcuthi=huge(mLQcuthi)

      mLQcutlo=powheginput('#mLQcutlo')
      if (mLQcutlo .lt. 0) mLQcutlo=0
      
      phirec=powheginput('#phirec')
      if (phirec .lt. 0) phirec=0.4

      dphilptmisscut=powheginput('#dphilptmiss')
      if (dphilptmisscut .lt. 0) dphilptmisscut=0.3
      
      drljetcut=powheginput('#drljetcut')
      if (drljetcut .lt.0 ) drljetcut=0.5

      ptmisscut=powheginput('#ptmisscut')
      if (ptmisscut .lt.0 ) ptmisscut=100

      tageff=powheginput('#tageff')
      if(tageff.lt.0) tageff=0.6d0

      call bookupeqbins('xsec',1d0,0d0,1d0) 
      call bookupeqbins('y-LQ',0.2d0,-4d0,4d0)   
      call bookupeqbins('m-LQ',20d0,100d0,5000d0)
      call bookupeqbins('pt-lep1',10d0,50d0,2500d0)
      call bookupeqbins('pt-jet1',10d0,50d0,2500d0)
      call bookupeqbins('eta-lep1',0.2d0,-4d0,4d0)
      call bookupeqbins('eta-lep2',0.2d0,-4d0,4d0)
      call bookupeqbins('eta-jet1',0.2d0,-4d0,4d0)
      call bookupeqbins('pt-sys',10d0,500d0,3500d0)
      call bookupeqbins('dphi',0.2d0,0d0,3.2d0)
      call bookupeqbins('dr',0.2d0,0d0,6.4d0)
      call bookupeqbins('pt-LQ',10d0,50d0,3000d0)
      call bookupeqbins('eta-LQ',0.2d0,-4d0,4d0)
      call bookupeqbins('ptmiss',10d0,50d0,2500d0)
      call bookupeqbins('mcoll',20d0,100d0,5000d0)
      call bookupeqbins('pt-taujet',10d0,50d0,2500d0)

c      call bookupeqbins('y-LQrec',0.2d0,-4d0,4d0)   

      end


      subroutine analysis(dsig0)
      implicit none
      real * 8 dsig0
      integer, parameter :: maxweights=200
      real * 8 dsig(maxweights)
      include 'hepevt.h'
      include 'pwhg_math.h' 
      include  'LesHouches.h'
      integer, parameter :: maxjet=2048

      logical, save :: ini = .true.

      real * 8 rr,kt(maxjet),eta(maxjet),rap(maxjet),
     1     phi(maxjet),qjet(4,maxjet),pjet(4,maxjet)
      integer njets,mjets
      
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/

      integer ihep,nleps,i1,i2,id1,id2,nphoton,ntau,ntaujet,nnu
      integer, parameter :: maxleps=500
      integer ilep(maxleps),iphoton(maxleps),itau(maxleps),inu(maxleps)
      integer itaujet(maxleps),taujet(maxleps)
      integer itmp
      integer i,j
      real * 8 powheginput,y1,eta1,pt1,mass1,y2,eta2,pt2,
     1     mass2,mrec,ptmiss,yjet1,etajet1,ptjet1,massjet1,
     2     yjet2,etajet2,ptjet2,massjet2,yleprec,etaleprec,
     3     ptleprec,massleprec,dphileprec,drphoton,
     4     yphoton,etaphoton,ptphoton,massphoton
      character * 30 proc
      common/cproc/proc
      real * 8 ptcutjet,ptcutlep,etacutjet,etacutlep,
     1         mLQcuthi,mLQcutlo,phirec,dphilptmisscut,drljetcut,
     2         ptmisscut,tageff
      common/ccuts/ptcutjet,ptcutlep,etacutjet,etacutlep,
     1             mLQcuthi,mLQcutlo,phirec,dphilptmisscut,drljetcut,
     2             ptmisscut,tageff
      real * 8 pp1(4),pp2(4),ppjet(4),ppjet2(4),pleprec(4),
     1     massofvector,recmass,mreclep,mrecjet,
     2     yresrec,etaresrec,ptresrec,plep(4)
      logical flagsmear
      common/cflagsmear/flagsmear
      integer lqlep
      common/clqlep/lqlep
      logical recombination
      common/crecombination/recombination

      logical dosmear
      common/cdosmear/dosmear
      
      real *8 dphi,ptsys,philep1,phijet1,dr,phiphoton
      real * 8 dylph,detalph,dphilph,drlph
      real * 8 yjet,etajet,ptjet,phijet,massjet
      real * 8 ylep,etalep,ptlep,philep,masslep
      real * 8 ytau,etatau,pttau,phitau,masstau
      real * 8 rand
      integer taufound,bfound
      real * 8 ynu,etanu,ptnu,massnu,sumptnu
      real * 8 ptm(4),xvis,mcoll
      real * 8 dphilptmiss,drljet
      real * 8 pttaujet
      
      interface 
         subroutine getdphi(p1,p2,dphi,phi1,phi2)
         real(kind=8),intent(in)   :: p1(4),p2(4)
         real(kind=8),intent(out)  :: dphi
         real(kind=8),intent(out),optional :: phi1,phi2
         end subroutine
      end interface

      if (ini) then
         write (*,*)
         write (*,*) '********************************************'
         if(whcprg.eq.'NLO') then
            write(*,*) '       NLO analysis'
         elseif(WHCPRG.eq.'LHE   ') then
            write(*,*) '       LHE analysis'
         elseif(WHCPRG.eq.'HERWIG') then
            write (*,*) '           HERWIG ANALYSIS            '
            write(*,*) 'not implemented analysis'
            write(*,*) 'no plots will be present at the end of the run'
          elseif(WHCPRG.eq.'PYTHIA') then
            write (*,*) '           PYTHIA ANALYSIS            '
            write(*,*) 'not implemented analysis'
            write(*,*) 'no plots will be present at the end of the run'
         endif
         write(*,*) '*****************************'
         
         ini=.false.
      endif

c     smearing break NLO cancellation
      dosmear = flagsmear .and. (.not. whcprg.eq.'NLO')
      
      call multi_plot_setup(dsig0,dsig,maxweights)
      call filld('xsec',0.5d0,dsig)

      nphoton = 0
      nleps=0
      do ihep=1,nhep
         if(isthep(ihep).eq.1) then
            if(  abs(idhep(ihep)) == 11 .or.
     1           abs(idhep(ihep)) == 13 .or.
     2           abs(idhep(ihep)) == 15  ) then
               if(nleps<maxleps) then
                  call getyetaptmass(phep(1:4,ihep),y1,eta1,pt1,mass1)
                  if (abs(eta1)<etacutlep) then 
                     nleps = nleps+1
                     ilep(nleps)=ihep
                  endif
               else
                  write(*,*) ' analysis:  too many leptons'
                  write(*,*) ' increase maxleps.'
                  write(*,*) ' exiting ...'
                  call exit(-1)
               endif
            endif
            if (idhep(ihep) .eq. 22) then
               if(nphoton .lt. maxleps) then
                  call getyetaptmass(phep(1:4,ihep),yphoton,etaphoton,
     1                               ptphoton,massphoton)
c                  if (abs(etaphoton) .lt. etacutlep) then
                  nphoton = nphoton+1
                  iphoton(nphoton) = ihep
c                  endif
               else
                  write(*,*) ' analysis:  too many leptons'
                  write(*,*) ' increase maxleps.'
                  write(*,*) ' exiting ...'
                  call exit(-1)
               endif
            endif
         endif
      enddo

C     Make a list of all tau leptons of the shower.
      if (WHCPRG.eq.'HERWIG') then 
         ntau=0
         do ihep=1,nhep
            if(abs(idhep(ihep)).eq.15) then 
               ntau=ntau+1
               itau(ntau)=ihep
            endif
         enddo
      endif
C     Should not stop when no leptons found, since tau may decay to
C     hadrons. 
C      if (nleps==0) return 
            
c This was after recombination; better be before ...
      if(nleps>1) call sortbypt(nleps,ilep(1:nleps))

      if (recombination) then
         do i=1,nleps
            do j=1,nphoton
               if (abs(phep(4,iphoton(j))).gt.0) then 
                  call getdydetadphidr(phep(1:4,ilep(i)),
     1                 phep(1:4,iphoton(j)),dylph,detalph,dphilph,drlph)
                  if (drlph<0.2) then
                     phep(1:4,ilep(i)) = phep(1:4,ilep(i))
     1                    + phep(1:4,iphoton(j))
                     phep(1:4,iphoton(j)) = 0d0
                  endif
               endif
            enddo            
         enddo         
      endif

      i1 = ilep(1)
      if (i1.ge.1) pp1 = phep(1:4,i1)
      if(dosmear) call smearmom(pp1,idhep(i1))
      call getyetaptmass(pp1,y1,eta1,pt1,mass1)
      
      if(nleps > 1) then
         i2 = ilep(2)
         pp2 = phep(1:4,i2)
         if(dosmear) call smearmom(pp2,idhep(i2))
         call getyetaptmass(pp2,y2,eta2,pt2,mass2)
      endif

c     jet radius
      rr = 0.4d0
      call buildjets(1,njets,rr,kt,eta,rap,phi,qjet)

CCC build array of jets in acceptance ordered in pT      
      mjets=0
      do i=1,njets
         call getyetaptmass(qjet(:,i),yjet1,etajet1,ptjet1,massjet1)
         if (abs(etajet1) < etacutjet) then 
            mjets=mjets+1
            pjet(:,mjets) = qjet(:,i)
         endif
      enddo

C     In case of the shower, run the tau tagging for the jets
      if (WHCPRG.eq.'HERWIG') then 
         do i=1,mjets
            ntaujet=0
            call getyetaptmass(pjet(:,i),yjet,etajet,ptjet,massjet)
            do j=1,ntau
               call getyetaptmass(phep(:,itau(j)),ytau,etatau,pttau,
     1                            masstau)
               call getdphi(pjet(:,i),phep(:,itau(j)),dphi,phijet,phitau)
               call getdr(etajet,etatau,phijet,phitau,dr)
               call random_number(rand)
               if (dr.lt.drljetcut.and.rand.lt.tageff) then 
                  ntaujet=ntaujet+1
                  taujet(ntaujet)=i
                  exit
               endif
            enddo
         enddo
      endif
            
                     
C     Fill histograms
      if (WHCPRG.eq.'HERWIG') then
         if (mjets.ge.2) then 
C     Pick the hardest non-tau = bjet and the hardest tau jet
            bfound=0
            taufound=0
            do i=1,mjets
               if (.not.any(taujet.eq.i).and.bfound.eq.0) then
                  ppjet=pjet(:,i)
                  call getyetaptmass(ppjet,yjet1,etajet1,ptjet1,massjet1)
                  bfound=1
               endif
               if (any(taujet.eq.i).and.taufound.eq.0) then
                  pp1=pjet(:,i)
                  call getyetaptmass(pp1,y1,eta1,pt1,mass1)
                  taufound=1
               endif
            enddo
C     If there is a final state lepton, that is harder then the selected
C     tau tagged jet, pick the lepton instead of the jet.
C            if (nleps.ge.1) then
C               plep=phep(1:4,i1)
C               call getyetaptmass(plep,ylep,etalep,ptlep,masslep)
C               if (ptlep.gt.pt1) then
C                  pp1=plep
C                  call getyetaptmass(pp1,y1,eta1,pt1,mass1)
C               endif
C            endif
            if (taufound.eq.1.and.bfound.eq.1) then 
               call getyetaptmass(pp1+ppjet,yresrec,etaresrec,ptresrec,
     1                             mrec)
               call missingpt(ptmiss,ptm)
               call getdphi(ptm,pp1,dphilptmiss)
               call getdphi(pp1,ppjet,dphi,philep1,phijet1)
               call getdr(eta1,etajet1,philep1,phijet1,dr)
               if (abs(eta1).lt.etacutlep.and.abs(etajet1).lt.etacutjet 
     1   .and.pt1.gt.ptcutlep.and.ptjet1.gt.ptcutjet.and.
     2    mrec.gt.mLQcutlo.and.mrec.lt.mLQcuthi.and. 
     3    mjets.gt.0.and.nleps.gt.0 .and. 
     4    abs(dphilptmiss).lt.dphilptmisscut
     5    .and.ptmiss.gt.ptmisscut) then
                     call filld('pt-lep1',pt1,dsig)
                     call filld('eta-lep1',eta1,dsig)
                     call filld('y-LQ',yresrec,dsig)
                     call filld('eta-LQ',etaresrec,dsig)
                     call filld('m-LQ',mrec,dsig)
                     call filld('pt-LQ',ptresrec,dsig)
                     call filld('pt-jet1',ptjet1,dsig)
                     call filld('eta-jet1',etajet1,dsig)
                     ptsys = pt1 + ptjet1
                     call filld('pt-sys',ptsys,dsig)
                     call filld('dphi',dphi,dsig)
                     call filld("dr",dr,dsig)
                     call filld('ptmiss',ptmiss,dsig)
                     xvis=pt1/(pt1+(pp1(1)*ptm(1)+pp1(2)*ptm(2))
     1                      /dsqrt(pp1(1)**2+pp1(2)**2+pp1(3)**2))
                     mcoll=mrec/dsqrt(xvis)
                     call filld("mcoll",mcoll,dsig)
                     call getyetaptmass(pp1+ptm,yresrec,etaresrec,
     1                                  ptresrec,mrec)
                     call filld("pt-taujet",ptresrec,dsig)
               endif
            endif
         endif
      else
         if (mjets > 0) then 
            ppjet=pjet(:,1)
            call getyetaptmass(ppjet,yjet1,etajet1,ptjet1,massjet1)
         endif 
         if( mjets > 0 .and. nleps > 0  ) then
            call getyetaptmass(pp1+ppjet,yresrec,etaresrec,ptresrec,
     1                         mrec)
            call getdphi(pp1,ppjet,dphi,philep1,phijet1)
            call getdr(eta1,etajet1,philep1,phijet1,dr)
         endif
         call missingpt(ptmiss,ptm)
         call getdphi(ptm,pp1,dphilptmiss)
         call getdphi(ptm,pp1,dphilptmiss)
         if (nhep.eq.5.or.nhep.eq.6) then 
            dphilptmiss = 0d0
         endif
         if (abs(eta1) .lt. etacutlep .and. abs(etajet1) .lt. etacutjet 
     1   .and. pt1 .gt. ptcutlep .and. ptjet1 .gt. ptcutjet .and.
     2    mrec .gt. mLQcutlo .and. mrec .lt. mLQcuthi .and. 
     3    mjets .gt. 0 .and. nleps .gt. 0 .and.
     4    abs(dphilptmiss).lt.dphilptmisscut) then
               call filld('pt-lep1',pt1,dsig)
               call filld('eta-lep1',eta1,dsig)
               call filld('y-LQ',yresrec,dsig)
               call filld('eta-LQ',etaresrec,dsig)
               call filld('m-LQ',mrec,dsig)
               call filld('pt-LQ',ptresrec,dsig)
               call filld('pt-jet1',ptjet1,dsig)
               call filld('eta-jet1',etajet1,dsig)
               ptsys = pt1 + ptjet1
               call filld('pt-sys',ptsys,dsig)
               call filld('dphi',dphi,dsig)
               call filld("dr",dr,dsig)
               call missingpt(ptmiss,ptm)
               call filld('ptmiss',ptmiss,dsig)
               xvis=pt1/(pt1+(pp1(1)*ptm(1)+pp1(2)*ptm(2))
     1                     /dsqrt(pp1(1)**2+pp1(2)**2+pp1(3)**2))
               mcoll=mrec/dsqrt(xvis)
               call filld("mcoll",mcoll,dsig)
               pttaujet=pt1/xvis
               call filld("pt-taujet",pttaujet,dsig)
         endif
      endif

      end subroutine

      function massofvector(p)
      implicit none
      real * 8 massofvector,p(4)
      massofvector = sqrt(p(4)**2-p(1)**2-p(2)**2-p(3)**2)
      end


      function recmass(p1,p2)
      implicit none
      real * 8 recmass,p1(4),p2(4)
      real * 8 pt1,pt2,pp(4)
      real * 8 massofvector

c     reconstuct the mass of the p1-p2 system
c     assuming that they are balanced in pt
c     and using only the full p1 and the direction of p2.
c     The idea is that p2 may be well known in direction
c     but not in energy, e.g. the jet that has lost energy
c     out of the cone
      pt1=sqrt(p1(1)**2+p1(2)**2)
      pt2=sqrt(p2(1)**2+p2(2)**2)
      pp = p2*pt1/pt2
      recmass = massofvector(p1+pp)
      end

      subroutine getyetaptmass(p,y,eta,pt,mass)
      implicit none
      real * 8 p(*),y,eta,pt,mass,pv
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      pt=sqrt(p(1)**2+p(2)**2)
      pv=sqrt(pt**2+p(3)**2)
      eta=0.5d0*log((pv+p(3))/(pv-p(3)))
      mass=sqrt(abs(p(4)**2-pv**2))
      end

      subroutine getdydetadphidr(p1,p2,dy,deta,dphi,dr)
      implicit none
      include 'pwhg_math.h' 
      real * 8 p1(*),p2(*),dy,deta,dphi,dr
      real * 8 y1,eta1,pt1,mass1,phi1
      real * 8 y2,eta2,pt2,mass2,phi2
      call getyetaptmass(p1,y1,eta1,pt1,mass1)
      call getyetaptmass(p2,y2,eta2,pt2,mass2)
      dy=y1-y2
      deta=eta1-eta2
      phi1=atan2(p1(1),p1(2))
      phi2=atan2(p2(1),p2(2))
      dphi=abs(phi1-phi2)
      dphi=min(dphi,2d0*pi-dphi)
      dr=sqrt(deta**2+dphi**2)
      end

      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(0:3),y
      y=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(0:3),m
      m=sqrt(abs(p(0)**2-p(1)**2-p(2)**2-p(3)**2))
      end

      subroutine get_pseudorap(p,eta)
      implicit none
      real*8 p(0:3),eta,pt,th
      real *8 tiny
      parameter (tiny=1.d-5)

      pt=sqrt(p(1)**2+p(2)**2)
      if(pt.lt.tiny.and.abs(p(3)).lt.tiny)then
         eta=sign(1.d0,p(3))*1.d8
      elseif(pt.lt.tiny) then   !: added this elseif
         eta=sign(1.d0,p(3))*1.d8
      else
         th=atan2(pt,p(3))
         eta=-log(tan(th/2.d0))
      endif
      end



      subroutine buildjets(iflag,mjets,rr,kt,eta,rap,phi,pjet)
c     arrays to reconstruct jets, radius parameter rr
      implicit none
      integer iflag,mjets
      real * 8  rr,kt(*),eta(*),rap(*),
     1     phi(*),pjet(4,*)
      include   'hepevt.h'
      include  'LesHouches.h'
      integer   maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real * 8  ptrack(4,maxtrack),pj(4,maxjet)
      integer   jetvec(maxtrack),itrackhep(maxtrack)
      integer   ntracks,njets
      integer   j,k,mu,jb
      real * 8 r,palg,ptmin,pp,tmp
      logical islept
      external islept
C - Initialize arrays and counters for output jets
      do j=1,maxtrack
         do mu=1,4
            ptrack(mu,j)=0d0
         enddo
         jetvec(j)=0
      enddo      
      ntracks=0
      do j=1,maxjet
         do mu=1,4
            pjet(mu,j)=0d0
            pj(mu,j)=0d0
         enddo
      enddo
      if(iflag.eq.1) then
C     - Extract final state particles to feed to jet finder
         do j=1,nhep
            if (isthep(j).eq.1.and..not.islept(idhep(j))) then
               if(ntracks.eq.maxtrack) then
                  write(*,*) 'analyze: need to increase maxtrack!'
                  write(*,*) 'ntracks: ',ntracks
                  stop
               endif
               ntracks=ntracks+1
               do mu=1,4
                  ptrack(mu,ntracks)=phep(mu,j)
               enddo
               itrackhep(ntracks)=j
            endif
         enddo
      else
         do j=1,nup
            if (istup(j).eq.1.and..not.islept(idup(j))) then
               if(ntracks.eq.maxtrack) then
                  write(*,*) 'analyze: need to increase maxtrack!'
                  write(*,*) 'ntracks: ',ntracks
                  stop
               endif
               ntracks=ntracks+1
               do mu=1,4
                  ptrack(mu,ntracks)=pup(mu,j)
               enddo
               itrackhep(ntracks)=j
            endif
         enddo
      endif
      if (ntracks.eq.0) then
         mjets=0
         return
      endif
C --------------------------------------------------------------------- C
C - Inclusive jet pT and Y spectra are to be compared to CDF data:    - C    
C --------------------------------------------------------------------- C
C     R = 0.7   radius parameter
C     f = 0.75  overlapping fraction
c palg=1 is standard kt, -1 is antikt
      palg=-1
      r=rr
c      ptmin=20
      ptmin=0.1d0
      call fastjetppgenkt(ptrack,ntracks,r,palg,ptmin,pjet,njets,
     $                        jetvec)
      mjets=njets
      if(njets.eq.0) return
c check consistency
      do k=1,ntracks
         if(jetvec(k).gt.0) then
            do mu=1,4
               pj(mu,jetvec(k))=pj(mu,jetvec(k))+ptrack(mu,k)
            enddo
         endif
      enddo
      tmp=0
      do j=1,mjets
         do mu=1,4
            tmp=tmp+abs(pj(mu,j)-pjet(mu,j))
         enddo
      enddo
      if(tmp.gt.1d-4) then
         write(*,*) ' bug!'
      endif
C --------------------------------------------------------------------- C
C - Computing arrays of useful kinematics quantities for hardest jets - C
C --------------------------------------------------------------------- C
      do j=1,mjets
         kt(j)=sqrt(pjet(1,j)**2+pjet(2,j)**2)
         pp = sqrt(kt(j)**2+pjet(3,j)**2)
         eta(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         rap(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         phi(j)=atan2(pjet(2,j),pjet(1,j))
      enddo
      end



      subroutine sortbypt(n,iarr)
      implicit none
      integer n,iarr(n)
      include 'hepevt.h'
      integer j,k
      real * 8 tmp,pt(nmxhep)
      logical touched
      do j=1,n
         pt(j)=sqrt(phep(1,iarr(j))**2+phep(2,iarr(j))**2)
      enddo
c bubble sort
      touched=.true.
      do while(touched)
         touched=.false.
         do j=1,n-1
            if(pt(j).lt.pt(j+1)) then
               k=iarr(j)
               iarr(j)=iarr(j+1)
               iarr(j+1)=k
               tmp=pt(j)
               pt(j)=pt(j+1)
               pt(j+1)=tmp
               touched=.true.
            endif
         enddo
      enddo
      end



      function islept(j)
      implicit none
      logical islept
      integer j
      if(abs(j).ge.11.and.abs(j).le.16) then
         islept = .true.
      else
         islept = .false.
      endif
      end


c$$$      subroutine boostx(p_in,pt,ptt,p_out)
c$$$      implicit none
c$$$c--- Boost input vector p_in to output vector p_out using the same
c$$$c--- transformation as required to boost massive vector pt to ptt
c$$$      double precision p_in(4),pt(4),ptt(4),p_out(4),
c$$$     . p_tmp(4),beta(3),mass,gam,bdotp
c$$$      integer j
c$$$
c$$$      mass=pt(4)**2-pt(1)**2-pt(2)**2-pt(3)**2  
c$$$      if (mass .lt. 0d0) then
c$$$        write(6,*) 'mass**2 .lt. 0 in boostx.f, mass**2=',mass,pt
c$$$        stop
c$$$      endif
c$$$      mass=dsqrt(mass)
c$$$
c$$$c--- boost to the rest frame of pt
c$$$      gam=pt(4)/mass
c$$$
c$$$      bdotp=0d0
c$$$      do j=1,3
c$$$        beta(j)=-pt(j)/pt(4)
c$$$        bdotp=bdotp+beta(j)*p_in(j)
c$$$      enddo
c$$$      p_tmp(4)=gam*(p_in(4)+bdotp)
c$$$      do j=1,3
c$$$        p_tmp(j)=p_in(j)+gam*beta(j)/(1d0+gam)*(p_in(4)+p_tmp(4))
c$$$      enddo     
c$$$
c$$$c--- boost from rest frame of pt to frame in which pt is identical
c$$$c--- with ptt, thus completing the transformation          
c$$$      gam=ptt(4)/mass
c$$$
c$$$      bdotp=0d0
c$$$      do j=1,3
c$$$        beta(j)=+ptt(j)/ptt(4)
c$$$        bdotp=bdotp+beta(j)*p_tmp(j)
c$$$      enddo
c$$$      p_out(4)=gam*(p_tmp(4)+bdotp)
c$$$      do j=1,3
c$$$        p_out(j)=p_tmp(j)+gam*beta(j)/(1d0+gam)*(p_out(4)+p_tmp(4))
c$$$      enddo
c$$$
c$$$      return
c$$$      end
      

c$$$c test resten      
c$$$      implicit none
c$$$      real * 8 resten,p1(4),p2(4)
c$$$ 1    continue
c$$$      p1=0
c$$$      p2=0
c$$$      write(*,*) ' enter p1 x, z component'
c$$$      read(*,*) p1(1),p1(3)
c$$$      write(*,*) ' enter p2 z componen'
c$$$      read(*,*) p2(3)
c$$$      p2(1)=-p1(1)
c$$$      p1(4)=sqrt(p1(1)**2+p1(2)**2+p1(3)**2)
c$$$      p2(4)=sqrt(p2(1)**2+p2(2)**2+p2(3)**2)
c$$$      write(*,*) sqrt((p1(4)+p2(4))**2-(p1(1)+p2(1))**2
c$$$     1     -(p1(2)+p2(2))**2-(p1(3)+p2(3))**2)
c$$$      p2=2.345*p2
c$$$      write(*,*) resten(p1,p2)
c$$$      goto 1
c$$$      end
      
      function resten(p1,p2)
      implicit none
      real * 8 resten,p1(4),p2(4),beta,gamma,c1,c2,pt1,pt2
c     take two momenta p1,p2; assuming they are massless,
c     assuming that they are back-to-back in the transverse plane
c     compute the longitudinal boost beta that makes them back to back
      pt1=sqrt(p1(1)**2+p1(2)**2)
      pt2=sqrt(p2(1)**2+p2(2)**2)
      c1=p1(3)/pt1
      c2=p2(3)/pt2
      beta=-(c1+c2)/(sqrt(c1**2+1)+sqrt(c2**2+1))
      gamma=1/sqrt(1-beta**2)
      resten=2*gamma*(p1(4)+beta*p1(3))
      end
      



      subroutine missingpt(ptmiss,ptm)
      implicit none
      double precision ptmiss
      double precision ptmissv(2),pp(4)
      real(kind=8),dimension(4) :: ptm
      logical dosmear
      common/cdosmear/dosmear
      logical ptmissh
      common/cptmissh/ptmissh
      include 'hepevt.h'
      double precision yp,etap,ptp,mass
      integer ihep
      ptmissv = 0
      do ihep=1,nhep
         if(isthep(ihep).eq.1) then
            call getyetaptmass(phep(1:4,ihep),yp,etap,ptp,mass)
c     if (ptmissh) the ptmiss is computed summing all pts of
c     visibile particle within the rapidity acceptance 
            if( ptmissh .and. abs(etap)>2.5 ) cycle
            
            select case(abs(idhep(ihep)))
            case(12,14,16)
               if(ptmissh) then
                  cycle
               else
                  ptmissv = ptmissv + phep(1:2,ihep)
               endif
            case default
               if(ptmissh) then
                  pp=phep(1:4,ihep)
                  if(dosmear) then
                     call smearmom(pp,idhep(ihep))
                  endif
                  ptmissv = ptmissv - pp(1:2)
               endif
            end select
         endif
      enddo
      ptmiss=sqrt(ptmissv(1)**2+ptmissv(2)**2)
      ptm=0
      ptm(1:2)=ptmissv(:)
      end
      
      subroutine getdr(eta1,eta2,phi1,phi2,dr)
      implicit none
      real(kind=8),intent(in)  :: eta1,eta2,phi1,phi2
      real(kind=8),intent(out) :: dr
      real * 8 dphi
      real * 8, parameter :: pi=3.141592653589793d0
      dphi = abs(phi1 - phi2)
      if(dphi > pi) dphi=2*pi - dphi
      dr = sqrt((eta1-eta2)**2 + dphi**2)
      end subroutine getdr

      subroutine getdphi(p1,p2,dphi,phi1,phi2)
      implicit none
      include 'pwhg_math.h' 
      real(kind=8),intent(in)   :: p1(4),p2(4)
      real(kind=8),intent(out)  :: dphi
      real(kind=8),intent(out),optional :: phi1,phi2
      real(kind=8) :: phiA, phiB,pt1,pt2

      pt1=sqrt(p1(1)**2+p1(2)**2)
      pt2=sqrt(p2(1)**2+p2(2)**2)
      
      if (p1(2) .ge. 0) then
              phiA = dacos(p1(1)/pt1)
      else
              phiA = 2*pi - dacos(p1(1)/pt1)
      end if 

      if (p2(2) .ge. 0) then
              phiB = dacos(p2(1)/pt2)
      else
              phiB = 2*pi - dacos(p2(1)/pt2)
      end if 

      dphi = abs(phiA - phiB)

      if(dphi > pi) dphi=2*pi - dphi

      if (present(phi1) .and. present(phi2)) then
         phi1 = phiA 
         phi2 = phiB
      end if 
      end subroutine getdphi
