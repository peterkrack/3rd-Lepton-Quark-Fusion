      subroutine init_processes
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_flg.h'
      include 'pwhg_kn.h'
      include 'pwhg_st.h'
      include 'pwhg_par.h'
      include 'pwhg_pdf.h'
      include 'pwhg_rad.h'
      include 'LesHouches.h'
      include 'pwhg_physpar.h'
      include 'PhysPars.h'
      character * 30 proc
      common/cproc/proc
      logical debug
      parameter (debug=.false.)
      integer i,j,k,l,q
      real * 8 mLQ,wLQ,betaL,betaR
      real * 8 powheginput
      integer lflav
c     Set the colour representation of the leptoquark
      call set_colour(42,'3','set')
c     smartsig in POWHEG-BOX-RES does not check for equal
c     Born but not for equal colour correlated or spin correlated
c     amplitudes; switch it off
      flg_bornsmartsig = .false.
      
c     for testing only NLO QCD
      if (powheginput("#QCDonly") == 1) then 
         flg_with_em = .false.
      else
         flg_with_em = .true.
      endif
      
c     for testing only NLO QED, uncomment
      if (powheginput("#QEDonly") == 1) then 
         flg_QEDonly = .true.
      else
         flg_QEDonly = .false.
      endif

      flg_withdamp = .true.
      flg_bornzerodamp = .false.
      rad_ptsqmin_em=powheginput("#ptsqmin_em")
      if(rad_ptsqmin_em<0) rad_ptsqmin_em=0.5d0
            
      pdf_nparton = 22
      par_isrtinycsi = 1d-8
      par_isrtinyy = 1d-8
      par_fsrtinycsi = 1d-8
      par_fsrtinyy = 1d-8
c flag to do importance sampling in x variable in collinear remnants
      flg_collremnsamp=.true.      

c This ends up in the LH common block, not very useful ..
      lprup(1)=2000

c     Born flavour lists:
      flst_nborn=1
      if (ph_LQc .eq. 2 ) then
         do i=1,3
            do j=1,3
               if (ph_betaL(i,j).ne.0.or.ph_betaR(i,j).ne.0) then
                  if (j .eq. 1) l = 11
                  if (j .eq. 2) l = 13
                  if (j .eq. 3) l = 15
                  if (i .eq. 1) q = 1
                  if (i .eq. 2) q = 3
                  if (i .eq. 3) q = 5
                  if (ph_LQc .eq. 2) then
                     flst_born(:,flst_nborn) = [-l,q,42]
                     flst_nborn = flst_nborn+1
                     flst_born(:,flst_nborn) = [l,-q,42]
                     flst_nborn = flst_nborn+1
                     flst_born(:,flst_nborn) = [q,-l,42]
                     flst_nborn = flst_nborn+1
                     flst_born(:,flst_nborn) = [-q,l,42]
                     flst_nborn = flst_nborn+1
                  else
                     continue
                  endif
               else
                  continue
               endif
            enddo
         enddo
      else
         WRITE(*,*) "Charge of the LQ should be set to 2/3."
      endif
      flst_nborn = flst_nborn - 1
c as starting example, e u -> LQ      

c     Real flavour lists:
      flst_nreal=1
c     Strong corrections:
      
      if (ph_LQc .eq. 2) then
         do i=1,3
            do j=1,3
               if (ph_betaL(i,j).ne.0.or.ph_betaR(i,j).ne.0) then
                  if (j .eq. 1) l = 11
                  if (j .eq. 2) l = 13
                  if (j .eq. 3) l = 15
                  if (i .eq. 1) q = 1
                  if (i .eq. 2) q = 3
                  if (i .eq. 3) q = 5
                  if (ph_LQc .eq. 2) then
                     
                     if (.not. flg_QEDonly) then                      
                        flst_real(:,flst_nreal) = [-l,q,42,0]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [-l,0,42,-q]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [q,-l,42,0]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [0,-l,42,-q]
                        flst_nreal = flst_nreal + 1
                     endif
                     
                     if (flg_with_em) then  
                        flst_real(:,flst_nreal) = [22,q,42,l]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [q,22,42,l]
                        flst_nreal = flst_nreal + 1
                     endif

                     if (.not. flg_QEDonly) then 
                        flst_real(:,flst_nreal) = [l,-q,42,0]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [l,0,42,q]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [-q,l,42,0]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [0,l,42,q]
                        flst_nreal = flst_nreal + 1
                     endif
                     
                     if (flg_with_em) then  
                        flst_real(:,flst_nreal) = [22,-q,42,-l]
                        flst_nreal = flst_nreal + 1
                        flst_real(:,flst_nreal) = [-q,22,42,-l]
                        flst_nreal = flst_nreal + 1
                     endif
                  endif
               endif
            enddo
         enddo
      endif
      flst_nreal = flst_nreal -1
      
       WRITE(*,*) "Born Processes: "
       WRITE(*,"(*(g0))") ( (flst_born(j,i)," ",j=1,3),
     1  new_line("A"), i=1,flst_nborn)
       WRITE(*,*) "Real Processes: "
       WRITE(*,"(*(g0))") ( (flst_real(j,i)," ",j=1,4),
     1  new_line("A"), i=1,flst_nreal)

c     The following is POWHEG magic ...
      flst_bornlength=nlegborn
      flst_reallength=nlegreal
      flst_numfinal=1
      flst_bornres=0
      call buildresgroups(flst_nborn,nlegborn,flst_bornlength,
     1     flst_born,flst_bornres,flst_bornresgroup,flst_nbornresgroup)

      flst_realres=0
      call buildresgroups(flst_nreal,nlegreal,flst_reallength,
     1     flst_real,flst_realres,flst_realresgroup,flst_nrealresgroup)

      end

