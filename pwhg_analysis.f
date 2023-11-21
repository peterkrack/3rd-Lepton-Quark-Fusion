      subroutine setreal(p,rflav,amp2real)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_math.h'
      include 'pwhg_st.h'
      include 'pwhg_em.h'
      real * 8 msq
      real * 8 p(0:3,nlegreal)
      real * 8 pg(0:3),pe(0:3),ke(0:3)
      integer rflav(nlegreal),mcfmflav(nlegreal)
      real * 8 amp2real
      real * 8 dotp
      integer j
      real * 8 pgke, peke, s
c     Replace with your cross section depending upon the incoming flavour rflav
c     Below is an example for the real corrections to e+ e- -> H      
      do j=1,2
         if(rflav(j) == 22) then
            pg = p(:,j)
         else
            pe = p(:,j)
         endif
      enddo
      do j=3,4
         if(rflav(j) /= 25) then
            ke = p(:,j)
            exit
         endif
      enddo

      pgke = dotp(pg,ke)
      peke = dotp(pe,ke)
      s = 2*dotp(pg,pe)

      amp2real =
     1 -((s**2-4*pgke*s-4*peke*s+4*pgke**2+8*peke*pgke+8*peke**2)/(pgke*s
     2   ))/2


      amp2real = - amp2real * 2*pi/st_alpha * 4 * pi * em_alpha

      end

      subroutine regularcolour_lh
      write(*,*) ' regularcolour_lh: there are no regulars in this process'
      write(*,*) ' exiting ...'
      call exit(-1)
      end

