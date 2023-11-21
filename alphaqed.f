      function alphaqed(q2) result(res)
      use qed_coupling_module
      implicit none
      type(qed_coupling), save :: alpha_qed
      real * 8 res,q2
      real * 8 q
      logical, save :: ini=.true.
      double precision, parameter ::
     1     mcharm=1.5d0,mbottom=4.5d0,mtop=172.5d0
      integer, save:: count=0,maxcount=50
      if(ini) then
         call InitQEDCoupling(alpha_qed, mcharm, mbottom, mtop)
         ini = .false.
      endif
      q=sqrt(q2)
      res = value(alpha_qed,q)
      if(count < maxcount) then
         write(*,*) 'alphaqed: q,137*res=',q,137*res
         count = count+1
      endif
      end
      
