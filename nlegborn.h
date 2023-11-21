c -*- Fortran -*-

c The user must set nlegborn to the appropriate value for his process.
      integer nlegborn,nlegreal

      parameter (nlegborn=3)
      parameter (nlegreal=nlegborn+1)

c     ndiminteg is the dimensionality of the full real integral
c     ndiminteg=(nlegreal-2)*3-4+2-1
c     if there are undecayed resonances, we need extra variables to pilot
c     the resonance's masses

      integer ndiminteg
      parameter (ndiminteg=(nlegreal-2)*3-4+4)


      integer maxprocborn,maxprocreal
      parameter (maxprocborn=100,maxprocreal=100)

      integer maxalr
      parameter (maxalr=1000)

      integer maxreshists
      parameter (maxreshists=500)
      integer nlegbornexternal
      parameter (nlegbornexternal=nlegborn)
