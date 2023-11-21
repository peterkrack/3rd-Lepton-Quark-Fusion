#-*- Makefile -*-
## Choose compiler: gfortran,ifort
COMPILER=gfortran
## Choose PDF: native,lhapdf
## LHAPDF package has to be installed separately
PDF=lhapdf
## default analysis may require FASTJET package, that has to be installed separately (see below)
ANALYSIS=LQ-tautag
## For static linking uncomment the following
#STATIC= -static
#
ifeq ("$(COMPILER)","gfortran")	
F77= gfortran -fno-automatic -ffixed-line-length-none -fallow-argument-mismatch -Wall
## -fbounds-check sometimes causes a weird error due to non-lazy evaluation
## of boolean in gfortran.
#FFLAGS= -Wall -Wimplicit-interface -fbounds-check
## For floating point exception trapping  uncomment the following 
#FPE=-ffpe-trap=invalid,zero,overflow,underflow 
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT=-O2
## For debugging uncomment the following
#DEBUG= -ggdb -pg
endif

ifeq ("$(COMPILER)","ifort")
F77 = ifort -save  -extend_source
CXX = icpc
LIBS = -limf
FFLAGS =  -check
## For floating point exception trapping  uncomment the following 
#FPE = -fpe0
OPT = -O3 #-fast
## For debugging uncomment the following
#DEBUG= -debug -g
endif

OBJ=obj-$(COMPILER)
$(shell [ -d $(OBJ) ] || mkdir -p $(OBJ))

PWD=$(shell pwd)

ifeq ("$(shell whoami)","nason")
RES=/home/nason/Pheno/POWHEG-BOX-RES
else ifeq ("$(shell whoami)","zanderighi")
RES=/Users/zanderighi/tpimac/POWHEG/POWHEG-BOX-RES
else ifeq ("$(shell whoami)","lucabuonocore")
RES=/Users/lucabuonocore/codes/POWHEG-BOX-RES
else ifeq ("$(shell whoami)","lbuono")
RES=/disk/data11/ttp/lbuono/codes/POWHEG-BOX-RES
else ifeq ("$(shell whoami)","krack")
RES=/Users/krack/POWHEG-BOX-RES
CC=gcc-13
CXX=g++-13
else
RES=../
endif

VPATH= ./:./POWHEGpatch/:$(RES)/Beta-Progress/:$(RES):$(OBJ)/

INCLUDE0=$(PWD)
INCLUDE1=$(PWD)/inlcude
INCLUDE2=$(PWD)/POWHEGpatch
INCLUDE3=$(RES)/include
FF=$(F77) $(FFLAGS) $(FPE) $(OPT) $(DEBUG) -I$(INCLUDE0) -I$(INCLUDE1) -I$(INCLUDE2) -I$(INCLUDE3)

INCLUDE =$(wildcard $(RES)/include/*.h *.h include/*.h)

ifeq ("$(PDF)","lhapdf")
LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdf6if.o lhapdf6ifcc.o
FJCXXFLAGS+= $(shell $(LHAPDF_CONFIG) --cxxflags)
LIBSLHAPDF= -Wl,-rpath,$(shell $(LHAPDF_CONFIG) --libdir)  -L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF
ifeq  ("$(STATIC)","-static") 
## If LHAPDF has been compiled with gfortran and you want to link it statically, you have to include
## libgfortran as well. The same holds for libstdc++. 
## One possible solution is to use fastjet, since $(shell $(FASTJET_CONFIG) --libs --plugins ) -lstdc++
## does perform this inclusion. The path has to be set by the user. 
 LIBGFORTRANPATH=/usr/lib/gcc/x86_64-redhat-linux/4.1.2
 LIBSTDCPP=/lib64
# ifeq ("$(shell whoami)","zanderighi")
 LIBSLHAPDF+=  -L$(LIBGFORTRANPATH)  -lgfortranbegin -lgfortran -L$(LIBSTDCPP) -lc++
# else 
# LIBSLHAPDF+=  -L$(LIBGFORTRANPATH)  -lgfortranbegin -lgfortran -L$(LIBSTDCPP) -lstdc++
#endif
endif
LIBS+=$(LIBSLHAPDF)
else
PDFPACK=mlmpdfif.o hvqpdfpho.o
endif


ifeq ("$(shell whoami)","zanderighi")
LCPP=-lc++
else ifeq ("$(shell whoami)","lucabuonocore")
LCPP=-lc++
else ifeq ("$(shell whoami)","krack")
LCPP=-lstdc++
else
LCPP=-lstdc++
endif

ifeq ("$(ANALYSIS)","default")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(LCPP)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_analysis.o pwhg_bookhist-multi.o multi_plot.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)
PWHGANAL+= fastjetfortran.o
endif
ifeq ("$(ANALYSIS)","LQ")
##To include Fastjet configuration uncomment the following lines.
FASTJET_CONFIG=$(shell which fastjet-config)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(LCPP)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_analysis-LQ.o pwhg_bookhist-multi.o multi_plot.o smearmom.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)
PWHGANAL+= fastjetfortran.o
endif
ifeq ("$(ANALYSIS)","LQ-tautag")
##To include Fastjet configuration uncomment the following lines.
FASTJET_CONFIG=$(shell which fastjet-config)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(LCPP)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_analysis-LQ-tautag.o pwhg_bookhist-multi.o multi_plot.o smearmom.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)
PWHGANAL+= fastjetfortran.o
endif

%.o: %.f $(INCLUDE)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.f90 $(INCLUDE)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o $(OBJ)/$@ $^ 

%.o: %.cc
	$(CXX) $(DEBUG) -c -o $(OBJ)/$@ $^ $(FJCXXFLAGS)

LIBS+=-lz

USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o  \
     real.o alphaqed.o qed_coupling.o qcd.o warnings_and_errors.o assertions.o $(PWHGANAL)

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o     \
   LesHouches.o LesHouchesreg.o gen_Born_phsp.o find_regions.o \
   test_Sudakov.o pt2maxreg.o sigborn.o gen_real_phsp.o maxrat.o  \
   gen_index.o gen_radiation.o Bornzerodamp.o sigremnants.o \
   random.o boostrot.o bra_ket_subroutines.o cernroutines.o \
   init_phys.o powheginput.o pdfcalls.o sigreal.o sigcollremn.o   \
   pwhg_analysis_driver.o checkmomzero.o     \
   setstrongcoupl.o integrator.o newunit.o mwarn.o sigsoftvirt.o  \
   sigcollsoft.o sigvirtual.o reshufflemoms.o  setlocalscales.o    \
        validflav.o mint_upb.o opencount.o fullrwgt.o          \
   pwhg_io_interface.o rwl_weightlists.o sigregular.o  \
        rwl_setup_param_weights.o rwl_setup_param_weights_user.o lhefread.o \
   cache_similar.o sigequiv_hook.o utils.o  $(PDFPACK) $(USER) $(FPEOBJ) PhaseSpaceUtils.o             \
        ubprojections-new.o  mintwrapper.o fill_res_histories.o  genericphsp.o boostrot4.o \
        resweights.o locks.o build_resonance_hists.o resize_arrays.o

# target to generate LHEF output
pwhg_main:$(PWHG)
	$(FF) $(patsubst %,$(OBJ)/%,$(PWHG)) $(LIBS) $(LIBSFASTJET) $(STATIC) $(LCPP) -o $@


LHEF=lhef_analysis.o boostrot.o random.o cernroutines.o	locks.o	\
     opencount.o powheginput.o $(PWHGANAL) \
     lhefread.o pwhg_io_interface.o rwl_weightlists.o newunit.o pwhg_analysis_driver.o $(FPEOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEF)
	$(FF) $(patsubst %,$(OBJ)/%,$(LHEF)) $(LIBS) $(LIBSFASTJET) $(STATIC)  $(LCPP) -o $@ 

alphaqed.o : types.o qed_coupling.o
qed_coupling.o : warnings_and_errors.o qcd.o assertions.o

# target to cleanup
.PHONY: clean
clean:
	rm -f $(OBJ)/*.o pwhg_main lhef_analysis
