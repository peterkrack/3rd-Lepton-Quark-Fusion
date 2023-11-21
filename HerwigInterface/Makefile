#-*- Makefile -*-

ifeq ("$(USER)","lbuono")
# set the following variables
######################################################
# location of powheg box v2 files
POWHEGDIR=/disk/data11/ttp/lbuono/codes/POWHEG-BOX-V2
# process directory (not necessary if dummy analysis is used)
PROCDIR=/disk/data11/ttp/lbuono/codes/POWHEG-BOX-RES/PWHG-leptoquarks
# analysis file: use dummy if a rivet analysis is used
pwhg_analysis=pwhg_analysis.o
HERWIGCONFIG=herwig-config
THEPEGCONFIG=thepeg-config
#####################################################
endif

ifeq ("$(USER)","krack")
# set the following variables
######################################################
# location of powheg box v2 files
POWHEGDIR=/Users/krack/POWHEG-BOX-RES
# process directory (not necessary if dummy analysis is used)
PROCDIR=../
# analysis file: use dummy if a rivet analysis is used
pwhg_analysis=pwhg_analysis-LQ-tautag.o
HERWIGCONFIG=herwig-config
THEPEGCONFIG=thepeg-config
#####################################################
endif

ifeq ("$(USER)","nason")
# set the following variables
######################################################
# location of powheg box v2 files
POWHEGDIR=/home/nason/Pheno/POWHEG-BOX-RES
# process directory (not necessary if dummy analysis is used)
PROCDIR=../LQ-Res-Prod
# analysis file: use dummy if a rivet analysis is used
pwhg_analysis=pwhg_analysis-LQ.o
HERWIGCONFIG=herwig-config
THEPEGCONFIG=thepeg-config
#####################################################
endif

COMPILER=gfortran
ff=$(COMPILER)
cc=gcc
cxx=g++

ifeq ("$(shell whoami)","krack")
cc=gcc-13
cxx=g++-13
endif

OBJ=./obj-$(COMPILER)-hw
OBJDIR:=$(OBJ)

HERWIGDIR=$(shell $(HERWIGCONFIG) --prefix)
LIBHERWIG=-L$(HERWIGDIR)/lib/Herwig -l:HwShower.so
ifeq ("$(shell whoami)","krack")
LIBHERWIG=-L$(HERWIGDIR)/lib/Herwig -lHerwigAPI
endif

## set the HepMC and ThePEG installation directories
HEPMCDIR=/home/ttp/lbuono/local/HepMC-2.06.11
ifeq ("$(shell whoami)","krack")
HEPMCDIR=/opt/local/
endif
LIBHEPMC=-L$(HEPMCDIR)/lib -lHepMC  

THEPEGDIR=$(shell $(THEPEGCONFIG) --prefix)
LIBTHEPEG=-L$(THEPEGDIR)/lib/ThePEG -lThePEG -l:LesHouches.so 
ifeq ("$(shell whoami)","krack")
LIBTHEPEG=-L$(THEPEGDIR)/lib/ThePEG -lThePEG -lLesHouches 
endif

libextra=-lstdc++
ifeq ("$(shell whoami)","krack")
libextra=-lstdc++ -lz -v
endif
librpath=-Wl,-rpath,$(HEPMCDIR)/lib:$(THEPEGDIR)/lib/ThePEG

ifeq ("$(ff)","gfortran")	
ff=gfortran -fno-automatic -ffixed-line-length-none -fbounds-check
endif

## For debugging uncomment the following
#DEBUG= -ggdb
ifdef DEBUG
OPTF=-O0  
OPTC=-O0  -Warray-bounds -Wno-div-by-zero -Waddress -Wall -W  -Wodr  -Wno-builtin-declaration-mismatch
else 
OPTF=-O2
OPTC=-O2
endif

INCLUDES=-I$(PWD) -I$(PROCDIR) -I$(HEPMCDIR)/include -I$(THEPEGDIR)/include -I$(HERWIGDIR)/include -I$(POWHEGDIR) -I$(POWHEGDIR)/include
FLAGSF=-fPIC $(OPTF) $(DEBUG) $(INCLUDES)
FLAGSC=-fPIC $(OPTC) $(DEBUG) $(INCLUDES) 
FF=$(ff) $(FLAGSF)
CC=$(cc)  $(FLAGSC)
CXX=$(cxx)  $(FLAGSC) -fpermissive
#-fpermissive
LIBS=$(LIBHEPMC) $(LIBTHEPEG) $(libextra) $(librpath) $(LIBHERWIG)

VPATH=$(PWD):$(PWD):$(POWHEGDIR):$(PROCDIR):$(OBJDIR)

FASTJET_CONFIG=$(shell which fastjet-config)
##WWarn: this fastjet MUST BE the one used to configure ThePeg!! 
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o multi_plot.o 
PWHGANAL+=pwhg_bookhist-multi-extra.o fastjetfortran.o

PWHGANAL+=smearmom.o

PWHGANAL+=$(pwhg_analysis)




%.o: %.f | $(OBJDIR)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.f90 | $(OBJDIR)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.c | $(OBJDIR) 
	$(CC) $(DEBUG) -c -o $(OBJ)/$@ $^ 

%.o: %.cc | $(OBJDIR)
	$(CXX) $(DEBUG) -c -o $(OBJ)/$@ $^ $(FJCXXFLAGS) -std=gnu++11

powhegAnalysis=powhegAnalysis.o herwig7_interface.o \
        bra_ket_subroutines.o\
	powheginput.o newunit.o utils.o \
        pwhg_io_interface.o $(PWHGANAL) $(FPEOBJ)

powhegReader=powhegLesHouchesFileReader.o opencount.o 

powhegHerwig=$(powhegAnalysis) $(powhegReader) 


powhegHerwig.so:$(powhegHerwig)
	$(FF) -shared $(patsubst %,$(OBJ)/%,$(powhegHerwig)) $(LIBSFASTJET) $(LIBS) -o $@ -std=gnu++11

clean:
	-rm -f $(OBJ)/*.o $(OBJ)/*.so *.so *.o

# target to generate object directory if it does not exist
$(OBJDIR):
	mkdir -p $(OBJDIR)

##########################################################################
