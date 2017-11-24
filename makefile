COMP_SER = gfortran
LINK_SER = $(COMP_SER)
OPT_SER  = -O3 -ffree-form -ffree-line-length-none

OPT_format := -ffree-form -ffree-line-length-none
OPT_normal := $(OPT_format) -O3
# quick compilation:
OPT_quick  := $(OPT_format) -O0
OPT_test   := $(OPT_normal)
# provide useful info at exceptions (use for debug):
OPT_debug  := $(OPT_format) -fimplicit-none -fbacktrace -g -fcheck=all -ffpe-trap=zero,overflow -finit-real=nan
# provides many warnings:
OPT_warn   := $(OPT_format) -O0 -Wall -Wno-character-truncation -Wno-conversion
# time the execution
OPT_gprof  := $(OPT_format) -O0 -pg

mode = normal
ifeq ($(mode),normal)
 OPT_SER = $(OPT_normal)
endif
ifeq ($(mode),test)
 OPT_SER = $(OPT_test)
endif
ifeq ($(mode),debug)
 OPT_SER = $(OPT_debug)
endif
ifeq ($(mode),quick)
 OPT_SER = $(OPT_quick)
endif
ifeq ($(mode),warn)
 OPT_SER = $(OPT_warn)
endif
ifeq ($(mode),gprof)
 OPT_SER = $(OPT_gprof)
endif
LLIB =

ifeq ($(CBIND), TRUE)                      #F03 c-bindings
  DEFINES_BASE += -DF03_CBIND
  ifeq ($(mode),test)
    DEFINES_BASE += -D_TEST_
  endif
  ifeq ($(mode),normal)
    DEFINES_BASE += -D_NORMAL_
  endif
  ifeq ($(mode),debug)
    DEFINES_BASE += -D_DEBUG_
  endif
  ifeq ($(mode),warn)
    DEFINES_BASE += -D_WARN_
  endif
  ifeq ($(mode),quick)
    DEFINES_BASE += -D_QUICK_
  endif
  ifeq ($(mode),gprof)
    DEFINES_BASE += -D_GPROF_
  endif
endif


DEFINES_SER  = $(DEFINES_BASE)

#-----------------------------------------------------------------
#                          Compiler flags
#-----------------------------------------------------------------

FLAG_SER  = $(DEFINES_SER) $(OPT_SER)
FLAG_PAR  = $(DEFINES_PAR) $(OPT_PAR)

#-----------------------------------------------------------------
LLIB += -L$(FFTWLIB) -lfftw3 -lm

#-----------------------------------------------------------------

F90 = $(wildcard *.f90)
OBJ = $(patsubst %.f90,%.o,$(F90))
all: llimag.exe

%.exe: $(OBJ)
	$(LINK_SER) $(FLAG_SER) -o $@ $(OBJ) $(LLIB)

%.o: %.f90
	$(COMP_SER) $(FLAG_SER) -c $<

particlemodule.o: systemmodule.o
llimag.o: particlemodule.o systemmodule.o

#*****************************************************************
#*                         Tidy                                  *
#*****************************************************************

tidy:
	rm -f *.o *.mod *.vtf

#*****************************************************************
#*                         Clean                                 *
#*****************************************************************

clean:
	rm -f *.o *.mod *.i90 $(BINDIR)/*.exe *__genmod*
