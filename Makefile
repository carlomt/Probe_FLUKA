# ###########################################
#  Generic Makefile for FLUKA applications
#
#  Compiles all *.f files in the current
#  directory and links against FLUKA.
# 
#  Change LFLUKA if you want to link against
#  DPMJET-III (by default it is not linked).
#
#  The executable is called x_fluka.
#
#  Use "make clean" to clean up files.
#
# ###########################################
#
#
############## DEFINITIONS ##################
#
FFF=$(FLUKA)/flutil/fff
LFLUKA=$(FLUKA)/flutil/lfluka
# LFLUKA=$(FLUKA)/flutil/ldpm3qmd
#
#
SRCFILES := $(wildcard ./*.f) 
OBJECTS := $(patsubst %.f, %.o, $(SRCFILES))
#
#
############# RULES ########################
#
.f.o:
	$(FFF) $<
#
#
############# TARGETS ######################
#
all: x_fluka
#
#
x_fluka: $(OBJECTS)
	echo $(OBJECTS)
	$(LFLUKA) -m fluka -o $@ $^
#
#
clean:
	rm -f x_fluka *.o *.map *.FOR *.so *.x
#

CC      = g++ -Wall  -fPIC
ROOT    = `root-config --glibs --cflags`


converter3.x: converter3.C
	$(CC) $(ROOT) -o converter3.x converter3.C
