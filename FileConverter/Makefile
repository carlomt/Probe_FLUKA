CC      = g++ -Wall  -fPIC
ROOT    = `root-config --glibs --cflags`

#ARCH          = linux
CXX           =
ObjSuf        = o
SrcSuf        = cpp
ExeSuf        =
DllSuf        = so
OutPutOpt     = -o

ROOTCFLAGS   := $(shell root-config --cflags)
ROOTLIBS     := $(shell root-config --libs)
ROOTGLIBS    := $(shell root-config --glibs)
ROOTLDFLAGS  := $(shell root-config --ldflags)

# Linux with egcs, gcc 2.9x, gcc 3.x (>= RedHat 5.2)
CXX           = g++ -m32 
CXXFLAGS      = -O -Wall  -fPIC
LD            = g++ -m32 
LDFLAGS       = -O -Wall -fPIC $(ROOTLDFLAGS)
SOFLAGS       = -shared

CXXFLAGS     += $(ROOTCFLAGS)
LIBS          = $(ROOTLIBS) $(SYSLIBS)
GLIBS         = $(ROOTGLIBS) $(SYSLIBS)

default: 
		$(CXX) $(CXXFLAGS) $(LIBS) $(GLIBS) -D__OFNAME__='"FileConverter.x"' -o FileConverter.x FileConverter.C


clean:		rm -f *.x