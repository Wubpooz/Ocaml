#Usages
#make : compiles all .ml
#make clean : remove ALL binaries and .cmo

.PHONY : all clean

#Compiler
CP = ocamlc
CFLAGS =

#Linker
LD = ocamlc
LDFLAGS =

#Cleaner (can specify a dir in which bin would be created)
RM = rm -rvf

#Executable files retriving
SRCS := $(wildcard *.ml)
BINS := $(SRCS:%.ml=%)

all: clean $(BINS) 

%: %.cmo
	$(LD) $(LDFLAGS) -o $@ $<

%.cmo: %.ml
	@echo "Creating object files..."
	$(CP) $(CFLAGS) -c $<

run : $(BINS)
	./$<

dm : all run

clean:
	@echo "Cleaning up..."
	$(RM) *.cmo *.cmi $(BINS)