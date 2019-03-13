PROG =	ddtest

SRCS =	data_dd.f90 ks_array.f90 ks_data_module.f90 main.f90

OBJS =	data_dd.o ks_array.o ks_data_module.o main.o

LIBS =	

CC = cc
CFLAGS = -O
FC = f77
FFLAGS = -O
F90 = gfortran
F90FLAGS = -O -g -std=f2008 -Wall -Wextra -fcheck=all -finit-real=snan
LDFLAGS = 

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

%.o : %.mod

ks_array.o: ks_data_module.o
ks_data_module.o: data_dd.o
main.o: ks_array.o
