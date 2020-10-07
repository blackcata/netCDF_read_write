F90=ifort
FCFLAGS=-O2 -I${NETCDF}/include -mcmodel=large -shared-intel
LDFLAGS=-L${NETCDF}/lib -lnetcdff -lnetcdf

TARGET= EXE_netCDF
OBJECT= mod_netCDF_IO.o  example_netCDF.o 

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.o
	rm -f *.mod
	rm EXE_netCDF
