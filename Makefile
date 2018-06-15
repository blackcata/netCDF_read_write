F90=ifort
FCFLAGS=-O3 -I${NETCDF}/include
LDFLAGS=-L${NETCDF}/lib -lnetcdff

TARGET= MAIN
OBJECT= netCDF_main.o netCDF_write.o netCDF_read.o

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.o
	rm MAIN
