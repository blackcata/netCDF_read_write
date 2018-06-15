F90=gfortran
FCFLAGS=-O2 -I${NCDIR}/include -L${NCDIR}/lib
LDFLAGS=-lnetcdff

TARGET= MAIN
OBJECT= netCDF_main.f90 netCDF_write.f90 netCDF_read.f90

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $< $(LDFLAGS)

clean :
	rm -f *.o
	rm MAIN
