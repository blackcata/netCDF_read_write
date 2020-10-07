!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : example_netCDF.f90                                               !
!                                                                              !
!   PURPOSE : Read the variables from netCDF file                              !
!                                                                              !
!                                                             2020.10.06.K.Noh !
!                                                                              !
!   INSTRUCTION :                                                              !
!      1. Define the number of each dimension (time,lev,lat,lon)               !
!      2. Define the path of directory and the name of file                    !
!      3. Check the name of dimension in netCDF file                           !
!      4. Define the starting point of each dimension(ind_time,lev,lat,lon_str)!
!      5. Check the name of variable in netCDF file through ncdump (var_name)  !
!      6. Match the var_name to same variable name                             !
!      7. Compile the codes by using Makefile (Just make commands)             !
!      8. Run the EXE_netCDF excutable file                                    !
!                                                                              !
!------------------------------------------------------------------------------!
PROGRAM netCDF_example

     USE mod_netCDF_IO 

     IMPLICIT NONE
     INTEGER :: ind_time_str, ind_lev_str, ind_lat_str, ind_lon_str

     REAL(KIND=8),ALLOCATABLE,DIMENSION(:)        ::  time, lev, lat, lon
     REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:)    ::  var_3d
     REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:,:,:)  ::  var_4d

     !-------------------------------------------------------------------------!
     !                  BASIC SETTING BEFORE READING netCDF FILE               !
     !-------------------------------------------------------------------------!
     !< Allocate the number of each dimension 
     N1 = 360  !< Longitude
     N2 = 180  !< Latitude
     N3 = 60   !< Depth & Level
     N4 = 1200 !< Time 

     !<Define the path of directory and name of file
     dir_name  = '/data4/jhoh/SRC/RAMP_CO2/POP/TEMP/regrid/'
     file_name = 'regrid_180x360_b.e12.B2000C5CN.f09_g16.ramp.track1.028.pop.h.TEMP.230001-239912.nc'

     path_name = TRIM(dir_name)//TRIM(file_name)

     !-------------------------------------------------------------------------!
     !                     READ THE DIMENSION OF netCDF FILE                   !
     !-------------------------------------------------------------------------!
     !<Read the time dimension
     N  =  N4   ;  ind_time_str  = 1 !<Starting point of time dimension
     var_name   =  "time"
     ALLOCATE(  time(1:N) )
     CALL netCDF_read_1d(time, ind_time_str)

     !<Read the level dimension
     N  =  N3   ; ind_lev_str  = 1 !<Starting point of level dimension
     var_name   =  "z_t"
     ALLOCATE(  lev(1:N) )
     CALL netCDF_read_1d(lev, ind_lev_str)

     !<Read the latitude dimension
     N  =  N2   ; ind_lat_str  = 1 !<Starting point of latitude dimension
     var_name   =  "lat"
     ALLOCATE(  lat(1:N) )
     CALL netCDF_read_1d(lat, ind_lat_str)

     !<Read the longitude dimension
     N  =  N1   ; ind_lon_str  = 1 !<Starting point of longitude dimension
     var_name   =  "lon"
     ALLOCATE(  lon(1:N) )
     CALL netCDF_read_1d(lon, ind_lon_str)

     !-------------------------------------------------------------------------!
     !                      READ THE VARIABLE OF netCDF FILE                   !
     !-------------------------------------------------------------------------!
     !<Read the variable from netCDF file 
     var_name = "TEMP" 
     ALLOCATE( var_4d(1:N1, 1:N2, 1:N3, 1:N4) )
     CALL netCDF_read_4d(temp, ind_lon_str, ind_lat_str,                        &
                               ind_lev_str, ind_time_str)

     DEALLOCATE( var_4d )

END PROGRAM netCDF_example
