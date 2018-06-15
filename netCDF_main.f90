        PROGRAM netCDF_main

            USE netcdf

            IMPLICIT NONE

            INTEGER :: line_num

            REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: time, var
            CHARACTER(LEN=128) :: file_name, dir_name, var_name

            !-------------------------------------------------------------------!
            !                         READING PROCESS                           !
            !-------------------------------------------------------------------!
            dir_name  = 'DATA'
            var_name  = 'PDO'

            CALL Read_Line_num(dir_name,line_num,var_name)

            ALLOCATE( time(1:line_num*12), var(1:line_num*12) )

            CALL File_read(dir_name,line_num,var_name,time,var)

            !-------------------------------------------------------------------!
            !                 TRANSFORM DATA TO NETCDF FORMAT                   !
            !-------------------------------------------------------------------!
            dir_name = 'RESULT'
            var_name = 'PDO'
            CALL Write_netCDF_1D(dir_name,line_num,var_name,time,var)

            DEALLOCATE( time, var )

            !-------------------------------------------------------------------!
            !                         READING PROCESS                           !
            !-------------------------------------------------------------------!
            dir_name  = 'DATA'
            var_name  = 'CTI'

            CALL Read_Line_num(dir_name,line_num,var_name)

            ALLOCATE( time(1:line_num*12), var(1:line_num*12) )

            CALL File_read(dir_name,line_num,var_name,time,var)

            !-------------------------------------------------------------------!
            !                 TRANSFORM DATA TO NETCDF FORMAT                   !
            !-------------------------------------------------------------------!
            dir_name = 'RESULT'
            var_name = 'CTI'
            CALL Write_netCDF_1D(dir_name,line_num,var_name,time,var)

            DEALLOCATE( time, var )

            !-------------------------------------------------------------------!
            !                         READING PROCESS                           !
            !-------------------------------------------------------------------!
            dir_name  = 'DATA'
            var_name  = 'Indian_ocean_SAT_anomalies'

            CALL Read_Line_num(dir_name,line_num,var_name)

            ALLOCATE( time(1:line_num*12), var(1:line_num*12) )

            CALL File_read(dir_name,line_num,var_name,time,var)

            !-------------------------------------------------------------------!
            !                 TRANSFORM DATA TO NETCDF FORMAT                   !
            !-------------------------------------------------------------------!
            dir_name = 'RESULT'
            var_name = 'Indian_ocean_SAT_anomalies'
            CALL Write_netCDF_1D(dir_name,line_num,var_name,time,var)

            DEALLOCATE( time, var )


        END PROGRAM netCDF_main
