        !----------------------------------------------------------------------!
        !                       Allocate data to arrays                        !
        !----------------------------------------------------------------------!
        SUBROUTINE File_read(dir_name,line_num,var_name,time,var)
            IMPLICIT NONE

            INTEGER :: it, t, year_num, IO
            INTEGER,INTENT(IN) :: line_num

            REAL(KIND=8) :: year_tmp(1:12), var_tmp(1:12)
            REAL(KIND=8),INTENT(INOUT) :: time(1:line_num*12), var(1:line_num*12)
            CHARACTER(LEN=128) :: file_name, path_name, header
            CHARACTER(LEN=128) :: dir_name,  var_name

            file_name = TRIM(var_name)//'.dat'
            path_name = TRIM(dir_name)//'/'//TRIM(file_name)

            OPEN(100,FILE=path_name,FORM='FORMATTED',STATUS='OLD')
            DO it = 1,7
              READ(100,*) header
            END DO

            Do it = 1,line_num
              READ(100,*) year_num, var_tmp(1:12)
              DO t = 1,12
               year_tmp(t) = year_num + (t-1)*(1.0d0/12.0d0)
              END DO

              time(12*(it-1)+1:12*(it-1)+12) = year_tmp(1:12)
              var(12*(it-1)+1:12*(it-1)+12)  = var_tmp(1:12)

            END DO

        END SUBROUTINE File_read

        !----------------------------------------------------------------------!
        !                      Read line number of data                        !
        !----------------------------------------------------------------------!
        SUBROUTINE Read_Line_num(dir_name,line_num,var_name)
            IMPLICIT NONE

            INTEGER :: it, IO, year_num
            INTEGER,INTENT(OUT) :: line_num

            REAL(KIND=8) :: year_tmp(1:12), var_tmp(1:12)

            CHARACTER(LEN=128)  ::  path_name, file_name, header
            CHARACTER(LEN=128),INTENT(IN) :: dir_name, var_name

            file_name = TRIM(var_name)//'.dat'
            path_name = TRIM(dir_name)//'/'//TRIM(file_name)
            OPEN(100,FILE=path_name,FORM='FORMATTED',STATUS='OLD')

            DO it = 1,7
              READ(100,*) header
              print*,header
            END DO

            line_num = 0
            DO it = 1,1000
              READ(100,*,IOSTAT=IO) year_num, var_tmp(1:12)
              IF (IO < 0) exit

              line_num = line_num + 1
             ! WRITE(*,"(I,2X,13(F10.5,2X))")year_num,var_tmp(1:12)
            END DO

            CLOSE(100)

        END SUBROUTINE Read_Line_num
