        !----------------------------------------------------------------------!
        !                     Writing 3D data subroutine                       !
        !----------------------------------------------------------------------!
        SUBROUTINE Write_netCDF_1D(dir_name,line_num,var_name,time,var)
            USE netcdf

            IMPLICIT NONE

            INTEGER :: status, ncid, varid, time_dim, var_dim, line_num
            INTEGER :: time_id, var_id

            REAL(KIND=8) :: missing
            REAL(KIND=8),INTENT(IN):: time(1:line_num*12), var(1:line_num*12)
            CHARACTER(LEN=128) :: dir_name, path_name, file_name, var_name

            file_name = TRIM(var_name)//'.nc'
            path_name = TRIM(dir_name)//'/'//TRIM(file_name)

            !------------------------------------------------------------------!
            !                           Open NC file                           !
            !------------------------------------------------------------------!
            status = NF90_CREATE(path_name,NF90_SHARE,ncid)

            !------------------------------------------------------------------!
            !                  Define dimensions and variables                 !
            !------------------------------------------------------------------!
            status = NF90_DEF_DIM(ncid,'time',line_num*12,time_dim)
            status = NF90_DEF_DIM(ncid,var_name ,line_num*12,var_dim)

            status = NF90_DEF_VAR(ncid,'time',NF90_DOUBLE,time_dim,time_id)
            status = NF90_DEF_VAR(ncid,var_name ,NF90_DOUBLE,time_dim,var_id)

            status = NF90_PUT_ATT(ncid,varid,'_FillValue',missing)

            status = NF90_ENDDEF(ncid)

            !------------------------------------------------------------------!
            !                             Put variables                        !
            !------------------------------------------------------------------!
            status = NF90_PUT_VAR(ncid,time_id,time)
            status = NF90_PUT_VAR(ncid,var_id,var)

            !------------------------------------------------------------------!
            !                             Close NC file                        !
            !------------------------------------------------------------------!
            status = NF90_CLOSE(ncid)

        END SUBROUTINE Write_netCDF_1D

        !----------------------------------------------------------------------!
        !                     Writing 3D data subroutine                       !
        !----------------------------------------------------------------------!
        SUBROUTINE Write_netCDF_3D(oncfname,varname,x,y,z,time,var,missing,nx,ny,nz,nt)

            USE netcdf

            IMPLICIT NONE
            CHARACTER(LEN=128) :: oncfname, varname
            INTEGER            :: ncid, xid, yid, zid, timeid, varid
            INTEGER            :: xdim, ydim, zdim, timedim
            INTEGER            :: nx, ny, nz, nt
            INTEGER            :: status
            REAL(KIND=4)       :: missing
            REAL(KIND=8)       :: x(nx), y(ny), z(nz), time(nt)
            REAL(KIND=8)       :: var(nx,ny,nz,nt)

            !------------------------------------------------------------------!
            !                           Open NC file                           !
            !------------------------------------------------------------------!
            status = NF90_CREATE(oncfname, NF90_SHARE, ncid)
            CALL handle_err(status, 'nf_open o')

            !------------------------------------------------------------------!
            !                  Define dimensions and variables                 !
            !------------------------------------------------------------------!
            status = NF90_DEF_DIM(ncid,'x',nx,xdim)
            CALL handle_err(status,'def_xdim')

            status = NF90_DEF_DIM(ncid,'y',ny,ydim)
            CALL handle_err(status,'def_ydim')

            status = NF90_DEF_DIM(ncid,'z',nz,zdim)
            CALL handle_err(status,'def_zdim')

            status = NF90_DEF_DIM(ncid,'time',nt,timedim)
            CALL handle_err(status,'def_timedim')

            status = NF90_DEF_VAR(ncid,'x',NF90_DOUBLE,xdim,xid)
            CALL handle_err(status, 'def_var x')

            status = NF90_DEF_VAR(ncid,'y',NF90_DOUBLE, ydim, yid)
            CALL handle_err(status, 'def_var y')

            status = NF90_DEF_VAR(ncid,'z',NF90_DOUBLE, zdim, zid)
            CALL handle_err(status, 'def_var z')

            status = NF90_DEF_VAR(ncid,'time', NF90_DOUBLE, timedim, timeid)
            CALL handle_err(status, 'def_var time')

            status = NF90_DEF_VAR(ncid,varname,NF90_REAL,                      &
                                  (/xdim,ydim,zdim,timedim/), varid)
            CALL handle_err(status, 'def_var')

            status = NF90_PUT_ATT(ncid, varid, '_FillValue', missing)
            CALL handle_err(status, 'missing')

            status = NF90_ENDDEF(ncid)
            CALL handle_err(status, 'enddef')

            !------------------------------------------------------------------!
            !                             Put variables                        !
            !------------------------------------------------------------------!
            status = NF90_PUT_VAR(ncid,xid,x)
            CALL handle_err(status,'put_x')

            status = NF90_PUT_VAR(ncid,yid,y)
            CALL handle_err(status,'put_y')

            status = NF90_PUT_VAR(ncid,zid,z)
            CALL handle_err(status,'put_z')

            status = NF90_PUT_VAR(ncid,timeid,time)
            CALL handle_err(status,'put_time')

            status = NF90_PUT_VAR(ncid, varid, var)
            CALL handle_err(status, 'put_var')

            !------------------------------------------------------------------!
            !                             Close NC file                        !
            !------------------------------------------------------------------!
            status = NF90_CLOSE(ncid)

        END SUBROUTINE Write_netCDF_3D

        !----------------------------------------------------------------------!
        !                         Error message subroutine                     !
        !----------------------------------------------------------------------!
        SUBROUTINE handle_err(status, message)

            USE netcdf

            INTEGER,INTENT(IN)  :: status
            CHARACTER(*)        :: message

            IF(status /=NF90_NOERR) THEN
              WRITE(*,*) '(3a)', message, ' : ', NF90_STRERROR(status)
              STOP 'stopped'
            END IF

        END SUBROUTINE handle_err
