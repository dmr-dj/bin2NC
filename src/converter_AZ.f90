     module converter_AZ

      implicit none

      private

      public:: main_work


     contains

     subroutine main_work()


      use global_constants_mod, only : dblp=>dp, ip, silp=>sp
      use ncio, only: nc_read

      implicit none

      integer(kind=ip) :: unitinputfile, io_stat=0, nb_days=365, i,unitoutputfile, lat, lon
      integer(kind=ip), dimension(1) :: lo, la

      integer(kind=ip), dimension(2) :: size_coords

      real(kind=dblp), dimension(:), allocatable :: x_input_var
      real(KIND=dblp),  PARAMETER :: undef_dblp = (-1.0_dblp)*HUGE(1.0_silp)

      real(kind=dblp)                            :: lat_itude, long_itude, no_data=undef_dblp, lon360
      real(kind=dblp), dimension(:), allocatable :: coord_lat, coord_lon
      real(kind=dblp), dimension(:,:,:), allocatable :: var_to_write_nc, var_to_check_nc

      logical :: ca_marche

      integer :: nb_points = 0

      coord_lat = read_input_geom("test-data/6k_dominant_caraib.nc","lat")
      coord_lon = read_input_geom("test-data/6k_dominant_caraib.nc","lon")

      open(newunit=unitinputfile,file='test-data/temsg2.dat', form='formatted')


      allocate(x_input_var(nb_days))
      allocate(var_to_write_nc(size(coord_lon),size(coord_lat),nb_days))
      allocate(var_to_check_nc(size(coord_lon),size(coord_lat),nb_days))

      var_to_write_nc(:,:,:) = no_data
      var_to_check_nc(:,:,:) = no_data


      do while(io_stat .eq. 0)
        read(unitinputfile,*,iostat=io_stat) long_itude, lat_itude, (x_input_var(i),i=1,nb_days)

        if (long_itude.gt.180.0_dblp) long_itude = long_itude - 360.0_dblp

        lo(:) = minloc(abs(long_itude-coord_lon))
        la(:) = minloc(abs(lat_itude-coord_lat))

        var_to_write_nc(lo(1),la(1),:) = x_input_var(:)
        nb_points = nb_points + 1
      enddo

      close(unitinputfile)

      ca_marche = write_nc_file_2DTime("temsg2.nc","temsg",var_to_write_nc,coord_lon,coord_lat,nb_days)


      call nc_read("temsg2.nc","temsg",var_to_check_nc)

      write(*,*) "check", maxval(var_to_check_nc-var_to_write_nc), minval(var_to_check_nc-var_to_write_nc), nb_points &
                        , ubound(var_to_check_nc,dim=1),ubound(var_to_check_nc,dim=2)


      open(newunit=unitoutputfile, file='temsg2-chck.dat', form='formatted')


      do lon=1,ubound(var_to_check_nc,dim=1)
        do lat=1,ubound(var_to_check_nc,dim=2)
          if (coord_lon(lon).lt.0.0_dblp) lon360 = coord_lon(lon) + 360.0_dblp
          if (var_to_check_nc(lon,lat,1).GT.undef_dblp) then
            write(unitoutputfile, '(*(F10.4))') lon360, coord_lat(lat), (var_to_check_nc(lon,lat,i),i=1,nb_days)
          endif
        enddo
      enddo

      close(unitoutputfile)

      deallocate(x_input_var)
      deallocate(var_to_write_nc)
      deallocate(var_to_check_nc)


     end subroutine main_work

     function read_input_geom(file_name,nameXY) result (coord_to_read)

       use ncio, only: nc_read, nc_size
       use global_constants_mod, only : dblp=>dp, ip

       character(len=*), intent(in)               :: file_name
       character(len=3), intent(in)               :: nameXY
       real(kind=dblp), dimension(:), allocatable   :: coord_to_read


       integer(kind=ip)                           :: size_coord

       size_coord = nc_size(file_name, nameXY)

       allocate(coord_to_read(1:size_coord))
       call nc_read(file_name,nameXY,coord_to_read)

     end function read_input_geom

     function write_nc_file_2DTime(filenm_to_write,name_var_to_write, var_to_write,axis_x, axis_y,nb_time) result (success)

        use ncio, only: nc_create, nc_write_dim, nc_write
        use global_constants_mod, only : dblp=>dp, ip, silp=>sp


        real(kind=dblp), dimension(:,:,:), intent(in) :: var_to_write
        real(kind=dblp), dimension(:),     intent(in) :: axis_x, axis_y
        character(len=*),                intent(in) :: filenm_to_write, name_var_to_write
        integer(kind=ip),                intent(in) :: nb_time


        logical                                     :: success
        REAL(KIND=dblp),  PARAMETER :: undef_dblp = (-1.0_dblp)*HUGE(1.0_silp)
        integer(kind=ip)                            :: i

        CALL nc_create(filenm_to_write,overwrite=.true.,netcdf4=.true., author="roche")
        ! CALL nc_write_attr(this%filename,"Title",TRIM(this%title_file))
        ! CALL nc_write_attr(this%filename,"Institution", TRIM(this%institution_name))

             ! Time AXIS
        CALL nc_write_dim(filenm_to_write,"time",x=1.0, units="days",calendar="365d"   &
                          , unlimited=.TRUE.)
             ! Spatial AXES
        CALL nc_write_dim(filenm_to_write,"latitude" ,x=axis_y,units="degrees_north")
        CALL nc_write_dim(filenm_to_write,"longitude",x=axis_x,units="degrees_east")

        do i = 1, nb_time
          call nc_write(filenm_to_write,"time",i,dim1="time",start=[i],count=[1])
          call nc_write(filenm_to_write,name_var_to_write,var_to_write(:,:,i)                                        &
                          , dim1="longitude",dim2="latitude",dim3="time"                                             &
                          ,start=[1,1,i],count=[UBOUND(var_to_write,dim=1),UBOUND(var_to_write,dim=2),1],      &
                            missing_value=undef_dblp)
        enddo

     end function write_nc_file_2DTime

     end module converter_AZ
