     module converter_AZ

      use global_constants_mod, only : dblp=>dp, strlen => str_len

      implicit none

      private

      public:: main_work

      logical :: initialized = .false.

      real(kind=dblp), dimension(:), allocatable :: coord_lat, coord_lon

      character(len=strlen) :: ref_latlon_file = "../fxd_data/EUROPEAN_GRID_Zapolska.nc"

     contains

     subroutine init_coords()

      use ncio, only: nc_read

       coord_lat = read_input_geom(ref_latlon_file,"lat")
       coord_lon = read_input_geom(ref_latlon_file,"lon")

       initialized = .true.

     end subroutine


     subroutine main_work(filenmin, filenmout, varnm,txt_to_nc,stdnm,multiple)


      use global_constants_mod, only : dblp=>dp, ip, silp=>sp
      use ncio, only: nc_read

      implicit none

      logical, intent(in)          :: txt_to_nc
      character(len=*), intent(in) :: filenmin, filenmout, varnm
      character(len=*), optional, intent(in) :: stdnm
      real(kind=dblp), optional, intent(in) :: multiple

      integer(kind=ip) :: unitinputfile, io_stat=0, nb_days=365, i,unitoutputfile, lat, lon
      integer(kind=ip), dimension(1) :: lo, la

      integer(kind=ip), dimension(2) :: size_coords

      real(kind=dblp), dimension(:), allocatable :: x_input_var
      real(KIND=dblp),  PARAMETER :: undef_dblp = (-1.0_dblp)*HUGE(1.0_silp)

      real(kind=dblp)                            :: lat_itude, long_itude, no_data=undef_dblp, lon360
      real(kind=dblp)                            :: multple

      real(kind=dblp), dimension(:,:,:), allocatable :: var_to_write_nc, var_to_check_nc

      logical :: ca_marche

      integer :: nb_points = 0


      if (.not. initialized) then
        call init_coords()
      endif

      if (PRESENT(multiple)) then
        multple = multiple
      else
        multple = 1.0_dblp
      endif

      if ( txt_to_nc ) then

      open(newunit=unitinputfile,file=filenmin, form='formatted')

      allocate(x_input_var(nb_days))
      allocate(var_to_write_nc(size(coord_lon),size(coord_lat),nb_days))

      var_to_write_nc(:,:,:) = no_data

      do while(io_stat .eq. 0)
        read(unitinputfile,*,iostat=io_stat) long_itude, lat_itude, (x_input_var(i),i=1,nb_days)

        if (long_itude.gt.180.0_dblp) long_itude = long_itude - 360.0_dblp

        lo(:) = minloc(abs(long_itude-coord_lon))
        la(:) = minloc(abs(lat_itude-coord_lat))

        var_to_write_nc(lo(1),la(1),:) = x_input_var(:)*multple
        nb_points = nb_points + 1
      enddo

      close(unitinputfile)

      ca_marche = write_nc_file_2DTime(filenmout,varnm,var_to_write_nc,coord_lon,coord_lat,nb_days,stdnm=stdnm)

      deallocate(x_input_var)
      deallocate(var_to_write_nc)

      io_stat=0
      
      else

        allocate(var_to_check_nc(size(coord_lon),size(coord_lat),nb_days))
        var_to_check_nc(:,:,:) = no_data
        call nc_read(filenmin,varnm,var_to_check_nc)

        ! write(*,*) "check", maxval(var_to_check_nc-var_to_write_nc)                   &
        !                , minval(var_to_check_nc-var_to_write_nc), nb_points           &
        !                , ubound(var_to_check_nc,dim=1),ubound(var_to_check_nc,dim=2)


        open(newunit=unitoutputfile, file=filenmout, form='formatted')


        do lon=1,ubound(var_to_check_nc,dim=1)
          do lat=1,ubound(var_to_check_nc,dim=2)
            if (coord_lon(lon).lt.0.0_dblp) lon360 = coord_lon(lon) + 360.0_dblp
            if (var_to_check_nc(lon,lat,1).GT.undef_dblp) then
              write(unitoutputfile, '(*(F10.4))') lon360, coord_lat(lat), (var_to_check_nc(lon,lat,i),i=1,nb_days)
            endif
          enddo
        enddo

        close(unitoutputfile)
        deallocate(var_to_check_nc)

       endif

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

     function write_nc_file_2DTime(filenm_to_write,name_var_to_write, var_to_write,axis_x, axis_y,nb_time, stdnm) result (success)

        use ncio, only: nc_create, nc_write_dim, nc_write, nc_write_attr, nc_write_map
        use global_constants_mod, only : dblp=>dp, ip, silp=>sp


        real(kind=dblp), dimension(:,:,:), intent(in) :: var_to_write
        real(kind=dblp), dimension(:),     intent(in) :: axis_x, axis_y
        character(len=*),                intent(in) :: filenm_to_write, name_var_to_write
        character(len=*), optional,      intent(in) :: stdnm
        integer(kind=ip),                intent(in) :: nb_time


        logical                                     :: success
        REAL(KIND=dblp),  PARAMETER :: undef_dblp = (-1.0_dblp)*HUGE(1.0_silp)
        integer(kind=ip)                            :: i

        CALL nc_create(filenm_to_write,overwrite=.true.,netcdf4=.true., author="roche")
               
        ! CALL nc_write_attr(this%filename,"Title",TRIM(this%title_file))
        CALL nc_write_attr(filenm_to_write,         &
                          "Institution", "Laboratoire des Sciences du Climat et de l'Environnement ; Vrije Universiteit Amsterdam")
        CALL nc_write_attr(filenm_to_write, "Conventions", "CF-1.8")

             ! Time AXIS
        CALL nc_write_dim(filenm_to_write,"time",x=1.0, units="days since 1601-1-1 0:0:0",calendar="365_day" &
                         ,long_name="time @ 3 ka BP", standard_name="time", unlimited=.TRUE.)
             ! Spatial AXES
        CALL nc_write_dim(filenm_to_write,"lat" ,x=axis_y,units="degrees_north", long_name="latitude", standard_name="latitude")
        CALL nc_write_dim(filenm_to_write,"lon",x=axis_x,units="degrees_east", long_name="longitude", standard_name="longitude")

        call nc_write(filenm_to_write,"time",(/ (i, i = 0, nb_time-1) /),dim1="time",start=[1],count=[nb_time])
        call nc_write(filenm_to_write,name_var_to_write,var_to_write(:,:,:)                                            &
                                          , dim1="lon",dim2="lat",dim3="time",start=[1,1,1]                            &
                                          ,count=[UBOUND(var_to_write,dim=1),UBOUND(var_to_write,dim=2),nb_time]       &
                                          ,standard_name=stdnm, missing_value=undef_dblp)
        
        CALL nc_write_attr(filenm_to_write, varname="time", name="axis", value="T")
        
        call nc_write_map(filenm_to_write,"latitude_longitude")
        CALL nc_write_attr(filenm_to_write, varname="crs", name="semi_major_axis", value=6371000.0)
        CALL nc_write_attr(filenm_to_write, varname="crs", name="inverse_flattening", value=0)
 
        if ( present(stdnm) ) then
        
          select case(trim(stdnm))
          case("surface_temperature" )
              CALL nc_write_attr(filenm_to_write, varname=name_var_to_write, name="units", value="degC")
          case("relative_humidity" )
              CALL nc_write_attr(filenm_to_write, varname=name_var_to_write, name="units", value="1")              
          case("precipitation_flux" )
              CALL nc_write_attr(filenm_to_write, varname=name_var_to_write, name="units", value="kg.m-2.s-1")                            
          case("relative_cloud_cover" )
              CALL nc_write_attr(filenm_to_write, varname=name_var_to_write, name="units", value="1")     
          case("wind_speed" )
              CALL nc_write_attr(filenm_to_write, varname=name_var_to_write, name="units", value="m.s-1")               
          case DEFAULT
              ! do nothing
          end select
               
        endif
        
     end function write_nc_file_2DTime

     end module converter_AZ
