program main

  use ModFileList, only: getFileList, dynamicString, int2str
  use converter_AZ, only: main_work
  use basic_libs, only: BASENAME
  use global_constants_mod, only: dblp=>dp
  implicit none

   type(dynamicString), allocatable :: FileList(:), SystemInfo(:)
   character(63) :: record
   character(len=127)            :: searchString, outputfile
   character(len=3)              :: extension=".nc"
   character(len=:), allocatable :: dir,fileName,fileBase,FileExt,test
   logical                       :: exist
   integer                       :: i
   real*16                       :: dummy

   searchString = 'shrsg*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         call main_work(FileList(i)%record,trim(outputfile),"rcc",.true.,stdnm="relative_cloud_cover")
       end do
   end if

   searchString = 'wndsg*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         call main_work(FileList(i)%record,trim(outputfile),"wnd",.true.,stdnm="wind_speed")
       end do
   end if

   searchString = 'temsg*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         ! call main_work(FileList(i)%record,trim(outputfile),"prc",.true.)
         call main_work(FileList(i)%record,trim(outputfile),"tas",.true.,stdnm="surface_temperature")
       end do
   end if


   searchString = 'prcsg*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         call main_work(FileList(i)%record,trim(outputfile),"pr",.true.,stdnm="precipitation_flux",multiple=1._dblp/86400._dblp)
       end do
   end if

   searchString = 'rhusg*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         call main_work(FileList(i)%record,trim(outputfile),"hur",.true.,stdnm="relative_humidity")
       end do
   end if

end program main
