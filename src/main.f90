program main

  use ModFileList, only: getFileList, dynamicString, int2str
  use converter_AZ, only: main_work
  use basic_libs, only: BASENAME
  implicit none

!~   call main_work('test-data/temsg2.dat',"temsg2.nc","temsg",.true.)
!~   call main_work("temsg2.nc", 'temsg2-chck.dat',"temsg",.false.)

   type(dynamicString), allocatable :: FileList(:), SystemInfo(:)
   character(63) :: record
   character(len=127)            :: searchString, outputfile
   character(len=3)              :: extension=".nc"
   character(len=:), allocatable :: dir,fileName,fileBase,FileExt,test
   logical                       :: exist
   integer                       :: i
   real*16                       :: dummy

   searchString = 'shrsg*.dat'
   ! searchString = 'pi_corr_tem*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         ! call main_work(FileList(i)%record,trim(outputfile),"tem",.true.)
         call main_work(FileList(i)%record,trim(outputfile),"shr",.true.)
       end do
!~        call main_work("temsg230.dat","temsg230.nc","temsg",.true.)
   end if

   searchString = 'wndsg*.dat'
   ! searchString = 'pi_corr_prc*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         ! call main_work(FileList(i)%record,trim(outputfile),"prc",.true.)
         call main_work(FileList(i)%record,trim(outputfile),"wnd",.true.)
       end do
!~        call main_work("temsg230.dat","temsg230.nc","temsg",.true.)
   end if

   searchString = 'temsg*.dat'
   ! searchString = 'pi_corr_prc*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         ! call main_work(FileList(i)%record,trim(outputfile),"prc",.true.)
         call main_work(FileList(i)%record,trim(outputfile),"tem",.true.)
       end do
!~        call main_work("temsg230.dat","temsg230.nc","temsg",.true.)
   end if


   searchString = 'prcsg*.dat'
   ! searchString = 'pi_corr_prc*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         ! call main_work(FileList(i)%record,trim(outputfile),"prc",.true.)
         call main_work(FileList(i)%record,trim(outputfile),"prc",.true.)
       end do
!~        call main_work("temsg230.dat","temsg230.nc","temsg",.true.)
   end if

   searchString = 'rhusg*.dat'
   ! searchString = 'pi_corr_prc*.dat'
   FileList = getFileList(searchString)

   if (size(FileList)==0) then
       write(*,*) 'no file detected'
   else
       write(*,*) int2str(size(FileList)), " files detected containing ", trim(adjustl(searchString)), " :"
       do i = 1,size(FileList)
         
         outputfile=trim(basename(FileList(i)%record, ".dat"))//extension
         write(*,*) "Converting: -> ", FileList(i)%record, " " , trim(outputfile)
         ! call main_work(FileList(i)%record,trim(outputfile),"prc",.true.)
         call main_work(FileList(i)%record,trim(outputfile),"rhu",.true.)
       end do
!~        call main_work("temsg230.dat","temsg230.nc","temsg",.true.)
   end if

end program main
