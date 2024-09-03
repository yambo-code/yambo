!
integer function main(&
&  np,pid,&
&  string_N,in_file_N,in_dir_N,out_dir_N,com_dir_N,job_N,&
&  string,   in_file,   in_dir,   out_dir,   com_dir,   job)
 !
 use C_driver
 !
 integer,              intent(in)    :: np,pid 
 integer,              intent(in)    :: string_N,in_file_N,in_dir_N,out_dir_N,com_dir_N,job_N
 character(string_N),  intent(in)    :: string
 character(in_file_N), intent(in)    :: in_file
 character(in_dir_N),  intent(in)    :: in_dir
 character(out_dir_N), intent(in)    :: out_dir
 character(com_dir_N), intent(in)    :: com_dir
 character(job_N),     intent(in)    :: job
 !
 integer i_runlevel
 character(100) key,desc,pj,hash
 !
 if (pid==0) then
   !
   write (*,'(/a/)') " F driver"
   write (*,*) "NP, PID   :",np,pid
   write (*,*) "RUNSTRING :",string_N,string
   write (*,*) "INPUT file:",in_file_N,in_file
   write (*,*) "INPUT dir :",in_dir_N,in_dir
   write (*,*) "OUT   dir :",out_dir_N,out_dir
   write (*,*) "COM   dir :",com_dir_N,com_dir
   write (*,*) "JOB       :",job_N,job
  
   call C_driver_transfer( )
   write (*,'(/2a)') " RUNNING   :",trim(code_bin)
   write (*,'(2a)') " LIBS      :",trim(code_libraries)
   write (*,'(a,2(i1,a),i1/)') " VERSION   :",code_version(1),".",code_version(2),".",code_version(3)
   do i_runlevel=0,199
     key=" "
     desc=" "
     call get_runlevel(key,1,i_runlevel)
     call get_runlevel(desc,2,i_runlevel)
     if (trim(key)=="EMPTY") cycle
     write (*,*) "RUNLEVEL #",i_runlevel," KEY:",trim(key),' DESC:',trim(desc)
   enddo
endif

end function
   !
   character(100) function cstr(si) result(so)
     character(*), intent(IN) :: si
     integer :: i 
     i = len(trim(si))
     call clear_str(so)
     so(1:i) = si(1:i)
     so(i+1:i+1) = achar(0)
   end function cstr

   subroutine clear_str(str)
     character(*), intent(out) :: str
     integer :: i
     do i = 1, len(str)
       str(i:i) = " " 
     end do
   end subroutine clear_str
