!
integer function yambo(&
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
 integer i_runlevel,i_err,version,subversion,patchlevel,revision
 character(100) what_is_running,key,desc,pj,libraries,hash
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
  
   what_is_running=" "
   call get_running_tool(what_is_running)
   write (*,'(/2a)') " RUNNING   :",trim(what_is_running)
   libraries=" "
   call get_libraries(libraries)
   write (*,'(2a)') " LIBS      :",trim(libraries)
   hash=" "
   call get_version(version,subversion,patchlevel,revision,hash)
   write (*,'(a,2(i1,a),i1,1x,i5,1x,a/)') " VERSION   :",version,".",subversion,".",patchlevel,revision,trim(hash)
  
   do i_runlevel=1,99
     key=" "
     desc=" "
     pj=" "
     call get_runlevel(key,1,i_runlevel)
     call get_runlevel(desc,2,i_runlevel)
     call get_runlevel(pj,3,i_runlevel)
     if (trim(key)=="EMPTY") cycle
     write (*,*) "RUNLEVEL #",i_runlevel," KEY:",trim(key),' DESC:',trim(desc),' PJ:',trim(pj)
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
