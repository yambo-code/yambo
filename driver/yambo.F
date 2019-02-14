 !
 integer function yambo(&
&  np,pid,&
&  string_N,in_file_N,in_dir_N,out_dir_N,com_dir_N,job_N,&
&  string,   in_file,   in_dir,   out_dir,   com_dir,   job)
 !
 integer,              intent(in)    :: np,pid 
 integer,              intent(in)    :: string_N,in_file_N,in_dir_N,out_dir_N,com_dir_N,job_N
 character(string_N),  intent(in)    :: string
 character(in_file_N), intent(in)    :: in_file
 character(in_dir_N),  intent(in)    :: in_dir
 character(out_dir_N), intent(in)    :: out_dir
 character(com_dir_N), intent(in)    :: com_dir
 character(job_N),     intent(in)    :: job

 write (*,*) "F driver"
 write (*,*) "NP, PID   :",np,pid
 write (*,*) "RUNSTRING :",string_N,string
 write (*,*) "INPUT file:",in_file_N,in_file
 write (*,*) "INPUT dir :",in_dir_N,in_dir
 write (*,*) "OUT   dir :",out_dir_N,out_dir
 write (*,*) "COM   dir :",com_dir_N,com_dir
 write (*,*) "JOB       :",job_N,job

end function
