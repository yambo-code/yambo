!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_get_ENVIRONMENT_structure(ENVIRONMENT)
 !
 use pars,         ONLY:schlen
 use parallel_m,   ONLY:CPU_structure,n_CPU_str_max,ncpu
 use stderr,       ONLY:remove_new_line
 !
 implicit none
 !
 character(*) :: ENVIRONMENT
 !
 ! Work Space
 !
 integer            ::i_s,i_s_range(2)
 character(schlen)  ::PAR_pre_field
 character(len=255) ::CPU_string,ROLE_string,INV_string,DIAGO_string
 !
 if (ncpu==1) return
 !
 if (ENVIRONMENT=="ALL") then
   i_s_range=(/1,n_CPU_str_max/)
 else
   do i_s=1,n_CPU_str_max
     if (trim(CPU_structure(i_s)%Long_Description)==ENVIRONMENT) then
       i_s_range=i_s  
       exit
     endif
   enddo
 endif
 !
 do i_s=i_s_range(1),i_s_range(2)
   !
   ! Input field commands on shell environments
   !
   if (len_trim(CPU_structure(i_s)%CPU_string)>0) cycle
   !
   PAR_pre_field=CPU_structure(i_s)%Short_Description
   !
   CALL get_environment_variable("YAMBO_"//trim(PAR_pre_field)//"_CPU",CPU_string)
   CALL get_environment_variable("YAMBO_"//trim(PAR_pre_field)//"_ROLEs",ROLE_string)
   CALL get_environment_variable("YAMBO_"//trim(PAR_pre_field)//"_nCPU_LinAlg_INV",INV_string)
   CALL get_environment_variable("YAMBO_"//trim(PAR_pre_field)//"_nCPU_LinAlg_DIAGO",DIAGO_string)
   !
   call remove_new_line(CPU_string)
   call remove_new_line(ROLE_string)
   call remove_new_line(INV_string)
   call remove_new_line(DIAGO_string)
   !
   if (len_trim(CPU_string)>0)    read (CPU_string,*)   CPU_structure(i_s)%CPU_string
   if (len_trim(ROLE_string)>0)   read (ROLE_string,*)  CPU_structure(i_s)%ROLE_string
   if (len_trim(INV_string)>0)    read (INV_string,*)   CPU_structure(i_s)%nCPU_lin_algebra_INV
   if (len_trim(DIAGO_string)>0)  read (DIAGO_string,*) CPU_structure(i_s)%nCPU_lin_algebra_DIAGO
   !
 enddo
 !
end subroutine PARALLEL_get_ENVIRONMENT_structure
