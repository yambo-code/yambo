!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_Response_IO( )
 !
 ! io_RESPONSE check
 !
 ! No X I/O is possible ONLY when the number of cpu's allocated with the "q" field
 ! in the response function string is the same of the SE/RT/SC...string
 !  
 use IO_m,          ONLY:io_RESPONSE
 use parallel_m,    ONLY:n_CPU_str_max,CPU_structure,PARALLEL_CPU_role,PARALLEL_CPU_used,&
&                        PARALLEL_n_structures_active,i_PAR_structure,ncpu
 !
 implicit none
 !
 integer  :: i,i_s_se,N_q_SE,N_q_current
 !
 if (io_RESPONSE) return
 !
 call PARALLEL_get_user_structure("Self_Energy",.FALSE.)
 !
 if (len_trim( CPU_structure(i_PAR_structure)%ROLE(1) ) ==0 .and. ncpu>1) then
   call warning('Response function I/O forced in the default mode for the Self-Energy PARALLEL environment')
   io_RESPONSE=.TRUE.
   return
 endif
 !
 do i=1,n_CPU_str_max
   if (trim(CPU_structure(i)%Long_Description)=="Self_Energy") then
     i_s_se=i
   endif
 enddo
 !
 N_q_SE=1
 do i=1,CPU_structure(i_s_se)%N_chains
   if (trim(CPU_structure(i_s_se)%ROLE(i))=="q") N_q_SE=CPU_structure(i_s_se)%CPU(i)
 enddo
 !
 N_q_current=1
 do i=1,PARALLEL_n_structures_active
   if (trim(PARALLEL_CPU_role(i))=="q") N_q_current=PARALLEL_CPU_used(i)
 enddo
 !
 if (N_q_SE/=N_q_current) then
   call warning('Response function I/O forced. Different CPU for the "q" field in Response and Self-Energy and SE')
   io_RESPONSE=.TRUE.
 endif
 !
end subroutine PARALLEL_global_Response_IO
