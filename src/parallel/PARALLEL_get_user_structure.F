!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_get_user_structure(ENVIRONMENT,RUNNING_ENVIRONMENT)
 !
 use pars,         ONLY:schlen
 use parallel_m,   ONLY:CPU_structure,n_CPU_str_max,PARALLEL_CPU_role,PARALLEL_CPU_used,&
&                       PARALLEL_n_structures_active,ncpu,i_PAR_structure
 use stderr,       ONLY:STRING_split
 !
 implicit none
 !
 character(*) :: ENVIRONMENT
 logical      :: RUNNING_ENVIRONMENT
 !
 ! Work Space
 !
 integer          ::i_s,i_c,n_structures,n_eh_grps,n_t_blks,n_k_cpus
 logical          ::q_found,k_found
 character(schlen)::c_strings(10),r_strings(10),CPU_string_,ROLE_string_
 !
 i_PAR_structure=-1
 !
 q_found   =.FALSE.
 k_found   =.FALSE.
 !
 do i_s=1,n_CPU_str_max
   !
   if (ncpu==1) then
     CPU_structure(i_s)%CPU_string=" "
     CPU_structure(i_s)%ROLE_string=" "
   endif
   !
   if (trim(CPU_structure(i_s)%Long_Description)==ENVIRONMENT) then
     i_PAR_structure=i_s
     CPU_string_=CPU_structure(i_PAR_structure)%CPU_string
     ROLE_string_=CPU_structure(i_PAR_structure)%ROLE_string
   endif
   !
 enddo
 !
 do i_c=1,len_trim(CPU_string_)
   if (CPU_string_(i_c:i_c)==".") CPU_string_(i_c:i_c)=" "
   if (CPU_string_(i_c:i_c)==",") CPU_string_(i_c:i_c)=" "
 enddo
 !
 do i_c=1,len_trim(ROLE_string_)
   if (ROLE_string_(i_c:i_c)==".") ROLE_string_(i_c:i_c)=" "
   if (ROLE_string_(i_c:i_c)==",") ROLE_string_(i_c:i_c)=" "
 enddo
 !
 call STRING_split(CPU_string_,c_strings,n_non_empty_strings=CPU_structure(i_PAR_structure)%N_chains)
 call STRING_split(ROLE_string_,r_strings)
 !
 n_structures=0
 n_eh_grps=1
 n_t_blks=1
 n_k_cpus=1
 !
 do i_c=1,CPU_structure(i_PAR_structure)%N_chains
   !
   read(c_strings(i_c),'(i3)') CPU_structure(i_PAR_structure)%CPU(i_c)
   read(r_strings(i_c),'(a )') CPU_structure(i_PAR_structure)%ROLE(i_c)
   if (trim(CPU_structure(i_PAR_structure)%ROLE(i_c))=="eh") n_eh_grps=CPU_structure(i_PAR_structure)%CPU(i_c)
   if (trim(CPU_structure(i_PAR_structure)%ROLE(i_c))=="t" ) n_t_blks=CPU_structure(i_PAR_structure)%CPU(i_c)
   if (trim(CPU_structure(i_PAR_structure)%ROLE(i_c))=="k") then
     k_found=.TRUE.
     n_k_cpus=CPU_structure(i_PAR_structure)%CPU(i_c)
   endif
   if (trim(CPU_structure(i_PAR_structure)%ROLE(i_c))=="q") q_found=.TRUE.
   !
 enddo
 !
 if (.not.RUNNING_ENVIRONMENT) return
 !
 call PARALLEL_set_the_active_structures( n_structures )
 !
 ! The two top fields ("q" and "k") must be defined anyway. But only these two!
 !
 if (.not.q_found.or.ncpu==1) then
   n_structures=n_structures+1
   PARALLEL_CPU_used(n_structures)=1
   PARALLEL_CPU_role(n_structures)="q"
 endif
 !
 if (.not.k_found.or.ncpu==1) then
   n_structures=n_structures+1
   PARALLEL_CPU_used(n_structures)=1
   PARALLEL_CPU_role(n_structures)="k"
 endif
 !
 if (ncpu==1) n_structures=0
 !
 CPU_structure(i_PAR_structure)%N_chains=n_structures
 !
 PARALLEL_n_structures_active=n_structures
 !
end subroutine PARALLEL_get_user_structure
