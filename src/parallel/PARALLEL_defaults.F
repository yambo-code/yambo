!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine PARALLEL_defaults(ENVIRONMENT)
 !
 use parallel_m,    ONLY:PARALLEL_n_structures_active,PARALLEL_CPU_used,&
&                        PARALLEL_CPU_role,ncpu,QP_range,n_bands,n_c_bands,n_v_bands,Q_range,&
&                        EH_range,K_range,CPU_structure,i_PAR_structure
 use interfaces,    ONLY:PARALLEL_live_message
 use com,           ONLY:error
 use stderr,        ONLY:intc
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 !
 ! Work Space
 ! 
 integer :: N_fields,CPU_s(4),i_c,NC,N_basis,i1
 !
 if (ncpu==1) return
 !
 N_fields=0
 CPU_s   =1
 NC      =ncpu
 !
 N_basis=1
 do i1=2,13
   if (mod(NC,i1)==0) then
     N_basis=i1
     exit
   endif
 enddo
 !
 call DEFAULT_action("check")
 !
 if (product(CPU_s)==ncpu) return
 !
 call PARALLEL_live_message('CPU structure provided for the '//trim(ENVIRONMENT)//&
&                           ' ENVIRONMENT is incomplete. Switching to defaults')
 !
 call DEFAULT_action("define")
 !
 if (product(PARALLEL_CPU_used(:PARALLEL_n_structures_active))/=ncpu) then
   call error('Impossible to define an appropriate parallel structure')
 endif
 !
 contains
   !
   subroutine DEFAULT_action(what)
     !
     character(*) :: what
     !
     do i_c=1,PARALLEL_n_structures_active
       !
       if (what/="check") cycle
       !
       select case(ENVIRONMENT)
         case("Response_G_space_Zero_Momentum")
           if (any(trim(PARALLEL_CPU_role(i_c))==(/"k","c","v"/))) then
             N_fields=N_fields+1
             CPU_s(N_fields)=PARALLEL_CPU_used(i_c)
           endif
         case("Response_G_space_Finite_Momentum","Response_G_space") 
           if (any(trim(PARALLEL_CPU_role(i_c))==(/"q","k","c","v"/))) then
             N_fields=N_fields+1
             CPU_s(N_fields)=PARALLEL_CPU_used(i_c)
           endif
         case("Real_Time") 
           if (trim(PARALLEL_CPU_role(i_c))=="k".or.trim(PARALLEL_CPU_role(i_c))=="b".or.&
&              trim(PARALLEL_CPU_role(i_c))=="q".or.trim(PARALLEL_CPU_role(i_c))=="qp") then
             N_fields=N_fields+1
             CPU_s(N_fields)=PARALLEL_CPU_used(i_c)
           endif
         case("Self_Energy") 
           if (trim(PARALLEL_CPU_role(i_c))=="q".or.trim(PARALLEL_CPU_role(i_c))=="qp".or.&
&              trim(PARALLEL_CPU_role(i_c))=="b") then
             N_fields=N_fields+1
             CPU_s(N_fields)=PARALLEL_CPU_used(i_c)
           endif
         case("Response_T_space") 
           if (trim(PARALLEL_CPU_role(i_c))=="k".or.trim(PARALLEL_CPU_role(i_c))=="eh".or.&
&              trim(PARALLEL_CPU_role(i_c))=="t") then
             N_fields=N_fields+1
             CPU_s(N_fields)=PARALLEL_CPU_used(i_c)
           endif
#if defined _SCALAPACK
         case("ScaLapacK") 
           if (trim(PARALLEL_CPU_role(i_c))=="p".or.trim(PARALLEL_CPU_role(i_c))=="d") then
             N_fields=N_fields+1
             CPU_s(N_fields)=PARALLEL_CPU_used(i_c)
           endif
#endif
       end select
       !
     enddo
     !
     if (what=="define") then
       select case(ENVIRONMENT)
         case("Response_G_space_Zero_Momentum")
           CPU_structure(i_PAR_structure)%ROLE(1)="c"
           CPU_structure(i_PAR_structure)%CPU(1)=assign_cpu(n_c_bands(2)-n_c_bands(1)+1)
           CPU_structure(i_PAR_structure)%ROLE(2)="v"
           CPU_structure(i_PAR_structure)%CPU(2)=assign_cpu(n_v_bands(2)-n_v_bands(1)+1)
           CPU_structure(i_PAR_structure)%ROLE(3)="k"
           CPU_structure(i_PAR_structure)%CPU(3)=assign_cpu(K_range)
           CPU_structure(i_PAR_structure)%N_chains=3
         case("Response_G_space_Finite_Momentum","Response_G_space") 
           CPU_structure(i_PAR_structure)%ROLE(1)="c"
           CPU_structure(i_PAR_structure)%CPU(1)=assign_cpu(n_c_bands(2)-n_c_bands(1)+1)
           CPU_structure(i_PAR_structure)%ROLE(2)="v"
           CPU_structure(i_PAR_structure)%CPU(2)=assign_cpu(n_v_bands(2)-n_v_bands(1)+1)
           CPU_structure(i_PAR_structure)%ROLE(3)="k"
           CPU_structure(i_PAR_structure)%CPU(3)=assign_cpu(K_range)
           CPU_structure(i_PAR_structure)%ROLE(4)="q"
           CPU_structure(i_PAR_structure)%CPU(4)=assign_cpu(Q_range(2)-Q_range(1)+1)
           CPU_structure(i_PAR_structure)%N_chains=4
#if defined _SC
         case("Real_Time") 
           CPU_structure(i_PAR_structure)%ROLE(1)="k"
           CPU_structure(i_PAR_structure)%CPU(1)=assign_cpu(K_range)
           CPU_structure(i_PAR_structure)%ROLE(2)="b"
           CPU_structure(i_PAR_structure)%CPU(2)=assign_cpu(n_bands(2)-n_bands(1)+1)
           CPU_structure(i_PAR_structure)%ROLE(3)="q"
           CPU_structure(i_PAR_structure)%CPU(3)=assign_cpu(Q_range(2))
           CPU_structure(i_PAR_structure)%N_chains=3
#endif
#if defined _SCALAPACK
         case("ScaLapacK") 
           CPU_structure(i_PAR_structure)%ROLE(1)="p"
           CPU_structure(i_PAR_structure)%CPU(1)=ncpu
           CPU_structure(i_PAR_structure)%ROLE(2)="d"
           CPU_structure(i_PAR_structure)%CPU(2)=1
           CPU_structure(i_PAR_structure)%N_chains=2
#endif
         case("Self_Energy") 
           CPU_structure(i_PAR_structure)%ROLE(1)="b"
           CPU_structure(i_PAR_structure)%CPU(1)=assign_cpu(n_bands(2)-n_bands(1)+1)
           CPU_structure(i_PAR_structure)%ROLE(2)="qp"
           CPU_structure(i_PAR_structure)%CPU(2)=assign_cpu(QP_range)
           CPU_structure(i_PAR_structure)%ROLE(3)="q"
           CPU_structure(i_PAR_structure)%CPU(3)=assign_cpu(Q_range(2))
           CPU_structure(i_PAR_structure)%N_chains=3
         case("Response_T_space") 
           CPU_structure(i_PAR_structure)%ROLE(1)="k"
           CPU_structure(i_PAR_structure)%CPU(1)=assign_cpu(K_range)
           CPU_structure(i_PAR_structure)%ROLE(2)="eh"
           CPU_structure(i_PAR_structure)%CPU(2)=assign_cpu(EH_range)
           CPU_structure(i_PAR_structure)%N_chains=2
       end select
       !
       call PARALLEL_clean_up_the_structure(PARALLEL_n_structures_active)
       !
       select case(PARALLEL_n_structures_active)
         case (1)
           CPU_structure(i_PAR_structure)%CPU_string =trim(intc( PARALLEL_CPU_used(1) ))
           CPU_structure(i_PAR_structure)%ROLE_string=trim(CPU_structure(i_PAR_structure)%ROLE(1))
         case (2)
           CPU_structure(i_PAR_structure)%CPU_string =trim(intc( PARALLEL_CPU_used(1) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(2) ))
           CPU_structure(i_PAR_structure)%ROLE_string=trim(CPU_structure(i_PAR_structure)%ROLE(1))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(2))
         case (3)
           CPU_structure(i_PAR_structure)%CPU_string =trim(intc( PARALLEL_CPU_used(1) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(2) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(3) ))
           CPU_structure(i_PAR_structure)%ROLE_string=trim(CPU_structure(i_PAR_structure)%ROLE(1))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(2))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(3))
         case (4)
           CPU_structure(i_PAR_structure)%CPU_string =trim(intc( PARALLEL_CPU_used(1) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(2) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(3) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(4) ))
           CPU_structure(i_PAR_structure)%ROLE_string=trim(CPU_structure(i_PAR_structure)%ROLE(1))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(2))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(3))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(4))
       end select
       !
     endif
     !
   end subroutine
   !
   integer function assign_cpu(I_range)
     integer :: I_range
     assign_cpu=1
     do while (assign_cpu*N_basis<=NC.and.assign_cpu*N_basis<=I_range) 
       assign_cpu=assign_cpu*N_basis
     enddo
     NC=NC/assign_cpu
   end function
   !
end subroutine
