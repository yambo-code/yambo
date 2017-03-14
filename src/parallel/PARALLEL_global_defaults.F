!
!        Copyright (C) 2000-2017 the YAMBO team
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
 integer :: CPU_s(4),NC,N_CPU_fields,BASIS
 !
 if (ncpu==1) return
 !
 N_CPU_fields=0
 CPU_s       =1
 NC          =ncpu
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
     integer      :: i_c,i_c_used
     !
     i_c_used=0
     !
     do i_c=1,PARALLEL_n_structures_active
       !
       if (what/="check") cycle
       !
       select case(ENVIRONMENT)
         case("Response_G_space_Zero_Momentum")
           N_CPU_fields=3
           if (any(trim(PARALLEL_CPU_role(i_c))==(/"k","c","v"/))) then
             i_c_used=i_c_used+1
             CPU_s(i_c_used)=PARALLEL_CPU_used(i_c)
           endif
         case("Response_G_space_Finite_Momentum","Response_G_space") 
           N_CPU_fields=4
           if (any(trim(PARALLEL_CPU_role(i_c))==(/"q","k","c","v"/))) then
             i_c_used=i_c_used+1
             CPU_s(i_c_used)=PARALLEL_CPU_used(i_c)
           endif
         case("Real_Time") 
           N_CPU_fields=4
           if (trim(PARALLEL_CPU_role(i_c))=="k".or.trim(PARALLEL_CPU_role(i_c))=="b".or.&
&              trim(PARALLEL_CPU_role(i_c))=="q".or.trim(PARALLEL_CPU_role(i_c))=="qp") then
             i_c_used=i_c_used+1
             CPU_s(i_c_used)=PARALLEL_CPU_used(i_c)
           endif
         case("Self_Energy") 
           N_CPU_fields=3
           if (trim(PARALLEL_CPU_role(i_c))=="q".or.trim(PARALLEL_CPU_role(i_c))=="qp".or.&
&              trim(PARALLEL_CPU_role(i_c))=="b") then
             i_c_used=i_c_used+1
             CPU_s(i_c_used)=PARALLEL_CPU_used(i_c)
           endif
         case("Response_T_space") 
           N_CPU_fields=3
           if (trim(PARALLEL_CPU_role(i_c))=="k".or.trim(PARALLEL_CPU_role(i_c))=="eh".or.&
&              trim(PARALLEL_CPU_role(i_c))=="t") then
             i_c_used=i_c_used+1
             CPU_s(i_c_used)=PARALLEL_CPU_used(i_c)
           endif
#if defined _SCALAPACK
         case("ScaLapacK") 
           N_CPU_fields=2
           if (trim(PARALLEL_CPU_role(i_c))=="p".or.trim(PARALLEL_CPU_role(i_c))=="d") then
             i_c_used=i_c_used+1
             CPU_s(i_c_used)=PARALLEL_CPU_used(i_c)
           endif
#endif
       end select
       !
     enddo
     !
     if (what=="define") then
       CPU_structure(i_PAR_structure)%N_chains=N_CPU_fields
       select case(ENVIRONMENT)
         case("Response_G_space_Zero_Momentum")
           call GIMME_a_parallel_proposal(N_CPU_fields,(/"c","v","k"/))
         case("Response_G_space_Finite_Momentum","Response_G_space") 
           call GIMME_a_parallel_proposal(N_CPU_fields,(/"c","v","k","q"/))
#if defined _RT
         case("Real_Time") 
           call GIMME_a_parallel_proposal(N_CPU_fields,(/"k","b","q"/))
#endif
#if defined _SCALAPACK
         case("ScaLapacK") 
           CPU_structure(i_PAR_structure)%ROLE(1)="p"
           CPU_structure(i_PAR_structure)%CPU(1)=ncpu
           CPU_structure(i_PAR_structure)%ROLE(2)="d"
           CPU_structure(i_PAR_structure)%CPU(2)=1
#endif
         case("Self_Energy") 
           call GIMME_a_parallel_proposal(N_CPU_fields,(/" b","qp"," q"/))
         case("Response_T_space") 
           call GIMME_a_parallel_proposal(N_CPU_fields,(/" k","eh"/))
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
         case (5)
           CPU_structure(i_PAR_structure)%CPU_string =trim(intc( PARALLEL_CPU_used(1) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(2) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(3) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(4) ))//"."//&
&                                                     trim(intc( PARALLEL_CPU_used(5) ))
           CPU_structure(i_PAR_structure)%ROLE_string=trim(CPU_structure(i_PAR_structure)%ROLE(1))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(2))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(3))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(4))//"."//&
&                                                     trim(CPU_structure(i_PAR_structure)%ROLE(5))
       end select
       !
     endif
     !
   end subroutine
   !
   subroutine GIMME_a_parallel_proposal(N_fields,FIELDS)
     !
     use stderr,      ONLY:string_remove
     use vec_operate, ONLY:sort
     !
     ! "c" -> PAR_n_c_bands(2)-PAR_n_c_bands(1)+1
     ! "v" -> PAR_n_v_bands(2)-PAR_n_v_bands(1)+1
     ! "k" -> PAR_K_range
     ! "b" -> PAR_n_bands(2)-PAR_n_bands(1)+1
     ! "q" -> PAR_Q_range(2)- PAR_Q_range(1)+1
     ! "qp"-> PAR_QP_range
     ! "eh"-> PAR_EH_range
     !
     integer      :: N_fields,i_f,VALS(N_fields),VALS_sorted(N_fields),&
&                    MY_cpu(N_fields),i_v,INDEX_sorted(N_fields),i_x,i_v_ref
     character(*) :: FIELDS(N_fields)
     character(3) :: ch
     !
     VALS  =0
     MY_cpu=1
     !
     do i_f=1,N_fields
       ch=string_remove(FIELDS(i_f)," ")
       CPU_structure(i_PAR_structure)%ROLE(i_f)=trim(ch)
       if (trim(ch)=="c" ) VALS(i_f)=n_c_bands(2)-n_c_bands(1)+1
       if (trim(ch)=="v" ) VALS(i_f)=n_v_bands(2)-n_v_bands(1)+1
       if (trim(ch)=="b" ) VALS(i_f)=n_bands(2)-n_bands(1)+1
       if (trim(ch)=="k" ) VALS(i_f)=K_range
       if (trim(ch)=="q" ) VALS(i_f)=Q_range(2)-Q_range(1)+1
       if (trim(ch)=="qp") VALS(i_f)=QP_range
       if (trim(ch)=="eh") VALS(i_f)=EH_range
     enddo
     !
     VALS_sorted=VALS
     call sort(VALS_sorted,indx=INDEX_sorted)
     !
     sc_loop: do while(product(MY_cpu)/=ncpu)
       do i_v=N_fields,1,-1
         i_v_ref=i_v-1
         if  (i_v==1) i_v_ref=2
         i_x=INDEX_sorted(i_v)
         do while (VALS_sorted(i_v)>=VALS_sorted(i_v_ref))
           call define_the_basis( )
           if (all(VALS_sorted<BASIS)) exit sc_loop
           if (BASIS>VALS_sorted(i_v)) exit
           MY_cpu(i_x)=MY_cpu(i_x)*BASIS
           VALS_sorted(i_v)=VALS(i_x)/MY_cpu(i_x)
           NC=NC/BASIS
           if (all(VALS_sorted==1)) exit sc_loop
           if (product(MY_cpu)==ncpu) exit sc_loop
         enddo
       enddo
     enddo sc_loop
     !
     do i_f=1,N_fields
       CPU_structure(i_PAR_structure)%CPU(i_f)=MY_cpu(i_f)
     enddo
     !
   end subroutine
   !
   subroutine define_the_basis( )
     integer :: i1,i3
     i1_loop: do i1=2,NC
       do i3=2,i1-1
         if (mod(i1,i3)==0) cycle i1_loop
       enddo
       if (mod(NC,i1)==0) then
         BASIS=i1
         return
       endif
     enddo i1_loop
   end subroutine
   !
end subroutine
