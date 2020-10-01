!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PARALLEL_global_defaults(ENVIRONMENT)
 !
 use parallel_m,    ONLY:PARALLEL_n_structures_active,PARALLEL_CPU_used,&
&                        ncpu,CPU_structure,i_PAR_structure,PARALLEL_ENV_uses_default,PARALLEL_CPU_role
 use interfaces,    ONLY:PARALLEL_live_message
 use stderr,        ONLY:intc
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 !
 ! Work Space
 ! 
 integer :: i_p,NC,BASIS,n_par_la
 !
 ! Is there anything to do?
 !
 if (ncpu==1) return
 !
 ! Set default for INVERSION/DIAGONALIZATION
 !
 n_par_la=1
 !
#if defined _SCALAPACK 
 !
 ! In the SLK case the default = nxn (with n=2,4,6)
 !
 if (ncpu>=4 ) n_par_la=4
 if (ncpu>=16) n_par_la=16
 if (ncpu>=36) n_par_la=36
 !
#endif
 !
 if(CPU_structure(i_PAR_structure)%nCPU_lin_algebra_INV==-1)  then
   CPU_structure(i_PAR_structure)%nCPU_lin_algebra_INV  =n_par_la
 endif
 if(CPU_structure(i_PAR_structure)%nCPU_lin_algebra_DIAGO==-1)  then
   CPU_structure(i_PAR_structure)%nCPU_lin_algebra_DIAGO=n_par_la
 endif
 !
 if (.not.PARALLEL_ENV_uses_default(i_PAR_structure).and.&
&    product(PARALLEL_CPU_used(:PARALLEL_n_structures_active))==ncpu) return
 !
 call PARALLEL_live_message(trim(ENVIRONMENT)//' parallel ENVIRONMENT is incomplete. Switching to defaults')
 !
 ! Propose the defaults
 !-----------------------------
 select case(ENVIRONMENT)
   case("DIPOLES")
     call GIMME_a_parallel_proposal(3,(/"v","c","k"/))
   case("Response_G_space_and_IO","Response_G_space")
     call GIMME_a_parallel_proposal(5,(/"g","v","c","k","q"/))
   case("Real_Time") 
     call GIMME_a_parallel_proposal(4,(/" k"," b","qp"," q"/))
   case("PH_Self_Energy") 
     call GIMME_a_parallel_proposal(3,(/"k","b","q"/))
   case("Non_Linear") 
     call GIMME_a_parallel_proposal(2,(/" w"," k"/))
   case("ScaLapacK") 
     CPU_structure(i_PAR_structure)%ROLE(1)="p"
     CPU_structure(i_PAR_structure)%CPU(1)=ncpu
     CPU_structure(i_PAR_structure)%ROLE(2)="d"
     CPU_structure(i_PAR_structure)%CPU(2)=1
   case("Self_Energy") 
      call GIMME_a_parallel_proposal(3,(/" b","qp"," q"/))
   case("Response_T_space") 
      call GIMME_a_parallel_proposal(2,(/"eh"," k"/))
 end select
 !
 ! Reset the active structures
 !-----------------------------
 call PARALLEL_set_the_active_structures(PARALLEL_n_structures_active)
 !
 ! Dump
 !------
 CPU_structure(i_PAR_structure)%CPU_string =trim(intc( PARALLEL_CPU_used(1) ))
 CPU_structure(i_PAR_structure)%ROLE_string=trim(PARALLEL_CPU_role(1))
 do i_p=2,PARALLEL_n_structures_active
   CPU_structure(i_PAR_structure)%CPU_string =trim(CPU_structure(i_PAR_structure)%CPU_string)//"."//&
&                                             trim(intc( PARALLEL_CPU_used(i_p) ))
   CPU_structure(i_PAR_structure)%ROLE_string=trim(CPU_structure(i_PAR_structure)%ROLE_string)//"."//&
&                                             trim(PARALLEL_CPU_role(i_p))
 enddo
 !
 ! Save that this ENVIRONMENT uses defaults to repeat the default procedure
 !
 PARALLEL_ENV_uses_default(i_PAR_structure)=.TRUE.
 !
 ! Ok?
 !-----
 if (product(PARALLEL_CPU_used(:PARALLEL_n_structures_active))==ncpu) return
 !
 call error('Impossible to define an appropriate parallel structure')
 !
 contains
   !
   subroutine GIMME_a_parallel_proposal(N_fields,FIELDS)
     !
     use stderr,      ONLY:STRING_remove
     use vec_operate, ONLY:sort
     use parallel_m,  ONLY:PAR_QP_range,PAR_n_bands,PAR_n_c_bands,PAR_n_v_bands,&
&                          PAR_EH_range,PAR_K_range,PAR_n_freqs,PAR_n_G_vectors,&
&                          PARALLEL_default_mode,master_cpu,PAR_Q_ibz_range,PAR_Q_bz_range
     !
     ! "c" -> PAR_n_c_bands(2)-PAR_n_c_bands(1)+1
     ! "v" -> PAR_n_v_bands(2)-PAR_n_v_bands(1)+1
     ! "k" -> PAR_K_range
     ! "b" -> PAR_n_bands(2)-PAR_n_bands(1)+1
     ! "q" -> PAR_Q_ibz_range(2)- PAR_Q_ibz_range(1)+1
     ! "qp"-> PAR_QP_range
     ! "eh"-> PAR_EH_range
     ! "w" -> PAR_n_freqs
     ! "g" -> PAR_n_G_vectors
     !
     integer      :: N_fields,i_f,VALS(N_fields),VALS_sorted(N_fields),&
&                    MY_cpu(N_fields),i_v,INDEX_sorted(N_fields),i_x,i_v_ref,iv(N_fields)
     character(*) :: FIELDS(N_fields)
     character(3) :: ch
     logical      :: CONDITION
     !
     VALS  =0
     MY_cpu=1
     NC    =ncpu
     CPU_structure(i_PAR_structure)%N_chains=N_fields
     !
     do i_f=1,N_fields
       ch=STRING_remove(FIELDS(i_f)," ")
       CPU_structure(i_PAR_structure)%ROLE(i_f)=trim(ch)
       if (trim(ch)=="g" ) VALS(i_f)=PAR_n_G_vectors
       if (trim(ch)=="c" ) VALS(i_f)=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1
       if (trim(ch)=="v" ) VALS(i_f)=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1
       if (trim(ch)=="b" ) VALS(i_f)=PAR_n_bands(2)-PAR_n_bands(1)+1
       if (trim(ch)=="k" ) VALS(i_f)=PAR_K_range
       if (trim(ch)=="q" ) then
         !
         if (N_fields==5.or.N_fields==3) VALS(i_f)=PAR_Q_ibz_range(2)-PAR_Q_ibz_range(1)+1
         if (N_fields==4)                VALS(i_f)=PAR_Q_bz_range(2)-PAR_Q_bz_range(1)+1
         !
       endif
       if (trim(ch)=="qp") VALS(i_f)=PAR_QP_range
       if (trim(ch)=="eh") VALS(i_f)=PAR_EH_range
       if (trim(ch)=="w ") VALS(i_f)=PAR_n_freqs
     enddo
     !
     VALS_sorted=VALS
     !
     if (trim(PARALLEL_default_mode)=="workload") then
       ! ...sort
       call sort(VALS_sorted,indx=INDEX_sorted)
       ! ...and re order
       iv=INDEX_sorted
       do i_v=1,N_fields
         VALS_sorted (N_fields-i_v+1) =VALS( iv(i_v) )
         INDEX_sorted(N_fields-i_v+1) =iv(i_v)
       enddo 
     else
       do i_v=1,N_fields
         INDEX_sorted(i_v) = i_v
       enddo 
       !
     endif
     !
     sc_loop: do while(product(MY_cpu)/=ncpu)
       do i_v=1,N_fields
         !
         i_x=INDEX_sorted(i_v)
         !
         CONDITION=.TRUE.
         if (trim(PARALLEL_default_mode)=="workload") CONDITION=VALS_sorted(i_v)>=maxval(VALS_sorted)
         !
         do while (CONDITION)
           !
           call PARALLEL_global_defaults__define_the_basis(NC,BASIS)
           !
           if (all(VALS_sorted<BASIS)) exit sc_loop
           if (BASIS>VALS_sorted(i_v)) exit
           !
           MY_cpu(i_x)=MY_cpu(i_x)*BASIS
           VALS_sorted(i_v)=VALS(i_x)/MY_cpu(i_x)
           NC=NC/BASIS
           !
           if (all(VALS_sorted==1)) exit sc_loop
           if (product(MY_cpu)==ncpu) exit sc_loop
           !
           if (trim(PARALLEL_default_mode)=="workload")  CONDITION=VALS_sorted(i_v)>=maxval(VALS_sorted)
           if (trim(PARALLEL_default_mode)=="balanced")  CONDITION=.FALSE.
           if (trim(PARALLEL_default_mode)=="memory")    CONDITION=.TRUE.
           !
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
end subroutine
!
subroutine PARALLEL_global_defaults__define_the_basis(NC,BASIS)
  implicit none
  integer, intent(in) :: NC
  integer, intent(out):: BASIS
  integer :: i1,i3
  !
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
