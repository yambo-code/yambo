!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_ELPH_initialize(k,q)
 ! 
 use pars,           ONLY:SP,pi
 use real_time,      ONLY:q_weight,RT_q_px,elph_gkkp_expanded,THETA_matrix,&
&                         GKKP_matrix_index,THETA_matrix_index,Q_io_map,K_io_map,&
&                         l_ELPH_CCA_Kernel 
 use drivers,        ONLY:l_ph_corr,l_carrier_dynamics
 use R_lattice,      ONLY:RL_vol,bz_samp
 use par_indexes_m,  ONLY:par_indexes
 use par_proc_m,     ONLY:pp_indexes_reset,myid
 use IO_m,           ONLY:io_control,OP_RD,REP,RD_CL_IF_END
 use ELPH,           ONLY:ph_freqs_sq,elph_gkkp,ph_modes,elph_nb
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::iq,ID,io_err,ik,i_counter,alloc_err,iq_io,il,i_s,i_n,i_np
 real(SP)            ::RADIUS_sphere,ph_Esq_local(q%nbz,ph_modes)
 integer, external   ::ioELPH
 logical             ::l_Full_RT_T_evolution
 !
 ! Logicals
 !==========
 !
 if (.not.l_ph_corr) return
 !
 call parser('RT_T_evol',l_Full_RT_T_evolution)
 l_ELPH_CCA_Kernel=.not.l_Full_RT_T_evolution
 !
 ! Q/K grid remapping
 !====================
 !
 call RT_ELPH_gkkp_remap(k,q)
 !
 ! Allocation
 !============
 !
 allocate(elph_gkkp_expanded(k%nbz*RT_q_px%n_of_elements(myid+1),ph_modes,elph_nb,elph_nb),stat=alloc_err)
 call mem_est("GKKP_expanded",(/size(elph_gkkp_expanded)/),(/2*SP/),errors=(/alloc_err/))
 !
 if (.not.l_ELPH_CCA_Kernel) then
   allocate(THETA_matrix(2*q%nbz*ph_modes,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1,QP_nk))
   call mem_est("THETA_matrix",(/size(THETA_matrix)/),(/2*SP/))
   THETA_matrix=(0._SP,0._SP)
   allocate(THETA_matrix_index(q%nbz,ph_modes,2))
 endif
 !
 allocate(GKKP_matrix_index(q%nbz))
 !
 ! I/O
 !=====
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp_expanded')
 !
 i_counter=0
 !
 do iq=1,q%nbz
   !
   iq_io=Q_io_map(iq)
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq_io+1/),ID=ID)
   io_err=ioELPH(ID,'gkkp_expanded')
   !
   ph_Esq_local(iq,:)=ph_freqs_sq(iq_io,:)
   !
   if (.not.RT_q_px%element_1D(iq)) cycle
   !
   i_counter=i_counter+1
   !
   GKKP_matrix_index(iq)=i_counter
   !
   do ik=1,k%nbz
     !
     do i_n=1,elph_nb
       do i_np=1,elph_nb
         !
         ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
         ! where the band indexes have been exchanged. Instead of doing a global change of the notes
         ! and of the code I decided, here, to exchange the indexes:
         !
         elph_gkkp_expanded((i_counter-1)*k%nbz+ik,:,i_n,i_np)=elph_gkkp(K_io_map(ik),:,i_np,i_n)
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 ph_freqs_sq=ph_Esq_local
 !
 ! RIM
 !=====
 !
 allocate(q_weight(q%nbz))
 !
 call k_ibz2bz(q,'i',.TRUE.)  
 !
 RADIUS_sphere=(3.*RL_vol/q%nbz/4./pi)**(1./3.)
 !
 call rim_spherical(q%nbz,q%ptbz,q_weight,RADIUS_sphere,2,.FALSE.)
 !
 ! THETA matrix indexes
 !======================
 !
 if (l_ELPH_CCA_Kernel) return
 !
 i_counter=0
 !
 do iq=1,q%nbz
   do il=1,ph_modes
     do i_s=1,2
       !
       i_counter=i_counter+1
       !
       THETA_matrix_index(iq,il,i_s)=i_counter
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_initialize
