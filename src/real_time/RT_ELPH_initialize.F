!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
 use real_time,      ONLY:q_weight,elph_gkkp_expanded,THETA_matrix,&
&                         THETA_matrix_index,Q_io_map,K_io_map,&
&                         l_RT_CCA_Kernel,l_Debye_TBose_update
 use drivers,        ONLY:l_ph_corr
 use com,            ONLY:warning
 use R_lattice,      ONLY:RL_vol,bz_samp
 use IO_m,           ONLY:io_control,REP,OP_RD_CL
 use ELPH,           ONLY:ph_freqs_sq,elph_gkkp,ph_modes,elph_nb,W_debye
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use parser_m,       ONLY:parser
 use parallel_m,     ONLY:PAR_Q_index,PAR_IND_Q,PAR_nQ,PAR_Xk_nibz,PAR_Xk_ibz_index,PAR_IND_Xk_ibz
 !
 implicit none
 !
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::iq,ID,io_err,ik,i_counter,alloc_err,iq_io,il,i_s,i_n,i_np,ik_bz,ik_mem
 real(SP)            ::RADIUS_sphere,ph_Esq_local(q%nbz,ph_modes)
 integer, external   ::ioELPH
 logical             ::l_use_Debye_E
 !
 ! Logicals
 !==========
 !
 if (.not.l_ph_corr) return
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp_expanded')
 !
 if (io_err/=0) then
   l_ph_corr=.FALSE.
   call warning('ELPH database not found. E-P scattering switched off')
   return
 endif
 !
 call parser('UseDebyeE'   , l_use_Debye_E)
 call parser('UpdtDebyeT', l_Debye_TBose_update)
 !
 ! Q/K grid remapping
 !====================
 !
 call RT_ELPH_gkkp_remap(k,q)
 !
 ! Allocation
 !============
 !
 allocate(elph_gkkp_expanded(PAR_Xk_nibz*PAR_nQ,ph_modes,elph_nb,elph_nb),stat=alloc_err)
 call mem_est("GKKP_expanded",(/size(elph_gkkp_expanded)/),(/2*SP/),errors=(/alloc_err/))
 !
 if (.not.l_RT_CCA_Kernel) then
   allocate(THETA_matrix(2*q%nbz*ph_modes,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1,QP_nk))
   call mem_est("THETA_matrix",(/size(THETA_matrix)/),(/2*SP/))
   THETA_matrix=(0._SP,0._SP)
   allocate(THETA_matrix_index(q%nbz,ph_modes,2))
 endif
 !
 ! I/O
 !=====
 !
 i_counter=0
 !
 do iq=1,q%nbz
   !
   if (.not.PAR_IND_Q%element_1D(iq)) cycle
   !
   iq_io=Q_io_map(iq)
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/iq_io+1/),ID=ID)
   io_err=ioELPH(ID,'gkkp_expanded')
   !
   ph_Esq_local(iq,:)=ph_freqs_sq(iq_io,:)
   !
   do ik=1,k%nibz
     !
     ik_bz=sum(k%nstar(:ik-1))+1
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     ik_mem=PAR_Xk_ibz_index(ik)
     !
     do i_n=1,elph_nb
       do i_np=1,elph_nb
         !
         ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
         ! where the band indexes have been exchanged. Instead of doing a global change of the notes
         ! and of the code I decided, here, to exchange the indexes:
         !
         elph_gkkp_expanded((PAR_Q_index(iq)-1)*PAR_Xk_nibz+ik_mem,:,i_n,i_np)=elph_gkkp(K_io_map(ik_bz),:,i_np,i_n)
         !
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 ph_freqs_sq=ph_Esq_local
 !
 ! Test of the effect of using a single optical mode
 !
 if (l_use_Debye_E) ph_freqs_sq=W_debye**2.
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
 if (l_RT_CCA_Kernel) return
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
