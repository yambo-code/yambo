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
subroutine ELPH_expanded_databases_map(E,k)
 ! 
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV
 use real_time,      ONLY:elph_gkkp_expanded_sq,Q_io_map,K_io_map,RT_bands
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S,nqbz
 use IO_m,           ONLY:io_control,OP_RD_CL
 use ELPH,           ONLY:ph_freqs_sq,elph_gkkp,ph_modes,elph_nb,MAX_gkkp
 use vec_operate,    ONLY:degeneration_finder
 use parallel_m,     ONLY:PAR_Q_bz_index,PAR_IND_Q_bz,PAR_nQ_bz,PAR_Xk_nibz,PAR_Xk_ibz_index,PAR_IND_Xk_ibz,&
&                         PP_redux_wait,PAR_COM_Q_INDEX,PAR_COM_Xk_ibz_INDEX
 !
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k
 !
 ! Work Space
 !
 integer             ::iq,ID,io_err,ik,i_counter,iq_io,i_n,i_np,ik_bz,ik_mem,ik_m_q,&
&                      first_el(2,RT_bands(2)),n_of_el(2,RT_bands(2)),n_deg_grp(2),i1,i2,iq_mem
 real(SP)            ::ph_Esq_local(nqbz,ph_modes),gkkp_average(ph_modes),MAX_gkkp_expanded_at_kpq(k%nibz,nqbz)
 integer, external   ::io_ELPH
 !
 ! Allocation
 !============
 !
 YAMBO_ALLOC(elph_gkkp_expanded_sq,(elph_nb,elph_nb,ph_modes,PAR_nQ_bz,PAR_Xk_nibz))
 !
 MAX_gkkp_expanded_at_kpq=0._SP
 !
 ! Degeneration Finder & I/O
 !===========================
 !
 i_counter   =0
 ph_Esq_local=0._SP
 !
 do iq=1,nqbz
   !
   if (.not.PAR_IND_Q_bz%element_1D(iq)) cycle
   !
   iq_io=Q_io_map(iq)
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/iq_io+1/),ID=ID)
   io_err=io_ELPH(ID,'gkkp_expanded')
   !
   ph_Esq_local(iq,:)=ph_freqs_sq(iq_io,:)
   !
   do ik=1,k%nibz
     !
     ik_bz    =sum(k%nstar(:ik-1))+1
     ik_m_q   =k%sstar(qindx_S(ik,iq,1),1)
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     ik_mem = PAR_Xk_ibz_index(ik)
     iq_mem = PAR_Q_bz_index(iq)
     !
     do i_n=1,elph_nb
       do i_np=1,elph_nb
         !
         ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
         ! where the band indexes have been exchanged. Instead of doing a global change of the notes
         ! and of the code I decided, here, to exchange the indexes:
         !
         elph_gkkp_expanded_sq(i_n,i_np,:,iq_mem,ik_mem)=abs(elph_gkkp(:,i_np,i_n,K_io_map(ik_bz)))**2
         !
       enddo
     enddo
     !
     ! Feb 2014. Here I symmetrize the gkkp m.e. with respect to denerated bands. This procedure is not
     ! needed in the case the Double Grid is not used. But when the Double Grid is used gkkp m.e. corresponding to transitions between degenerate
     ! states can be exported to non-degenerate bands belonging to a k of the small grid. In this way
     ! the detailled balance condition is not satisfied anymore.
     !
     ! Degeneration Finder
     !====================
     call degeneration_finder(E%E(:RT_bands(2),ik,1),    RT_bands(2),first_el(1,:),n_of_el(1,:),n_deg_grp(1),0.0001/HA2EV)
     call degeneration_finder(E%E(:RT_bands(2),ik_m_q,1),RT_bands(2),first_el(2,:),n_of_el(2,:),n_deg_grp(2),0.0001/HA2EV)
     !
     do i1=1,n_deg_grp(1)
       do i_np=RT_bands(1),RT_bands(2)
         do i2=1,2
           if (i2==1) gkkp_average=0._SP
           do i_n=first_el(1,i1),first_el(1,i1)+n_of_el(1,i1)-1
             if (i2==1) gkkp_average(:)=gkkp_average(:)+elph_gkkp_expanded_sq(i_n,i_np,:,iq_mem,ik_mem)
             if (i2==2) elph_gkkp_expanded_sq(i_n,i_np,:,iq_mem,ik_mem)=gkkp_average(:)/real(n_of_el(1,i1))
           enddo
         enddo
       enddo
     enddo
     do i1=1,n_deg_grp(2)
       do i_n=RT_bands(1),RT_bands(2)
         do i2=1,2
           if (i2==1) gkkp_average=0._SP
           do i_np=first_el(2,i1),first_el(2,i1)+n_of_el(2,i1)-1
             if (i2==1) gkkp_average(:)=gkkp_average(:)+elph_gkkp_expanded_sq(i_n,i_np,:,iq_mem,ik_mem)
             if (i2==2) elph_gkkp_expanded_sq(i_n,i_np,:,iq_mem,ik_mem)=gkkp_average(:)/real(n_of_el(2,i1))
           enddo
         enddo
       enddo
     enddo
     !
     MAX_gkkp_expanded_at_kpq(ik,iq)=maxval(sqrt(elph_gkkp_expanded_sq(:,:,:,iq_mem,ik_mem)))
     !
   enddo
   !
 enddo
 !
 ph_freqs_sq=ph_Esq_local
 !
 call PP_redux_wait(ph_freqs_sq,COMM=PAR_COM_Q_INDEX%COMM)
 call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PAR_COM_Q_INDEX%COMM)
 call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 MAX_gkkp=maxval(MAX_gkkp_expanded_at_kpq)
 !
 if (any(ph_freqs_sq<0._SP)) call warning('Imaginary phonon frequencies found. Please check.')
 !
end subroutine ELPH_expanded_databases_map
