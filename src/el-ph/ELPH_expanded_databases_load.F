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
subroutine ELPH_expanded_databases_load(E,k,q,n_bands,mode)
 ! 
 ! mode can be combination of
 !
 !  BZ/IBZ + bare/dressed + square 
 ! 
 use pars,           ONLY:SP,rZERO
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S,nqbz
 use IO_m,           ONLY:io_control,OP_RD_CL
 use ELPH,           ONLY:PH_freqs_sq,GKKP,ph_modes,elph_nb,MAX_gkkp,GKKP_sq,GKKP_pe_sq,Q_io_map
 use vec_operate,    ONLY:degeneration_finder
 use interfaces,     ONLY:ELPH_alloc
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_Q_bz_index,PAR_IND_Q_bz,PAR_nQ_bz,PAR_Xk_nibz,PAR_Xk_ibz_index,PAR_IND_Xk_ibz,&
&                         PAR_COM_Q_INDEX,PAR_COM_Xk_ibz_INDEX,PARs_PH_Q_ibz,PARs_PH_Q_ibz
 !
#include<memory.h>
 ! 
 integer,      intent(in) :: n_bands(2)
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 character(*), intent(in) :: mode
 !
 ! Work Space
 integer             ::iq_bz,ID,io_err,ik,i_counter,iq_io,i_n,i_np,ik_bz,ik_mem,ik_m_q,&
&                      first_el(2,n_bands(2)),n_of_el(2,n_bands(2)),n_deg_grp(2),i_g,i_c,iq_mem
 real(SP)            ::gkkp_average(ph_modes),gkkp_expanded_sq(ph_modes,elph_nb,elph_nb),ph_Esq_local(q%nbz,ph_modes)
 real(SP),allocatable::MAX_gkkp_expanded_at_kpq(:,:)
 integer, external   ::io_ELPH
 !
 ! Control
 logical             ::l_BZ,l_IBZ,l_bare,l_dressed
 !
 ! What?
 !=======
 l_IBZ     = index(mode,"IBZ")>0
 l_BZ      = .not.l_IBZ
 l_bare    = index(mode,"bare")>0
 l_dressed = index(mode,"dressed")>0.or..not.l_bare
 !
 ! Allocation
 !============
 YAMBO_ALLOC(MAX_gkkp_expanded_at_kpq,(k%nbz,nqbz))
 if (l_BZ ) call ELPH_alloc("dV_sq",GKKP=GKKP_sq,Nk_mem=PAR_Xk_nibz,Nq_mem=PAR_nQ_bz)
 if (l_IBZ) call ELPH_alloc("dV_sq",GKKP=GKKP_pe_sq,Nk_mem=k%nbz,Nq_mem=PARs_PH_Q_ibz%N_par)
 MAX_gkkp_expanded_at_kpq=rZERO
 ph_Esq_local=rZERO
 !
 ! Degeneration Finder & I/O
 !===========================
 i_counter   =0
 !
 do iq_bz=1,nqbz
   !
   if (l_BZ) then
     if (.not.PAR_IND_Q_bz%element_1D(iq_bz)) cycle
   else
     if (q%sstar(iq_bz,2)/=1) cycle
     if (.not.PARs_PH_Q_ibz%IND%element_1D(q%sstar(iq_bz,1))) cycle
   endif
   !
   iq_io=Q_io_map(iq_bz)
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/iq_io+1/),ID=ID)
   io_err=io_ELPH(ID,'gkkp_expanded')
   !
   ph_Esq_local(iq_bz,:)=PH_freqs_sq(iq_io,:)
   !
   do ik_bz=1,k%nbz
     !
     if (l_BZ.and.k%sstar(ik_bz,2)/=1) cycle
     !
     ik       =k%sstar(ik_bz,1)
     ik_m_q   =k%sstar(qindx_S(ik,iq_bz,1),1)
     !
     if (l_BZ ) then
       if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
       iq_mem = PAR_Q_bz_index(iq_bz)
       ik_mem = PAR_Xk_ibz_index(ik)
     else if (l_IBZ) then
       ik_mem = ik_bz
       iq_mem = PARs_PH_Q_ibz%table( q%sstar(iq_bz,1))
       !if (.not.PAR_IND_Xk_bz%element_1D(ik)) cycle
       !ik_mem = PAR_Xk_ibz_index(ik)
     endif
     !
     do i_n=1,elph_nb
       do i_np=1,elph_nb
         !
         ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
         ! where the band indexes have been exchanged. Instead of doing a global change of the notes
         ! and of the code I decided, here, to exchange the indexes:
         !
         gkkp_expanded_sq(:,i_n,i_np)=abs(GKKP%dVc(:,i_np,i_n,ik_bz,1))**2
         !
       enddo
     enddo
     !
     ! Feb 2014. Here I symmetrize the gkkp m.e. with respect to denerated bands. This procedure is not
     ! needed in the case the Double Grid is not used. But when the Double Grid is used gkkp m.e. corresponding to transitions between degenerate
     ! states can be exported to non-degenerate bands belonging to a k of the small grid. In this way
     ! the detailed balance condition is not satisfied anymore.
     !
     ! Degeneration Finder
     !====================
     call degeneration_finder(E%E(:n_bands(2),ik,1),    n_bands(2),first_el(1,:),n_of_el(1,:),n_deg_grp(1),0.0001/HA2EV)
     call degeneration_finder(E%E(:n_bands(2),ik_m_q,1),n_bands(2),first_el(2,:),n_of_el(2,:),n_deg_grp(2),0.0001/HA2EV)
     !
     do i_g=1,n_deg_grp(1)
       do i_np=n_bands(1),n_bands(2)
         do i_c=1,2
           if (i_c==1) gkkp_average=0._SP
           do i_n=first_el(1,i_g),first_el(1,i_g)+n_of_el(1,i_g)-1
             if (i_c==1) gkkp_average(:)=gkkp_average(:)+gkkp_expanded_sq(:,i_n,i_np)
             if (i_c==2) gkkp_expanded_sq(:,i_n,i_np)=gkkp_average(:)/real(n_of_el(1,i_g))
           enddo
         enddo
       enddo
     enddo
     do i_g=1,n_deg_grp(2)
       do i_n=n_bands(1),n_bands(2)
         do i_c=1,2
           if (i_c==1) gkkp_average=0._SP
           do i_np=first_el(2,i_g),first_el(2,i_g)+n_of_el(2,i_g)-1
             if (i_c==1) gkkp_average(:)=gkkp_average(:)+gkkp_expanded_sq(:,i_n,i_np)
             if (i_c==2) gkkp_expanded_sq(:,i_n,i_np)=gkkp_average(:)/real(n_of_el(2,i_g))
           enddo
         enddo
       enddo
     enddo
     !
     MAX_gkkp_expanded_at_kpq(ik_bz,iq_bz)=maxval(sqrt(gkkp_expanded_sq(:,:,:)))
     !
     if (l_BZ ) GKKP_sq%dVr(:,:,:,ik_mem,iq_mem)   =gkkp_expanded_sq(:,:,:)
     if (l_IBZ) GKKP_pe_sq%dVr(:,:,:,ik_mem,iq_mem)=gkkp_expanded_sq(:,:,:)
     !
   enddo
   !
 enddo
 !
 PH_freqs_sq=ph_Esq_local
 !
 if (l_BZ ) then
   call PP_redux_wait(PH_freqs_sq,COMM=PAR_COM_Q_INDEX%COMM)
   call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PAR_COM_Q_INDEX%COMM)
   call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 else
   call PP_redux_wait(PH_freqs_sq,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
   call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 endif
 !
 MAX_gkkp=maxval(MAX_gkkp_expanded_at_kpq)
 !
 if (any(PH_freqs_sq<0._SP)) call warning('Imaginary phonon frequencies found. Please check.')
 !
 YAMBO_FREE(MAX_gkkp_expanded_at_kpq)
 !
end subroutine ELPH_expanded_databases_load
