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
subroutine ELPH_databases_load(GKKP_sq,E,k,q,n_bands,mode)
 ! 
 ! mode can be combination of
 !
 !  BZ/IBZ + bare/dressed + square 
 ! 
 use pars,           ONLY:SP,rZERO
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,nqbz
 use IO_m,           ONLY:io_control,OP_RD_CL
 use ELPH,           ONLY:PH_freqs_sq,GKKP,ph_modes,MAX_gkkp,Q_io_map,GKKP_me,K_io_map
 use interfaces,     ONLY:ELPH_alloc
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_Q_bz_index,PAR_IND_Q_bz,PAR_nQ_bz,PAR_Xk_nibz,PAR_Xk_ibz_index,PAR_IND_Xk_ibz,&
&                         PAR_COM_Q_INDEX,PAR_COM_Xk_ibz_INDEX,PARs_PH_Q_ibz,PARs_PH_Q_ibz
 !
#include<memory.h>
 ! 
 integer,      intent(in)    :: n_bands(2)
 type(levels), intent(in)    :: E
 type(bz_samp),intent(in)    :: k,q
 character(*), intent(in)    :: mode
 type(GKKP_me),intent(inout) :: GKKP_sq
 !
 ! Work Space
 !
 integer             ::iq_bz,ID,io_err,ik,iq_io,i_n,i_np,ik_bz,ik_mem,iq_mem,NK,NQ,NB2sym(2)
 real(SP)            ::ph_Esq_local(nqbz,ph_modes)
 real(SP),allocatable::MAX_gkkp_expanded_at_kpq(:,:),GKKP_to_sym(:,:,:)
 integer, external   ::io_ELPH
 character(1)        ::sym_mode
 logical             ::l_K_BZ,l_K_IBZ,l_Q_BZ,l_Q_IBZ,l_bare,l_dressed,l_mixed
 !
 ! What?
 !=======
 if (index(mode,"K-sym")>0) sym_mode="K"
 if (index(mode,"Q-sym")>0) sym_mode="Q"
 l_K_IBZ   = index(mode,"K-IBZ")>0
 l_K_BZ    = .not.l_K_IBZ
 l_Q_IBZ   = index(mode,"Q-IBZ")>0
 l_Q_BZ    = .not.l_Q_IBZ
 l_bare    = index(mode,"bare-bare")>0
 l_dressed = index(mode,"dressed-dressed")>0
 l_mixed   = index(mode,"dressed-bare")>0.or.index(mode,"bare-dressed")>0
 !
 ! Allocation
 !============
 NB2sym=(/1,elph_nb/)
 !DEBUG_AM>
 NB2sym=n_bands
 !DEBUG_AM<
 YAMBO_ALLOC(MAX_gkkp_expanded_at_kpq,(k%nbz,nqbz))
 YAMBO_ALLOC(GKKP_to_sym,(ph_modes,NB2sym(2),NB2sym(2)))
 if (l_Q_IBZ) NQ=PARs_PH_Q_ibz%N_par
 if (l_Q_BZ ) NQ=PAR_nQ_bz
 if (l_K_IBZ) NK=PAR_Xk_nibz
 if (l_K_BZ ) NK=k%nbz
 call ELPH_alloc("dV_sq",GKKP=GKKP_sq,Nk_mem=NK,Nq_mem=NQ,NB=n_bands)
 MAX_gkkp_expanded_at_kpq=rZERO
 ph_Esq_local=rZERO
 !
 ! Degeneration Finder & I/O
 !===========================
 do iq_bz=1,nqbz
   !
   if (l_Q_BZ.and..not.PAR_IND_Q_bz%element_1D(iq_bz)) cycle
   if (l_Q_IBZ) then
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
     if (l_K_IBZ.and.k%sstar(ik_bz,2)/=1) cycle
     !
     ik=k%sstar(ik_bz,1)
     !
     if (l_K_IBZ) then
       if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
       iq_mem = PAR_Q_bz_index(iq_bz)
       ik_mem = PAR_Xk_ibz_index(ik)
     else if (l_K_BZ) then
       ik_mem = ik_bz
       iq_mem = PARs_PH_Q_ibz%table( q%sstar(iq_bz,1))
       !if (.not.PAR_IND_Xk_bz%element_1D(ik)) cycle
       !ik_mem = PAR_Xk_ibz_index(ik)
     endif
     !
     do i_n=1,NB2sym(2)
       do i_np=1,NB2sym(2)
         !
         ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
         ! where the band indexes have been exchanged. Instead of doing a global change of the notes
         ! and of the code I decided, here, to exchange the indexes:
         !
         GKKP_to_sym(:,i_n,i_np)=abs(GKKP%dVc(:,i_np,i_n,K_io_map(ik_bz),1))**2
         !
       enddo
     enddo
     !
     call ELPH_databases_symmetrize(sym_mode,k,E,ik_bz,iq_bz,NB2sym,GKKP_to_sym)
     !
     GKKP_sq%dVr(:,:,:,ik_mem,iq_mem)=GKKP_to_sym(:,n_bands(1):n_bands(2),n_bands(1):n_bands(2))
     !
     MAX_gkkp_expanded_at_kpq(ik_bz,iq_bz)=maxval(sqrt(GKKP_sq%dVr(:,:,:,ik_mem,iq_mem)))
     !
   enddo
   !
 enddo
 !
 PH_freqs_sq=ph_Esq_local
 !
 if (l_Q_BZ ) then
   call PP_redux_wait(PH_freqs_sq,COMM=PAR_COM_Q_INDEX%COMM)
   call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PAR_COM_Q_INDEX%COMM)
   call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 else
   call PP_redux_wait(PH_freqs_sq,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
   call PP_redux_wait(MAX_gkkp_expanded_at_kpq,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 endif
 !
 if (any(PH_freqs_sq<0._SP)) call warning('Imaginary phonon frequencies found. Please check.')
 !
 MAX_gkkp=maxval(MAX_gkkp_expanded_at_kpq)
 !
 call ELPH_alloc("FREE",GKKP=GKKP)
 !
end subroutine ELPH_databases_load
