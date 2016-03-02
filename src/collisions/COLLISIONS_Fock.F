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
subroutine COLLISIONS_Fock(E,k,q)
 !
 ! Fock Collisions
 !
 use pars,          ONLY:SP,pi,cZERO
 use IO_m,          ONLY:io_COLLs
 use com,           ONLY:msg
 use LIVE_t,        ONLY:live_timing
 use electrons,     ONLY:levels,spin_occ,spin
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,&
&                        PAR_IND_Bp_mat,PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_index
 use collision,     ONLY:elemental_collision,elemental_collision_free,COLLISIONS_parallel_SC_structure,&
&                        FOCK_COLL_element,&
&                        elemental_collision_alloc,COLL_bands,FOCK_collisions
 use QP_m,          ONLY:QP_ng_Sx
 use R_lattice,     ONLY:qindx_S,bz_samp
 use hamiltonian,   ONLY:B_mat_index
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use interfaces,    ONLY:WF_load
 !
 implicit none
 !
 type(levels)             ::E
 type(bz_samp)            ::k,q
 !
 ! Work Space
 !
 integer                 :: i_coll,ig1,iqbz,iqibz,iqs,&
&                           i_n,i_k,i_k_s,i_sp,i_m,i_mp,N_g_vecs,&
&                           i_np,i_kmq,i_kmq_bz,i_kmq_s,i_kmq_G0,PAR_NC_local
 !
 type(elemental_collision):: isc,iscp
 complex(SP)              :: pre_factor
 !
#if defined _TIMING
 call timing('Fock Collisions',OPR='start')
#endif
 !
 ! Reset
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 !
 ! Dimensions
 !
 N_g_vecs  =QP_ng_Sx
 !
 call msg('r', '[EXS] Plane waves :',QP_ng_Sx)
 !
 ! WFs
 !
 if (WF_buffered_IO) then
   !
   call WF_load(WF_buffer,0,1,COLL_bands,(/1,k%nibz/),space='B',title='-BUFFER')
   !
 endif
 !
 call WF_load(WF,N_g_vecs,maxval(qindx_S(:,:,2)),COLL_bands,(/1,k%nibz/),title='-Fock Collisions')
 !
 isc%qs    =1
 isc%ngrho =N_g_vecs
 iscp%ngrho=N_g_vecs
 !
 ! Allocation
 !
 call elemental_collision_alloc(isc, NG=N_g_vecs,NG_GAMP=(/N_g_vecs,1/),TITLE="FOCK")
 call elemental_collision_alloc(iscp,NG=N_g_vecs,TITLE="FOCK")
 !
 call scatter_Gamp(isc,'x')
 !
 ! Parallel Indexes & LIVE-TIMING STEPS
 !
 PAR_NC_local=FOCK_collisions%PAR_N
 if (associated(FOCK_collisions%IO_status)) then
   PAR_NC_local=count(FOCK_collisions%IO_status==-1)
 endif
 call live_timing("Fock collisions",PAR_NC_local*PAR_nQ*PAR_n_Bp_mat_elements)
 !
 do i_coll=1,FOCK_collisions%N
   !
   if (.not.FOCK_collisions%PAR_IND%element_1D(i_coll)) cycle
   !
   if (io_COLLs) then
     if (FOCK_collisions%IO_status(i_coll)/=-1) cycle
   endif
   !
   i_n     = FOCK_collisions%state(i_coll,1)
   i_m     = FOCK_collisions%state(i_coll,2)
   i_k     = FOCK_collisions%state(i_coll,3)
   i_sp    = FOCK_collisions%state(i_coll,4)
   !
   FOCK_COLL_element(1)%v3_c=cZERO
   !
   do iqbz=1,q%nbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     !
     iqibz    =q%sstar(iqbz,1)
     iqs      =q%sstar(iqbz,2)
     !
     isc%qs   =(/0,iqibz,iqs/)
     iscp%qs  =(/0,iqibz,iqs/)
     !
     if (isc%qs(2)/=isc%iqref) call scatter_Gamp(isc,'x')
     !
     ! Scattering geometry (I)
     !=========================
     !
     !       (n,k,sp).     
     !               | m
     !               |   (q,r,sp)
     !               |
     !               | m'
     !    (n',kmq,sp).     
     !
     i_k_s   = 1
     !
     i_kmq_bz =qindx_S(i_k,iqbz,1)
     i_kmq_G0 =qindx_S(i_k,iqbz,2)
     !
     i_kmq   = k%sstar(i_kmq_bz,1)
     i_kmq_s = k%sstar(i_kmq_bz,2)
     !
     isc%is  =(/i_n,i_k,  i_k_s,  i_sp/)
     isc%os  =(/0  ,i_kmq,i_kmq_s,i_sp/)
     !
     iscp%is =(/i_m,i_k,  i_k_s,  i_sp/)
     iscp%os =(/0,  i_kmq,i_kmq_s,i_sp/)
     !
     isc%qs(1) =i_kmq_G0
     iscp%qs(1)=i_kmq_G0
     !
     do i_mp=COLL_bands(1),COLL_bands(2)
       !
       iscp%os(1)=i_mp
       !
       call scatter_Bamp(iscp)
       !
       do i_np=COLL_bands(1),COLL_bands(2)
         !
         ! COLLISIONS(n,n',qp_n_states) = \sum_{G,q} ( rho(m,n,G,q,k) x rho*(m',n',G,q,k) /|q+G|^2 )
         !
         if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(i_np,i_mp,COLL_bands) ) ) cycle
         !
         isc%os(1)=i_np
         !
         if (any(isc%is/=iscp%is).or.any(isc%os/=iscp%os)) then
           call scatter_Bamp(isc)
         else
           isc%rhotw=iscp%rhotw
         endif
         !
         pre_factor=cZERO
         !
         ! Remember isc%gamp=iscp%gamp
         !
!$omp parallel do default(shared), private(ig1), reduction(+:pre_factor)
         do ig1=1,N_g_vecs
           pre_factor=pre_factor+isc%rhotw(ig1)*isc%gamp(ig1,1)*conjg(iscp%rhotw(ig1))
         enddo
!$omp end parallel do
         !
         FOCK_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)=FOCK_COLL_element(1)%v3_c(i_np,i_mp,i_kmq)-4._SP/spin_occ*pi*pre_factor
         !
         call live_timing(steps=1)
         !
       enddo ! loop on the band i_np
       !
     enddo ! loop on the band i_mp
     !
   enddo ! loop on iqbz
   !
   if (COLLISIONS_parallel_SC_structure) then
     call PP_redux_wait(FOCK_COLL_element(1)%v3_c,COMM=PAR_COM_QP_A2A%COMM)
     call PP_redux_wait(FOCK_COLL_element(1)%v3_c,COMM=PAR_COM_Q_index%COMM)
   else
     call PP_redux_wait(FOCK_COLL_element(1)%v3_c,COMM=PAR_COM_G_b_A2A%COMM)
   endif
   !
   call COLLISIONS_linearize_and_IO("Fock",i_coll,FOCK_collisions,FOCK_COLL_element)
   !
 enddo ! loop on i_coll
 !
 call live_timing()
 !
 ! CLEAN
 !
 call elemental_collision_free(isc)
 call elemental_collision_free(iscp)
 !
#if defined _TIMING
 call timing('Fock Collisions',OPR='stop')
#endif
 !
end subroutine
