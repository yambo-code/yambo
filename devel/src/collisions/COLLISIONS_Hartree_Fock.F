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
subroutine COLLISIONS_Hartree_Fock(E,k,q)
 !
 ! Hartree-Fock
 !
 use pars,          ONLY:SP,schlen,DP,pi,cZERO
 use IO_m,          ONLY:io_COLLs
 use com,           ONLY:msg
 use LIVE_t,        ONLY:live_timing
 use electrons,     ONLY:levels,spin_occ,spin
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_QP,&
&                        PAR_IND_Q,PAR_QP_index,&
&                        PAR_n_Bp_mat_elements,PAR_COM_QP_A2A,&
&                        PAR_IND_Bp_mat,PAR_nQP,PAR_nQ,PAR_COM_G_b_A2A,PAR_COM_Q_index
 use collision,     ONLY:ggwinfo,collision_reset,COLLISIONS_parallel_SC_structure,&
&                        HF_collisions,nHF_collisions
 use QP_m,          ONLY:QP_ng_Sx,QP_table,QP_nb,QP_n_states
 use R_lattice,     ONLY:qindx_S,bz_samp
 use SC,            ONLY:SC_bands
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 use fragments,     ONLY:fragment_status
 use real_time,     ONLY:B_mat_index
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use interfaces,    ONLY:WF_load
 use wrapper,       ONLY:V_dot_V
#if defined _OPENMP
 use wrapper,       ONLY:V_dot_V_omp
#endif
 !
 implicit none
 type(levels)             ::E       
 type(bz_samp)            ::k,q
 !
 !Work Space
 !
 type(ggwinfo)           ::isc,iscp
 integer                 ::iq,ib,ibp,jb,ik,i_qp,jb2,ik_minus_q,ig,i_qp_mem
 complex(DP)             ::DP_Sx
 complex(SP),allocatable ::local_rhotw(:)
 character(schlen)       ::ch
 !
 ! Resets & basis setup
 !
 call collision_reset(isc)
 call collision_reset(iscp) 
 !
 ! COLLISIONS 
 !
 call msg('r', '[EXS] Plane waves :',QP_ng_Sx)
 !
! if ((l_gw0.or.l_cohsex).and.WF_buffered_IO) then
!   !
!   call WF_load(WF_buffer,0,1,(/1,n_WF_bands_to_load/),(/1,k%nibz/),space='B',title='-BUFFER',force_WFo=.true.)
!   !
! endif
 !
 call WF_load(WF,QP_ng_Sx,maxval(qindx_S(:,:,2)),(/1,max(E%nbm,QP_nb)/),(/1,k%nibz/),title='-HF Collisions')
 !
#if defined _TIMING
 call timing('HF Collisions',OPR='start')
#endif
 !
 ! allocation again
 !
 allocate(isc%gamp(QP_ng_Sx,1),isc%rhotw(QP_ng_Sx),iscp%rhotw(QP_ng_Sx),local_rhotw(QP_ng_Sx))
 isc%qs    =1
 isc%ngrho =QP_ng_Sx
 iscp%ngrho=QP_ng_Sx
 !
 call scatterGamp(isc,'x')
 !
 call live_timing("HF collisions",PAR_nQP*PAR_nQ*PAR_n_Bp_mat_elements)
 !
 do i_qp=1,QP_n_states
   !
   if (.not.PAR_IND_QP%element_1D(i_qp)) cycle
   !
   i_qp_mem=PAR_QP_index(i_qp)
   !
   if (io_COLLs) then
     if (fragment_status(i_qp)/=-1) then
       call live_timing(steps=PAR_nQ*PAR_n_Bp_mat_elements)
       cycle
     endif
   endif
   !
   ib =QP_table(i_qp,1)
   ibp=QP_table(i_qp,2)
   !
   HF_collisions(1)%v3c=cZERO
   !
   do iq=1,q%nbz
     !
     if (.not.PAR_IND_Q%element_1D(iq)) cycle
     !
     isc%qs(2:)=(/q%sstar(iq,1),q%sstar(iq,2)/)
     if (isc%qs(2)/=isc%iqref) call scatterGamp(isc,'x')
     !
     !  (n,k,sp_n).     
     !              |
     !              | (m,p,r,sp_m)
     !              |
     !              |
     !  (m,k,sp_m).     
     !
     ! n   =QP_table(i_qp,1)
     ! m   =QP_table(i_qp,2)
     ! k   =QP_table(i_qp,3)
     !
     isc%is=(/QP_table(i_qp,1),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     isc%os(2:)=(/k%sstar(qindx_S(isc%is(2),iq,1),:),spin(QP_table(i_qp,:))/)
     iscp%is=(/QP_table(i_qp,2),QP_table(i_qp,3),1,spin(QP_table(i_qp,:))/)
     !
     isc%qs(1)=qindx_S(QP_table(i_qp,3),iq,2)
     iscp%qs=isc%qs
     !
     ik_minus_q=isc%os(2)
     !
     do jb=SC_bands(1),SC_bands(2)
       !
       isc%os(1)=jb
       iscp%os=isc%os
       !
       call scatterBamp(isc)
       !
       ! COLLISIONS(n,n',qp_n_states) = \sum_{G,q} ( rho(m,n,G,q,k) x rho*(m',n',G,q,k) /|q+G|^2 )
       !
       do jb2=SC_bands(1),SC_bands(2)
         !
         if ( .not. PAR_IND_Bp_mat%element_1D( B_mat_index(jb,jb2) ) ) cycle
         !      
         iscp%os(1)=jb2
         if (any(isc%is/=iscp%is).or.any(isc%os/=iscp%os)) then
           call scatterBamp(iscp)
         else
           iscp%rhotw=isc%rhotw
         endif
         !
#if defined _OPENMP
         !
!$omp parallel do default(shared), private(ig)
         do ig = 1, isc%ngrho
           local_rhotw(ig) = -4./spin_occ*pi*isc%rhotw(ig)*conjg(iscp%rhotw(ig))
         enddo
!$omp end parallel do
         !
         DP_Sx=V_dot_V_omp(QP_ng_Sx,local_rhotw,isc%gamp(:,1))
#else
         !
         local_rhotw=-4./spin_occ*pi*isc%rhotw*conjg(iscp%rhotw)
         !
         DP_Sx=V_dot_V(QP_ng_Sx,local_rhotw,isc%gamp(:,1))
         !
#endif
         !
         HF_collisions(1)%v3c(jb,jb2,ik_minus_q)=HF_collisions(1)%v3c(jb,jb2,ik_minus_q)+DP_Sx
         !
         call live_timing(steps=1)
         !
       enddo
       !
     enddo
     !
   enddo
   !
   if (COLLISIONS_parallel_SC_structure) then
     call PP_redux_wait(HF_collisions(1)%v3c,COMM=PAR_COM_QP_A2A%COMM)
     call PP_redux_wait(HF_collisions(1)%v3c,COMM=PAR_COM_Q_index%COMM)
   else
     call PP_redux_wait(HF_collisions(1)%v3c,COMM=PAR_COM_G_b_A2A%COMM)
   endif
   !
   call COLLISIONS_linearize_and_IO("HF",i_qp,HF_collisions,nHF_collisions)
   !
 enddo
 !
 call live_timing()
 !
 deallocate(isc%gamp,isc%rhotw,iscp%rhotw,local_rhotw)
 !
 call collision_reset(isc)
 call collision_reset(iscp) 
 !
#if defined _TIMING
 call timing('HF Collisions',OPR='stop')
#endif
 !
end subroutine
