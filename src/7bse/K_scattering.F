!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine K_scattering(iq,ik1,ik2,Xk,q)
 !
 use pars,          ONLY:SP,IP
 use memory_m,      ONLY:mem_est
 use wave_func,     ONLY:wf_state,wf
 use FFT_m,         ONLY:fft_rot_r
 use com,           ONLY:msg
 use D_lattice,     ONLY:nsym,sop_inv,sop_tab
 use R_lattice,     ONLY:b,qindx_B,qindx_X,rl_sop,bz_samp,ik_is_table
 use BS,            ONLY:O_n_cc_scatt,O_table,O_phase,O_cc,O_vv,&
&                        O_n_vv_scatt,O_n_c_states,O_c_state,&
&                        O_n_v_states,O_v_state,O_ng,BS_bands,BS_res_K_corr
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use electrons,     ONLY:n_sp_pol
 implicit none
 type(bz_samp) ::Xk,q
 integer       ::ik1,ik2,iq
 !
 ! Work Space
 !
 type(ggwinfo)   ::isc
 type(pp_indexes)::pcc,pvv
 integer     :: i1,i2,i3,i4,b_ref,ik1bz,ik2bz,ifft,is1,is2,is1xis2,&
&               iss_tab,is_coll,ib,iv1,iv2,ic1,ic2,i_sp2,i_sp1
 integer     :: cc_stat(BS_bands(2)-BS_bands(1)+1,BS_bands(2)-BS_bands(1)+1,nsym,n_sp_pol)
 integer     :: vv_stat(BS_bands(2)-BS_bands(1)+1,BS_bands(2)-BS_bands(1)+1,nsym,n_sp_pol)
 real(SP)    :: v(3)
 complex(SP) :: wf1,wf2,wf_cmp
 logical     :: eval_collision
 !
 if (.not.BS_res_K_corr) return 
 !
 call pp_indexes_reset(pcc)
 call pp_indexes_reset(pvv)
 call collision_reset(isc)
 !
 ! K/S Table
 !
 call k_sym2sym(Xk,'s')
 !
 !================================
 !
 b_ref=BS_bands(1)-1
 if (.not.allocated(O_phase)) then
   allocate(O_phase(nsym,BS_bands(2),n_sp_pol))
   call mem_est("O_phase",(/size(O_phase)/),(/IP/))
 endif
 O_phase=0
 isc%ngrho=O_ng
 !
 eval_collision=.false.
 !
 if (allocated(O_cc))then
   allocate(isc%rhotw(O_ng))
   O_vv=(0.,0.)
   O_cc=(0.,0.)
 endif
 call pp_redux_wait
 !
1 continue
 !
 O_table=0
 cc_stat=0
 vv_stat=0
 O_n_cc_scatt=0
 O_n_vv_scatt=0
 !
 do i1=1,Xk%nstar(ik1)
   ik1bz=sum(Xk%nstar(:ik1-1))+i1
   is1=Xk%star(ik1,i1)
   !
   do i_sp1=1,n_sp_pol
     !
     do i2=1,Xk%nstar(ik2)
       ik2bz=sum(Xk%nstar(:ik2-1))+i2
       is2=Xk%star(ik2,i2)
       is1xis2=sop_tab(sop_inv(is1),is2)
       iss_tab=ik_is_table(ik2,is1xis2)
       !
       ! Phase search : |k ib is > =? PH |k ib iss_tab>
       !
       if (is1xis2==iss_tab) O_phase(is1xis2,:,:)=1
       if (is1xis2/=iss_tab.and..not.eval_collision) then
         !
         do i_sp2=1,n_sp_pol
           !
           do ib=BS_bands(1),BS_bands(2)
             !
             if (O_phase(is1xis2,ib,i_sp2)/=0) cycle
             ifft=wf_state(ib,ik2,i_sp2)
             !
             wf_cmp=dot_product(wf(fft_rot_r(is1xis2,:),ifft),wf(fft_rot_r(iss_tab,:),ifft))
             if (abs(abs(wf_cmp)-1.)<=1.E-3) then
               !
               O_phase(is1xis2,ib,i_sp2)=nint(real(wf_cmp))
             else
               O_phase(is1xis2,ib,i_sp2)=-99
             endif
           enddo
         enddo
       endif
       !
       ! NO Q simmetry and/or Go shift here
       !
       isc%qs=(/1, q%sstar( qindx_B(ik1bz,ik2bz,1) ,1) ,1/) 
       !
       ! <ic1 ik1bz | ic2 ik2bz>
       !
       do i3=1,O_n_c_states(ik1bz,i_sp1)
         do i4=1,O_n_c_states(ik2bz,i_sp1)
           ic1=O_c_state(ik1bz,i3,i_sp1)
           ic2=O_c_state(ik2bz,i4,i_sp1)
           is_coll=iss_tab
           !
           if (O_phase(is1xis2,ic2,i_sp1)==-99) is_coll=is1xis2
           if(cc_stat(ic1-b_ref,ic2-b_ref,is_coll,i_sp1)==0) then
             !
             O_n_cc_scatt=O_n_cc_scatt+1
             O_table(ic1-b_ref,is1,ic2-b_ref,is2,i_sp1)=O_n_cc_scatt
             cc_stat(ic1-b_ref,ic2-b_ref,is_coll,i_sp1)=O_n_cc_scatt
             isc%is=(/ic1,ik1,1,i_sp1/)
             isc%os=(/ic2,ik2,is_coll,i_sp1/)
             if (.not.allocated(O_cc)) cycle
             if (.not.eval_collision) cycle
             if (.not.pcc%i1p(O_n_cc_scatt)) cycle
             call scatterBamp(isc)
             O_cc(:,O_n_cc_scatt)=isc%rhotw
           else
             O_table(ic1-b_ref,is1,ic2-b_ref,is2,i_sp1)=cc_stat(ic1-b_ref,ic2-b_ref,is_coll,i_sp1)
           endif
         enddo
       enddo
       !
       ! <iv1 ik1bz | iv2 ik2bz>
       !
       do i3=1,O_n_v_states(ik1bz,i_sp1)
         do i4=1,O_n_v_states(ik2bz,i_sp1)
           iv1=O_v_state(ik1bz,i3,i_sp1)
           iv2=O_v_state(ik2bz,i4,i_sp1)
           is_coll=iss_tab
           !
           if (O_phase(is1xis2,iv2,i_sp1)==-99) is_coll=is1xis2
           if(vv_stat(iv1-b_ref,iv2-b_ref,is_coll,i_sp1)==0) then
             !
             O_n_vv_scatt=O_n_vv_scatt+1
             !
             O_table(iv1-b_ref,is1,iv2-b_ref,is2,i_sp1)=O_n_vv_scatt
             vv_stat(iv1-b_ref,iv2-b_ref,is_coll,i_sp1)=O_n_vv_scatt
             isc%is=(/iv1,ik1,1,i_sp1/)
             isc%os=(/iv2,ik2,is_coll,i_sp1/)
             !
             if (.not.allocated(O_vv)) cycle
             if (.not.eval_collision) cycle
             if (.not.pvv%i1p(O_n_vv_scatt)) cycle
             call scatterBamp(isc)
             O_vv(:,O_n_vv_scatt)=isc%rhotw
           else
             O_table(iv1-b_ref,is1,iv2-b_ref,is2,i_sp1)=vv_stat(iv1-b_ref,iv2-b_ref,is_coll,i_sp1)
           endif
         enddo
       enddo
     enddo
   enddo
 enddo
 !
 if (allocated(O_cc))then
   if (.not.eval_collision) then
     eval_collision=.true.
     call par_indexes(pvv,(/O_n_vv_scatt/))
     call par_indexes(pcc,(/O_n_cc_scatt/))
     goto 1
   endif
   do i1=1,O_n_vv_scatt
     call pp_redux_wait(O_vv(:,i1))
   enddo
   do i1=1,O_n_cc_scatt
     call pp_redux_wait(O_cc(:,i1))
   enddo
   call pp_indexes_reset(pcc)
   call pp_indexes_reset(pvv)
   call collision_reset(isc)
 else
   return
 endif
 !
 ! When O_phase is -99 the phase does not exist so that it must be set
 ! to 0
 !
 do i1=1,nsym
  do i2=1,BS_bands(2)
    do i_sp1=1,n_sp_pol
      if (O_phase(i1,i2,i_sp1)==-99.or.O_phase(i1,i2,i_sp1)==0) O_phase(i1,i2,i_sp1)=1
    enddo
  enddo
 enddo
 !
end subroutine
