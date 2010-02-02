!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine K_scattering(iq,ik1,ik2,Xk,q)
 !
 ! This routine evaluates the dimension of the
 ! scattering oscillators and, if allocated, it actually
 ! calculates them.
 !
 !  <n k1_bz |exp iG.r|m k2_bz> = <n k1_ibz s1 |exp iG.r|m k2_ibz s2>  =
 !                                <n k1_ibz |exp iG.r|m k2_ibz s1^{-1} x s2 > 
 !
 ! now, in general, s1^{-1} x s2 is not a symmetry in the star of k2_ibz, so we
 ! define and use the table ik_is_table:
 !
 ! is_in_the_star_of_k2=ik_is_table(k2,s1^{-1} x s2 )
 !
 ! to get
 !
 !  <n k1_bz |exp iG.r|m k2_bz> =  <n k1_ibz |exp iG.r|m k2_ibz  is_in_the_star_of_k2>*PHASE
 !
 ! where |k2 ib s1^{-1} x s2 > = PHASE * |k2 ib is_in_the_star_of_k2>
 !
 use pars,          ONLY:SP,IP
 use memory_m,      ONLY:mem_est
 use wave_func,     ONLY:wf_state,wf
 use FFT_m,         ONLY:fft_rot_r
 use D_lattice,     ONLY:nsym,sop_inv,sop_tab
 use R_lattice,     ONLY:b,qindx_B,bz_samp,ik_is_table
 use BS,            ONLY:O_n_scatt,O_table,O_phase,BS_O,O_n_c_states,O_c_state,&
&                        O_n_v_states,O_v_state,O_ng,BS_bands,&
&                        BS_res_K_corr,BS_cpl_K_corr
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use collision,     ONLY:ggwinfo,collision_reset
 use electrons,     ONLY:n_sp_pol
 implicit none
 !
 type(bz_samp) ::Xk,q
 integer       ::ik1,ik2,iq
 !
 ! Work Space
 !
 type(ggwinfo)   ::isc
 type(pp_indexes)::p_index
 integer     :: i1,i2,i3,i4,b_ref,ik1bz,ik2bz,ifft,is1,is2,is1xis2,&
&               iss_tab,is_coll,ib1,ib2,i_spin2,i_spin1
 integer     :: scatt_stat(BS_bands(2)-BS_bands(1)+1,BS_bands(2)-BS_bands(1)+1,nsym,n_sp_pol)
 complex(SP) :: wf_cmp
 logical     :: eval_collision
 !
 if (.not.BS_res_K_corr) return 
 !
 call pp_indexes_reset(p_index)
 call collision_reset(isc)
 !
 ! K/S Table
 !
 call k_sym2sym(Xk,'s')
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
 if (allocated(BS_O))then
   allocate(isc%rhotw(O_ng))
   BS_O=(0.,0.)
 endif
 call pp_redux_wait
 !
1 continue
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 !i_spin1=1
 !i_spin2=1
 ! GPL_INCLUDE_END SPIN_IGNORE
 !
 O_table=0
 scatt_stat=0
 O_n_scatt=0
 !
 do i1=1,Xk%nstar(ik1)
   !
   ik1bz=sum(Xk%nstar(:ik1-1))+i1
   is1=Xk%star(ik1,i1)
   !
   ! GPL_EXCLUDE_START SPIN_IGNORE
   !
   do i_spin1=1,n_sp_pol
     !
     ! GPL_EXCLUDE_END SPIN_IGNORE
     !
     do i2=1,Xk%nstar(ik2)
       !
       ik2bz=sum(Xk%nstar(:ik2-1))+i2
       is2  =Xk%star(ik2,i2)
       is1xis2=sop_tab(sop_inv(is1),is2)
       iss_tab=ik_is_table(ik2,is1xis2)
       !
       ! Phase search : |k ib is > =? PH |k ib iss_tab>
       !
       if (is1xis2==iss_tab) O_phase(is1xis2,:,:)=1
       if (is1xis2/=iss_tab.and..not.eval_collision) then
         !
         ! GPL_EXCLUDE_START SPIN_IGNORE
         !
         do i_spin2=1,n_sp_pol
           !
           ! GPL_EXCLUDE_END SPIN_IGNORE
           !
           do ib1=BS_bands(1),BS_bands(2)
             !
             if (O_phase(is1xis2,ib1,i_spin2)/=0) cycle
             ifft=wf_state(ib1,ik2,i_spin2)
             !
             wf_cmp=dot_product(wf(fft_rot_r(is1xis2,:),ifft),wf(fft_rot_r(iss_tab,:),ifft))
             if (abs(abs(wf_cmp)-1.)<=1.E-3) then
               O_phase(is1xis2,ib1,i_spin2)=nint(real(wf_cmp))
             else
               O_phase(is1xis2,ib1,i_spin2)=-99
             endif
           enddo
           !
           ! GPL_EXCLUDE_START SPIN_IGNORE
           !
         enddo
         !
         ! GPL_EXCLUDE_END SPIN_IGNORE
         !
       endif
       !
       ! NO Q simmetry and/or Go shift here
       !
       isc%qs=(/1, q%sstar( qindx_B(ik1bz,ik2bz,1) ,1) ,1/) 
       !
       ! <ib1 ik1bz | ib2 ik2bz>
       !
       do i3=1,O_n_v_states(ik1bz,i_spin1)+O_n_c_states(ik1bz,i_spin1)
         !
         if (i3<=O_n_v_states(ik1bz,i_spin1)) ib1=O_v_state(ik1bz,i3,i_spin1)
         if (i3> O_n_v_states(ik1bz,i_spin1)) ib1=O_c_state(ik1bz,i3-O_n_v_states(ik1bz,i_spin1),i_spin1)
         !
         do i4=1,O_n_v_states(ik2bz,i_spin1)+O_n_c_states(ik2bz,i_spin1)
           !
           if (i4<=O_n_v_states(ik2bz,i_spin1)) ib2=O_v_state(ik2bz,i4,i_spin1)
           if (i4> O_n_v_states(ik2bz,i_spin1)) ib2=O_c_state(ik2bz,i4-O_n_v_states(ik2bz,i_spin1),i_spin1)
           !
           ! Prevent the calculation of c->v or v->c oscillators in the resonant
           ! only calculation
           !
           if (.not.BS_cpl_K_corr.and. (&
&              (i3> O_n_v_states(ik1bz,i_spin1).and.i4<=O_n_v_states(ik2bz,i_spin1)).or.&
&              (i3<=O_n_v_states(ik1bz,i_spin1).and.i4> O_n_v_states(ik2bz,i_spin1)))) cycle
           !
           is_coll=iss_tab
           !
           if (O_phase(is1xis2,ib2,i_spin1)==-99) is_coll=is1xis2
           if(scatt_stat(ib1-b_ref,ib2-b_ref,is_coll,i_spin1)==0) then
             !
             O_n_scatt=O_n_scatt+1
             O_table(ib1-b_ref,is1,ib2-b_ref,is2,i_spin1)   =O_n_scatt
             scatt_stat(ib1-b_ref,ib2-b_ref,is_coll,i_spin1)=O_n_scatt
             if (.not.allocated(BS_O)) cycle
             if (.not.eval_collision) cycle
             if (.not.p_index%element_1D(O_n_scatt)) cycle
             !
             isc%is=(/ib1,ik1,1,i_spin1/)
             isc%os=(/ib2,ik2,is_coll,i_spin1/)
             call scatterBamp(isc)
             BS_O(:,O_n_scatt)=isc%rhotw 
             !
           else
             !
             O_table(ib1-b_ref,is1,ib2-b_ref,is2,i_spin1)=scatt_stat(ib1-b_ref,ib2-b_ref,is_coll,i_spin1)
             !
           endif
         enddo
       enddo
       !
     enddo
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
   enddo
   !
   ! GPL_EXCLUDE_END SPIN_IGNORE
   !
 enddo
 !
 if (allocated(BS_O))then
   if (.not.eval_collision) then
     eval_collision=.true.
     call par_indexes(p_index,(/O_n_scatt/))
     goto 1
   endif
   do i1=1,O_n_scatt
     call pp_redux_wait(BS_O(:,i1))
   enddo
   call pp_indexes_reset(p_index)
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
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
     do i_spin1=1,n_sp_pol
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
       if (O_phase(i1,i2,i_spin1)==-99.or.O_phase(i1,i2,i_spin1)==0) O_phase(i1,i2,i_spin1)=1
       ! 
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
     enddo
     !
     ! GPL_EXCLUDE_END SPIN_IGNORE
     !
   enddo
 enddo
 !
end subroutine
