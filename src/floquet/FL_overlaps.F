!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_overlaps(en,Xk,i_sp,ikbz,FL_V_bands,FL_S_plus,FL_S_minus,FL_time_S_det,l_eval_det)
 !
 ! Build Floquet overlaps between valence bands
 ! Structure taken from Build_Overlaps_det_NEQ
 !
 use pars,           ONLY:SP,DP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use hamiltonian,    ONLY:H_ref_bands
 use R_lattice,      ONLY:k_map,bz_samp
 use vec_operate,    ONLY:k_periodic_idx
 use DIPOLES,        ONLY:DIP_S
 use wrapper_omp,    ONLY:M_by_V_omp,mv_cgemv_omp
 use wrapper,        ONLY:Vstar_dot_V
 use QP_m,           ONLY:QP_nk
 use electric,       ONLY:l_force_SndOrd
 use fl_optics,      ONLY:max_fl_mode,max_extra_mode,FL_space_dim,n_tot_fl_modes,n_tot_extra_modes,&
&                         n_tot_time_steps
 !
 implicit none
 !
 integer,      intent(in) :: ikbz
 integer,      intent(in) :: i_sp
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: Xk
 complex(SP),  intent(in) :: FL_V_bands(H_ref_bands(2),n_tot_fl_modes,en%nbf(i_sp),QP_nk)
 logical,      intent(in) :: l_eval_det
 complex(SP), intent(inout) :: FL_S_plus(en%nbf(i_sp),en%nbf(i_sp),n_tot_extra_modes,6)
 complex(SP), intent(inout) :: FL_S_minus(en%nbf(i_sp),en%nbf(i_sp),n_tot_extra_modes,6)
 complex(DP), intent(inout) :: FL_time_S_det(n_tot_time_steps,6)
 !
 ! Work Space
 !
 integer     :: idx(3),id,ikp,ikm,i1,i2
 integer     :: ikm_ibz,ikp_ibz,ik_ibz,max_step,istep
 complex(SP) :: tmp_det,vtmp(H_ref_bands(2))
 complex(SP) :: vtmp2(en%nbf(i_sp))
 integer     :: i1_fl,i2_fl,i1_shifted_fl,i2_shifted_fl,i_sum_shifted,i_sum_fl,i1_inverted_fl,i1_extra,i2_extra
 character(4) :: filename
 !
 FL_S_plus =cZERO
 FL_S_minus=cZERO
 FL_time_S_det=cZERO
 !
 ! I split the construction of the FL_S_minus(Sm1_plus) matrix in four parts
 !
 !   1:H_ref_bands(1)-1   H_ref_bands(1):en%nbf(i_sp)
 !  _____________________________________
 !  |                 |                  |
 !  |     head        |      wing        |     1:H_ref_bands(1)-1
 !  |                 |                  |
 !  |                 |                  |
 !  |------------------------------------|
 !  |                 |                  | 
 !  |    wing         |      core        |   H_ref_bands(1):en%nbf(i_sp)
 !  |                 |                  |
 !  |                 |                  |
 !  ------------------------------------- 
 !
 ! 
 !  The head part is time-independent, the wings depends only from
 !  one of the time-dependent valence bands, while the core part depends from
 !  both the wave-functions at k and k-delta
 !
 !
 do id=1,3
   !
   ik_ibz=Xk%sstar(ikbz,1) !ikbz belongs to full BZ
   !
   if(k_map%max_kdir(id)/=1) then ! periodic directions
     !
     max_step=1
     if(k_map%max_kdir(id)>=6.and..not.l_force_SndOrd) max_step=2
     !
     do istep=1,max_step
       !
       ! Overlap S(k, k-dk)
       !
       idx=k_map%k_map_inv(ikbz,:)
       idx(id)=idx(id)-istep
       idx=k_periodic_idx(idx,k_map)
       ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
       !
       ! FL_S_minus(l,m; k, k - delta) = \sum_{i,j} V*(l,i; k) S*(i,j; k-delta, k) V(m,j ;k - delta)
       !
       ikm_ibz=Xk%sstar(ikm,1)
       !
       ! Matrix head
       !
       do i2=1,H_ref_bands(1)-1
         do i1=1,H_ref_bands(1)-1
           i1_extra = max_extra_mode+1 
           !I replaced an i1_fl loop + "if /=0 cycle" by just assigning the value
           FL_S_minus(i1,i2,i1_extra,id+(istep-1)*3)=conjg(DIP_S(i2,i1,id+(istep-1)*3,ikm,i_sp))
         enddo
       enddo
       !
       ! Matrix wings
       !
       ! wing 2 is included in the calculation of the core
       !
       do i2=1,H_ref_bands(1)-1 ! wing 1
         do i1=H_ref_bands(1),en%nbf(i_sp)
           do i1_fl=1,n_tot_fl_modes
             i1_inverted_fl =  (2*max_fl_mode+1) + 1 - i1_fl
             i1_extra = i1_fl + max_extra_mode - max_fl_mode !added extra mode
             FL_S_minus(i1,i2,i1_extra,id+(istep-1)*3)=& !assign to extra mode
                           &  Vstar_dot_V(H_ref_bands(2),&
                           &FL_V_bands(:,i1_inverted_fl,i1,ik_ibz),&
                           &conjg(DIP_S(i2,:,id+(istep-1)*3,ikm,i_sp)))
           enddo
         enddo
       enddo
       !
       ! Matrix core
       !
       do i2=H_ref_bands(1),en%nbf(i_sp)
         do i1_extra=1,n_tot_extra_modes
           i1_fl = i1_extra - max_extra_mode + max_fl_mode ! added this
           i1_shifted_fl = i1_fl - (max_fl_mode+1) !this needs no change
           do i2_fl=1,n_tot_fl_modes
             i2_shifted_fl = i2_fl - (max_fl_mode+1)
             i_sum_shifted = (i1_shifted_fl+i2_shifted_fl)
             if (abs(i_sum_shifted) > max_fl_mode) cycle
             i_sum_fl = i_sum_shifted + (max_fl_mode+1)
             !
             call M_by_V_omp('C',H_ref_bands(2),DIP_S(:,:,id+(istep-1)*3,ikm,i_sp),FL_V_bands(:,i_sum_fl,i2,ikm_ibz),vtmp)
             !
             call mv_cgemv_omp('C',H_ref_bands(2),en%nbf(i_sp),cONE,FL_V_bands(:,i2_fl,:,ik_ibz),&
&                    H_ref_bands(2),vtmp,1,cZERO,vtmp2,1)
             !
             FL_S_minus(:,i2,i1_extra,id+(istep-1)*3) = FL_S_minus(:,i2,i1_extra,id+(istep-1)*3) + vtmp2
           enddo
         enddo
       enddo
       !
       !
       idx=k_map%k_map_inv(ikbz,:)
       idx(id)=idx(id)+istep
       idx=k_periodic_idx(idx,k_map)
       ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))  
       !
       ! FL_S_plus(l,m; k, k + delta) = \sum_{i,j} V*(l,i; k) S(i,j; k-delta, k) V(m,j ;k + delta)
       !
       ikp_ibz=Xk%sstar(ikp,1)
       !
       ! Matrix head
       !
       do i1=1,H_ref_bands(1)-1
         do i2=1,H_ref_bands(1)-1
           i1_extra = max_extra_mode+1 
           !I replaced an i1_fl loop + "if /=0 cycle" by just assigning the value
           FL_S_plus(i1,i2,i1_extra,id+(istep-1)*3)=DIP_S(i1,i2,id+(istep-1)*3,ikbz,i_sp)
         enddo
       enddo
       !
       ! Matrix wings
       !
       ! wing 1 is included in the calculation of the core
       !
       do i2=1,H_ref_bands(1)-1 ! wing 1
         do i1=H_ref_bands(1),en%nbf(i_sp)
           do i1_fl=1,n_tot_fl_modes
             i1_inverted_fl =  (2*max_fl_mode+1) + 1 - i1_fl
             i1_extra = i1_fl + max_extra_mode - max_fl_mode !added extra mode
             FL_S_plus(i1,i2,i1_extra,id+(istep-1)*3)=& !assign to extra mode
             &  Vstar_dot_V(H_ref_bands(2),&
                           &FL_V_bands(:,i1_inverted_fl,i1,ik_ibz),&
                           &DIP_S(:,i2,id+(istep-1)*3,ikbz,i_sp))
           enddo
         enddo
       enddo
       !
       ! Matrix Core
       !
       do i2=H_ref_bands(1),en%nbf(i_sp)
         do i1_extra=1,n_tot_extra_modes
            i1_fl = i1_extra - max_extra_mode + max_fl_mode
            i1_shifted_fl = i1_fl - (max_fl_mode+1)
            do i2_fl=1,n_tot_fl_modes
              i2_shifted_fl = i2_fl - (max_fl_mode+1)
              i_sum_shifted = (i1_shifted_fl+i2_shifted_fl)
              if (abs(i_sum_shifted) > max_fl_mode) cycle
              i_sum_fl = i_sum_shifted + (max_fl_mode+1)
              !
              call M_by_V_omp('N',H_ref_bands(2),DIP_S(:,:,id+(istep-1)*3,ikbz,i_sp),FL_V_bands(:,i_sum_fl,i2,ikp_ibz),vtmp)
              !
              call mv_cgemv_omp('C',H_ref_bands(2),en%nbf(i_sp),cONE,FL_V_bands(:,i2_fl,:,ik_ibz),&
&                      H_ref_bands(2),vtmp,1,cZERO,vtmp2,1)
              !
              FL_S_plus(:,i2,i1_extra,id+(istep-1)*3) = FL_S_plus(:,i2,i1_extra,id+(istep-1)*3) + vtmp2
            enddo
         enddo
       enddo
       !
       !
       if (l_eval_det) then !aim137 I can add (.and. istep == 1) here. And make FL_time_S_det(3), not 6.
         !
         call FL_determinants(en,ik_ibz,ikp_ibz,i_sp,FL_S_plus(:,:,:,id+(istep-1)*3),FL_time_S_det(:,id+(istep-1)*3))
         !
       endif
       !
     enddo  
     !
   endif
   !
 enddo
 !
end subroutine FL_overlaps
