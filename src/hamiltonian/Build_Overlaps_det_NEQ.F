!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine Build_overlaps_det_NEQ(en,Xk,i_sp,ikbz,V_bands,Sm1_plus,Sm1_minus,S_det)
 !
 ! Build overlaps determinants between valence bands
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels
 use hamiltonian,    ONLY:H_ref_bands
 use R_lattice,      ONLY:k_map,bz_samp
 use vec_operate,    ONLY:k_periodic_idx
 use DIPOLES,        ONLY:DIP_S
 use wrapper,        ONLY:Vstar_dot_V,M_by_V,mv_cgemv
 use QP_m,           ONLY:QP_nk
 use electric,       ONLY:l_force_SndOrd
 !
 implicit none
 !
 integer,      intent(in) :: ikbz
 integer,      intent(in) :: i_sp
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: Xk
 complex(SP),  intent(in) :: V_bands(H_ref_bands(2),en%nbf(i_sp),QP_nk)
 complex(SP), intent(out) :: S_det(6)
 complex(SP), intent(out) :: Sm1_plus (en%nbf(i_sp),en%nbf(i_sp),6)
 complex(SP), intent(out) :: Sm1_minus(en%nbf(i_sp),en%nbf(i_sp),6)
 !
 ! Work Space
 !
 integer     :: idx(3),id,ikp,ikm,i1,i2
 integer     :: ikm_ibz,ikp_ibz,ik_ibz,max_step,istep
 !complex(SP) :: V_tmp(H_ref_bands(2),en%nbf(i_sp)) ! used in full construction of overlaps
 complex(SP) :: tmp_det,vtmp(H_ref_bands(2))
 !
!$omp parallel do default(shared), private(id)
 do id=1,6
   S_det    (id)    =cZERO
   Sm1_plus (:,:,id)=cZERO
   Sm1_minus(:,:,id)=cZERO
 enddo
!$omp end parallel do
 !
 ! I split the construction of the Sm1_minus(Sm1_plus) matrix in four parts
 !
 !   1:H_ref_bands(1)-1   H_ref_bands(1):H_ref_bands(2)
 !  _____________________________________
 !  |                 |                  |
 !  |     head        |      wing        |     1:H_ref_bands(1)-1
 !  |                 |                  |
 !  |                 |                  |
 !  |------------------------------------|
 !  |                 |                  | 
 !  |    wing         |      core        |   H_ref_bands(1):H_ref_bands(2)
 !  |                 |                  |
 !  |                 |                  |
 !  ------------------------------------- 
 !
 ! 
 !  The haed part is time-independent, the wings depends only from
 !  one of the time-dependent valence bands, while the core part depends from
 !  both the wave-functions at k and k-delta
 !
 ik_ibz=Xk%sstar(ikbz,1)
 !
 do id=1,3
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
       ! Sm1_minus(l,m; k, k - delta) = \sum_{i,j} V*(l,i; k) S*(i,j; k-delta, k) V(m,j ;k - delta)
       !
       ikm_ibz=Xk%sstar(ikm,1)
       !
       ! Full construction of the Sm1_minus matrix
       !
       !DEBUG>
       !
!       call M_by_M_omp('C','N',H_ref_bands(2),en%nbf(i_sp),H_ref_bands(2),cONE,DIP_S(:,:,id+(istep-1)*3,ikm,i_sp),&
!&                          H_ref_bands(2),V_bands(:,:,ikm_ibz),H_ref_bands(2),cZERO,V_tmp,H_ref_bands(2))
       !
!       call M_by_M_omp('C','N',en%nbf(i_sp),en%nbf(i_sp),H_ref_bands(2),cONE,V_bands(:,:,ik_ibz),&
!&                      H_ref_bands(2),V_tmp,H_ref_bands(2),cZERO,Sm1_minus(:,:,id+(istep-1)*3),en%nbf(i_sp))
       !
       !DEBUG<
       !
       ! Matrix head
       !
!$omp parallel do default(shared), private(i1,i2), collapse(2) 
       do i2=1,H_ref_bands(1)-1
         do i1=1,H_ref_bands(1)-1
           Sm1_minus(i1,i2,id+(istep-1)*3)=conjg(DIP_S(i2,i1,id+(istep-1)*3,ikm,i_sp))
         enddo
       enddo
!$omp end parallel do
       !
       ! Matrix wings
       !
!$omp parallel do default(shared), private(i1,i2), collapse(2)
       do i2=1,H_ref_bands(1)-1
         do i1=H_ref_bands(1),en%nbf(i_sp)
           Sm1_minus(i1,i2,id+(istep-1)*3)=Vstar_dot_V(H_ref_bands(2),V_bands(:,i1,ik_ibz), &
&                conjg(DIP_S(i2,:,id+(istep-1)*3,ikm,i_sp)))
          enddo
       enddo
!$omp end parallel do
       !
       ! Matrix core
       !
!$omp parallel do default(shared), private(i2,vtmp)
       do i2=H_ref_bands(1),en%nbf(i_sp)
         call M_by_V('C',H_ref_bands(2),DIP_S(:,:,id+(istep-1)*3,ikm,i_sp),V_bands(:,i2,ikm_ibz),vtmp)
         !
         call mv_cgemv('C',H_ref_bands(2),en%nbf(i_sp),cONE,V_bands(:,:,ik_ibz),&
&                      H_ref_bands(2),vtmp,1,cZERO,Sm1_minus(:,i2,id+(istep-1)*3),1)
       enddo
!$omp end parallel do
       !
       !DEBUG>
       !call SERIAL_inversion(en%nbf(i_sp),Sm1_minus(:,:,id+(istep-1)*3),tmp_det,.FALSE.)
       !DEBUG<
       idx=k_map%k_map_inv(ikbz,:)
       idx(id)=idx(id)+istep
       idx=k_periodic_idx(idx,k_map)
       ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))  
       !
       ! Sm1_plus(l,m; k, k + delta) = \sum_{i,j} V*(l,i; k) S(i,j; k-delta, k) V(m,j ;k + delta)
       !
       ikp_ibz=Xk%sstar(ikp,1)
       !
       ! Full construction of the Sm1_plus matrix
       !
       !DEBUG>
       !
!       call M_by_M_omp('N','N',H_ref_bands(2),en%nbf(i_sp),H_ref_bands(2),cONE,DIP_S(:,:,id+(istep-1)*3,ikbz,i_sp),&
!&                  H_ref_bands(2),V_bands(:,:,ikp_ibz),H_ref_bands(2),cZERO,V_tmp(:,:),H_ref_bands(2))
!       !
!       call M_by_M_omp('C','N',en%nbf(i_sp),en%nbf(i_sp),H_ref_bands(2),cONE,V_bands(:,:,ik_ibz),&
!&                      H_ref_bands(2),V_tmp,H_ref_bands(2),cZERO,Sm1_plus(:,:,id+(istep-1)*3),en%nbf(i_sp))
       !
       !DEBUG<
       !
       ! Matrix head
       !
!$omp parallel do default(shared), private(i1,i2), collapse(2)
       do i2=1,H_ref_bands(1)-1
         do i1=1,H_ref_bands(1)-1
           Sm1_plus(i1,i2,id+(istep-1)*3)=DIP_S(i1,i2,id+(istep-1)*3,ikbz,i_sp)
         enddo
       enddo
!$omp end parallel do
       !
       ! Matrix wings
       !
!$omp parallel do default(shared), private(i1,i2), collapse(2)
       do i2=1,H_ref_bands(1)-1
         do i1=H_ref_bands(1),en%nbf(i_sp)
            Sm1_plus(i1,i2,id+(istep-1)*3)=&
            &  Vstar_dot_V(H_ref_bands(2),V_bands(:,i1,ik_ibz),DIP_S(:,i2,id+(istep-1)*3,ikbz,i_sp))
         enddo
       enddo
!$omp end parallel do
       !
       ! Matrix Core
       !
!$omp parallel do default(shared), private(i2,vtmp)
       do i2=H_ref_bands(1),en%nbf(i_sp)
         call M_by_V('N',H_ref_bands(2),DIP_S(:,:,id+(istep-1)*3,ikbz,i_sp),V_bands(:,i2,ikp_ibz),vtmp)
         !
         call mv_cgemv('C',H_ref_bands(2),en%nbf(i_sp),cONE,V_bands(:,:,ik_ibz),&
&                      H_ref_bands(2),vtmp,1,cZERO,Sm1_plus(:,i2,id+(istep-1)*3),1)
       enddo
!$omp end parallel do
       !DEBUG>
       !call SERIAL_inversion(en%nbf(i_sp),Sm1_plus(:,:,id+(istep-1)*3),S_det(id+(istep-1)*3),.TRUE.)
       !DEBUG>
     enddo  
     !
   endif
   !
 enddo
 !
 ! Invert matrices
 !
!$omp parallel do default(shared), private(id)
 do id=1,3
   if(k_map%max_kdir(id)==1) cycle ! non-periodic directions
     max_step=1
     if(k_map%max_kdir(id)>=6.and..not.l_force_SndOrd) max_step=2
!$omp parallel do default(shared), private(istep)
     do istep=1,max_step
       call SERIAL_inversion(en%nbf(i_sp),Sm1_minus(:,:,id+(istep-1)*3),tmp_det,.FALSE.)
       call SERIAL_inversion(en%nbf(i_sp), Sm1_plus(:,:,id+(istep-1)*3),S_det(id+(istep-1)*3),.TRUE.)
     enddo
!$omp end parallel do
  enddo
!$omp end parallel do
  !
end subroutine Build_overlaps_det_NEQ
