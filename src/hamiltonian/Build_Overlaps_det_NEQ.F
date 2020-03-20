!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use electrons,      ONLY:levels,n_sp_pol
 use nl_optics,      ONLY:NL_bands
 use R_lattice,      ONLY:k_map,bz_samp
 use vec_operate,    ONLY:k_periodic_idx
 use DIPOLES,        ONLY:DIP_S
 use wrapper_omp,    ONLY:M_by_V_omp,mv_cgemv_omp
 use wrapper,        ONLY:Vstar_dot_V
 use QP_m,           ONLY:QP_nk
 use electric,       ONLY:l_force_SndOrd
 !
 implicit none
 !
 integer,      intent(in) :: ikbz
 integer,      intent(in) :: i_sp
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: Xk
 complex(SP),  intent(in) :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 complex(SP), intent(out) :: S_det(6)
 complex(SP), intent(out) :: Sm1_plus (en%nbf,en%nbf,6)
 complex(SP), intent(out) :: Sm1_minus(en%nbf,en%nbf,6)
 !
 ! Work Space
 !
 integer     :: idx(3),id,ikp,ikm,i1,i2
 integer     :: ikm_ibz,ikp_ibz,ik_ibz,max_step,istep
 !complex(SP) :: V_tmp(NL_bands(2),en%nbf) ! used in full construction of overlaps
 complex(SP) :: tmp_det,vtmp(NL_bands(2))
 !
 S_det    =cZERO
 Sm1_plus =cZERO
 Sm1_minus=cZERO
 !
 ! I split the construction of the Sm1_minus(Sm1_plus) matrix in four parts
 !
 !   1:NL_bands(1)-1   NL_bands(1):NL_bands(2)
 !  _____________________________________
 !  |                 |                  |
 !  |     head        |      wing        |     1:NL_bands(1)-1
 !  |                 |                  |
 !  |                 |                  |
 !  |------------------------------------|
 !  |                 |                  | 
 !  |    wing         |      core        |   NL_bands(1):NL_bands(2)
 !  |                 |                  |
 !  |                 |                  |
 !  ------------------------------------- 
 !
 ! 
 !  The haed part is time-independent, the wings depends only from
 !  one of the time-dependent valence bands, while the core part depends from
 !  both the wave-functions at k and k-delta
 !
 do id=1,3
   !
   ik_ibz=Xk%sstar(ikbz,1)
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
!       call M_by_M_omp('C','N',NL_bands(2),en%nbf,NL_bands(2),cONE,DIP_S(:,:,id+(istep-1)*3,ikm,i_sp),&
!&                          NL_bands(2),V_bands(:,:,ikm_ibz,i_sp),NL_bands(2),cZERO,V_tmp,NL_bands(2))
       !
!       call M_by_M_omp('C','N',en%nbf,en%nbf,NL_bands(2),cONE,V_bands(:,:,ik_ibz,i_sp),&
!&                      NL_bands(2),V_tmp,NL_bands(2),cZERO,Sm1_minus(:,:,id+(istep-1)*3),en%nbf)
       !
       ! Matrix head
       !
!$omp parallel do default(shared), private(i1,i2)
       do i2=1,NL_bands(1)-1
         do i1=1,NL_bands(1)-1
           Sm1_minus(i1,i2,id+(istep-1)*3)=conjg(DIP_S(i2,i1,id+(istep-1)*3,ikm,i_sp))
         enddo
       enddo
!$omp end parallel do
       !
       ! Matrix wings
       !
!$omp parallel do default(shared), private(i1,i2)
       do i2=1,NL_bands(1)-1
         do i1=NL_bands(1),en%nbf
           Sm1_minus(i1,i2,id+(istep-1)*3)=Vstar_dot_V(NL_bands(2),V_bands(:,i1,ik_ibz,i_sp), &
&                conjg(DIP_S(i2,:,id+(istep-1)*3,ikm,i_sp)))
          enddo
       enddo
!$omp end parallel do
       !
       ! Matrix core
       !
       do i2=NL_bands(1),en%nbf
         call M_by_V_omp('C',NL_bands(2),DIP_S(:,:,id+(istep-1)*3,ikm,i_sp),V_bands(:,i2,ikm_ibz,i_sp),vtmp)
         !
         call mv_cgemv_omp('C',NL_bands(2),en%nbf,cONE,V_bands(:,:,ik_ibz,i_sp),&
&                      NL_bands(2),vtmp,1,cZERO,Sm1_minus(:,i2,id+(istep-1)*3),1)
       enddo
       !
       call SERIAL_inversion(en%nbf,Sm1_minus(:,:,id+(istep-1)*3),tmp_det,.false.)
       !
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
!       call M_by_M_omp('N','N',NL_bands(2),en%nbf,NL_bands(2),cONE,DIP_S(:,:,id+(istep-1)*3,ikbz,i_sp),&
!&                  NL_bands(2),V_bands(:,:,ikp_ibz,i_sp),NL_bands(2),cZERO,V_tmp(:,:),NL_bands(2))
!       !
!       call M_by_M_omp('C','N',en%nbf,en%nbf,NL_bands(2),cONE,V_bands(:,:,ik_ibz,i_sp),&
!&                      NL_bands(2),V_tmp,NL_bands(2),cZERO,Sm1_plus(:,:,id+(istep-1)*3),en%nbf)
       !
       ! Matrix head
       !
!$omp parallel do default(shared), private(i1,i2)
       do i2=1,NL_bands(1)-1
         do i1=1,NL_bands(1)-1
           Sm1_plus(i1,i2,id+(istep-1)*3)=DIP_S(i1,i2,id+(istep-1)*3,ikbz,i_sp)
         enddo
       enddo
!$omp end parallel do
       !
       ! Matrix wings
       !
!$omp parallel do default(shared), private(i1,i2)
       do i2=1,NL_bands(1)-1
         do i1=NL_bands(1),en%nbf
            Sm1_plus(i1,i2,id+(istep-1)*3)=Vstar_dot_V(NL_bands(2),V_bands(:,i1,ik_ibz,i_sp),DIP_S(:,i2,id+(istep-1)*3,ikbz,i_sp))
         enddo
       enddo
!$omp end parallel do
       !
       ! Matrix Core
       !
       do i2=NL_bands(1),en%nbf
         call M_by_V_omp('N',NL_bands(2),DIP_S(:,:,id+(istep-1)*3,ikbz,i_sp),V_bands(:,i2,ikp_ibz,i_sp),vtmp)
         !
         call mv_cgemv_omp('C',NL_bands(2),en%nbf,cONE,V_bands(:,:,ik_ibz,i_sp),&
&                      NL_bands(2),vtmp,1,cZERO,Sm1_plus(:,i2,id+(istep-1)*3),1)
       enddo
       !
       call SERIAL_inversion(en%nbf,Sm1_plus(:,:,id+(istep-1)*3),S_det(id+(istep-1)*3),.true.)
       !
     enddo  
     !
   endif
   !
 enddo
 !
end subroutine Build_overlaps_det_NEQ
