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
module RT_operations
 !
 use pars,           ONLY:SP,rZERO
 use RT_occupations, ONLY:RT_el_occ,RT_ho_occ
 use RT_lifetimes,   ONLY:RT_lifetime,RT_EP_emit_life,RT_EE_life,RT_EH_life,RT_EP_abs_life,&
&                         RT_EP_emit_REF_life,RT_EE_REF_life,RT_EH_REF_life,RT_EP_abs_REF_life
 use real_time,      ONLY:G_lesser_reference,integrator_step,RT_bands,RT_t
 !
 implicit none
 !
 interface
   !
   subroutine RT_average_operator(N_V,O_VALUE,V_VALUE,O_REAL,O_QP,O_MATRIX,&
&                                 V_MATRIX,PV_MATRIX,G,k,en,TRANS,Double_Grid,nK_mem)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp,nXkibz
     use QP_m,       ONLY:QP_table,QP_n_states,QP_nk
     use electrons,  ONLY:n_spin,n_sp_pol,levels
     use FFT_m,      ONLY:fft_size
     use real_time,  ONLY:RT_bands
     type(bz_samp), intent(in) :: k
     complex(SP),   intent(in) :: G(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),QP_nk)
     type(levels),  intent(in) :: en
     integer,       intent(in) :: nK_mem,N_V
     complex(SP),     optional,intent(out) :: O_VALUE,V_VALUE(N_V)
     complex(SP),     optional, intent(in) ::  V_MATRIX(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) :: PV_MATRIX(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) ::  O_MATRIX(    RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
     real(SP),        optional, intent(in) :: O_REAL(fft_size,n_spin)
     logical,         optional, intent(in) :: TRANS
     character(6),    optional, intent(in) :: Double_Grid
   end subroutine RT_average_operator
   !
   subroutine RT_update(G_input,H_rotation,it,E,k,X,up_Gref,up_DIP,an_QP)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use QP_m,       ONLY:QP_nk
     use electrons,  ONLY:levels,n_sp_pol
     use X_m,        ONLY:X_t
     use real_time,  ONLY:RT_bands
     type(X_t),      intent(inout)        :: X
     complex(SP),    intent(inout)        :: G_input(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),QP_nk)
     complex(SP),    intent(in)           :: H_rotation(RT_bands(2),RT_bands(2),QP_nk,n_sp_pol)
     type(bz_samp),  intent(in)           :: k
     type(levels),   intent(in)           :: E
     integer,        intent(in)           :: it
     logical,        optional, intent(in) :: up_Gref,up_DIP,an_QP
   end subroutine RT_update
   !
 end interface
 !
 contains
   !
   !-----------
   ! LIFETIMES
   !-----------
   !
   real(SP) function ABS_RT_life_REF(i1,i2)
     integer                       :: i1,i2
     ABS_RT_life_REF=rZERO
     if (RT_EP_emit_life%active) then
       ABS_RT_life_REF=RT_EP_emit_REF_life%abs(i1,i2)
     endif
     if (RT_EP_abs_life%active) then
       ABS_RT_life_REF=ABS_RT_life_REF+RT_EP_abs_REF_life%emit(i1,i2)
     endif
     if (RT_EE_life%active) then
       ABS_RT_life_REF=ABS_RT_life_REF+RT_EE_REF_life%abs(i1,i2)
     endif
     if (RT_EH_life%active) then
       ABS_RT_life_REF=ABS_RT_life_REF+RT_EH_REF_life%abs(i1,i2)
     endif
   end function
   !
   real(SP) function EMIT_RT_life_REF(i1,i2)
     integer                       :: i1,i2
     EMIT_RT_life_REF=rZERO
     if (RT_EP_emit_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EP_emit_REF_life%emit(i1,i2)
     endif
     if (RT_EP_abs_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EP_abs_REF_life%abs(i1,i2)
     endif
     if (RT_EE_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EE_REF_life%emit(i1,i2)
     endif
     if (RT_EH_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EH_REF_life%emit(i1,i2)
     endif
   end function
   !
   real(SP) function ABS_RT_life_dGAMMA(i1,i2)
     integer                       :: i1,i2
     ABS_RT_life_dGAMMA=rZERO
     if (RT_EP_emit_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EP_emit_life%abs(i1,i2)
     endif
     if (RT_EP_abs_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EP_abs_life%emit(i1,i2)
     endif
     if (RT_EE_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EE_life%abs(i1,i2)
     endif
     if (RT_EH_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EH_life%abs(i1,i2)
     endif
   end function
   !
   real(SP) function EMIT_RT_life_dGAMMA(i1,i2)
     integer                       :: i1,i2
     EMIT_RT_life_dGAMMA=rZERO
     if (RT_EP_emit_life%active) then
       EMIT_RT_life_dGAMMA=RT_EP_emit_life%emit(i1,i2)
     endif
     if (RT_EP_abs_life%active) then
       EMIT_RT_life_dGAMMA=EMIT_RT_life_dGAMMA+RT_EP_abs_life%abs(i1,i2)
     endif
     if (RT_EE_life%active) then
       EMIT_RT_life_dGAMMA=EMIT_RT_life_dGAMMA+RT_EE_life%emit(i1,i2)
     endif
     if (RT_EH_life%active) then
       EMIT_RT_life_dGAMMA=EMIT_RT_life_dGAMMA+RT_EH_life%emit(i1,i2)
     endif
   end function
   !
   real(SP) function ABS_RT_life_one_KIND(LIFE,LIFE_ref,i1,i2)
     type(RT_lifetime), intent(in) :: LIFE,LIFE_ref
     integer                       :: i1,i2
     ABS_RT_life_one_KIND=rZERO
     if (.not.LIFE%active) return
     ABS_RT_life_one_KIND=LIFE%abs(i1,i2)+LIFE_ref%abs(i1,i2)
     if (LIFE%kind=="el-ph_emit".and.RT_EP_abs_life%active) then
       ABS_RT_life_one_KIND=ABS_RT_life_one_KIND+RT_EP_abs_life%emit(i1,i2)+RT_EP_abs_REF_life%emit(i1,i2)
     endif
   end function
   !
   real(SP) function EMIT_RT_life_one_KIND(LIFE,LIFE_ref,i1,i2)
     type(RT_lifetime), intent(in) :: LIFE,LIFE_ref
     integer                       :: i1,i2
     EMIT_RT_life_one_KIND=rZERO
     if (.not.LIFE%active) return
     EMIT_RT_life_one_KIND=LIFE%emit(i1,i2)+LIFE_ref%emit(i1,i2)
     if (LIFE%kind=="el-ph_emit".and.RT_EP_abs_life%active) then
       EMIT_RT_life_one_KIND=EMIT_RT_life_one_KIND+RT_EP_abs_life%abs(i1,i2)+RT_EP_abs_REF_life%abs(i1,i2)
     endif
   end function
   !
   logical function FIRST_step( )
     FIRST_step=integrator_step==1
   end function
   !
   subroutine ADD_to_saved_values(VAL,ARRAY,STEPS)
     integer  :: STEPS
     real(SP) :: VAL,ARRAY(STEPS)
     integer  :: i1
     do i1=1,STEPS-1
       ARRAY(i1)=ARRAY(i1+1)
     enddo
     ARRAY(STEPS)=VAL
   end subroutine
   !
   subroutine RT_init_occupation_reference( )
     use electrons,     ONLY:spin_occ
     integer               :: ib
     do ib=RT_bands(1),RT_bands(2)
       RT_el_occ%N_ref(ib,:)=         aimag(G_lesser_reference(ib,ib,:))
       RT_ho_occ%N_ref(ib,:)=spin_occ-aimag(G_lesser_reference(ib,ib,:))
     enddo
   end subroutine
   !
   subroutine RT_lifetime_to_RT_obj(action,i1,i2,J,LIFE,LIFE_ref,LIFE_obj)
     use RT_lifetimes,   ONLY:RT_obj_lifetime
     use real_time,      ONLY:RT_levels
     character(*)          :: action
     integer               :: i1,i2,J
     type(RT_lifetime)     :: LIFE,LIFE_ref
     type(RT_obj_lifetime) :: LIFE_obj
     if (.not.LIFE%active) return
     if (.not.LIFE_obj%active) return
     if (action=="fill_ref") then
       LIFE_obj%abs_bare(J) =LIFE_ref%abs(i1,i2)
       LIFE_obj%emit_bare(J)=LIFE_ref%emit(i1,i2)
     endif
     if (action=="fill") then
       LIFE_obj%abs(J) =LIFE%abs(i1,i2)
       LIFE_obj%emit(J)=LIFE%emit(i1,i2)
     endif
     if (action=="dump".and.allocated(RT_levels%E)) then
       LIFE%abs(i1,i2) =LIFE_obj%abs(J) 
       LIFE%emit(i1,i2)=LIFE_obj%emit(J)
     endif
   end subroutine
   !
end module
