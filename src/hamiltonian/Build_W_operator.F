!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
subroutine Build_W_operator(en,Xk,A_input,ik,i_sp,V_bands,H_nl_sc)
 !
 ! Build the Hermitian W operator eq. 62 and 63
 ! in PRB 69, 085106 (2004)
 !
 use pars,           ONLY:SP,cI,cZERO,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels
 use electric,       ONLY:l_force_SndOrd
 use QP_m,           ONLY:QP_nk
 use hamiltonian,    ONLY:H_ref_bands
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:a
 use matrix_operate, ONLY:hermitian
 use fields,         ONLY:gauge_field
 !
 implicit none
 !
 type(levels), intent(in)      :: en
 type(bz_samp),intent(in)      :: Xk
 type(gauge_field), intent(in) :: A_input
 integer,      intent(in)      :: ik
 integer,      intent(in)      :: i_sp
 complex(SP),  intent(in)      :: V_bands(H_ref_bands(2),en%nbf(i_sp),QP_nk)
 complex(SP),  intent(inout)   :: H_nl_sc(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
 ! Work Space
 !
 integer :: i1,i2,im,id,id_red
 integer :: ikbz,istep,max_step
 complex(SP) :: Wk(3,H_ref_bands(2),H_ref_bands(2))
 complex(SP) :: Wop(H_ref_bands(2),H_ref_bands(2),2)
 complex(SP) :: V_tilde(en%nbf(i_sp),H_ref_bands(2),2)
 complex(SP) :: E_vec_pot(3)
 !
 E_vec_pot=-A_input%vecpot_vel/SPEED_OF_LIGHT
 !
 Wk=cZERO
 !
 ikbz=Xk%k_table(ik,1)
 !
 do id_red=1,3  ! loop on reciprocal lattice
   !
   if(k_map%max_kdir(id_red)/=1) then
     !
     max_step=1
     !
     ! I need at leat 5 k-points in the line to
     ! use the second order formula
     !
     if(k_map%max_kdir(id_red)>=6.and..not.l_force_SndOrd) max_step=2
     !
     do istep=1,max_step
       !
       call Build_tilde_vbands(en,Xk,ikbz,i_sp,istep,id_red,V_bands,V_tilde)
       !
       ! Build the W operator as
       !
       ! W(i,j) = \sum_m V*(m,i) * [\tilde Vp(m,j) - \tilde Vm(m,j) ]
       !
       !$omp parallel do default(shared), private(i2,i1), collapse(2)
       do i2=H_ref_bands(1),H_ref_bands(2)
         do i1=H_ref_bands(1),H_ref_bands(2)
           Wop(i1,i2,istep)=sum(conjg(V_bands(i2,1:en%nbf(i_sp),ik))*(V_tilde(1:en%nbf(i_sp),i1,1)-V_tilde(1:en%nbf(i_sp),i1,2)))
         enddo
       enddo
       !$omp end parallel do
       !
       !$omp parallel do default(shared), private(i1)
       do i1=H_ref_bands(1),H_ref_bands(2)
         Wop(i1,H_ref_bands(1):,istep)=Wop(i1,H_ref_bands(1):,istep)*cI*k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
       enddo
       !$omp end parallel do
       !
     enddo
     !
   else ! non-periodic directions 
     !
     ! <\phi_i | e^{iGr} | \phi_j >
     !
     Wop(H_ref_bands(1):,H_ref_bands(1):,:)=cZERO !DIP_S(H_ref_bands(1):,H_ref_bands(1):,id_red,ik,i_sp)*cI/(4._SP*pi)   
     !
   endif    
   !
   ! Transform in cartesian coordinates 
   !
   if(max_step==1) then
     !      
     ! First order formula D(dk)      
     ! Eq. 96 of PRB 63, 155107(2001)
     !
     !$omp parallel do default(shared), private(id)
     do id=1,3
       Wk(id,H_ref_bands(1):,H_ref_bands(1):)=Wk(id,H_ref_bands(1):,H_ref_bands(1):)&
       &                                       +Wop(H_ref_bands(1):,H_ref_bands(1):,1)*a(id_red,id)
     enddo
     !$omp end parallel do
     !
   else
     !      
     ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
     ! Eq. 97 of PRB 63, 155107(2001)
     !
     !$omp parallel do default(shared), private(id)
     do id=1,3
       Wk(id,H_ref_bands(1):,H_ref_bands(1):)=Wk(id,H_ref_bands(1):,H_ref_bands(1):)+&
&               (4._SP*Wop(H_ref_bands(1):,H_ref_bands(1):,1)-Wop(H_ref_bands(1):,H_ref_bands(1):,2))/3._SP*a(id_red,id)
     enddo
     !$omp end parallel do
     !
   endif
   !
 enddo ! loop on id_red
 !
 ! Make an Hermitian operator
 !
 !$omp parallel do default(shared), private(id)
 do id=1,3
   Wk(id,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))=                     &
   &     (Wk(id,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))               &
   &      +hermitian(Wk(id,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))))
 enddo
 !$omp end parallel do
 !
 !$omp parallel do default(shared), private(i1,id)
 do i1=H_ref_bands(1),H_ref_bands(2)
   do id=1,3
     H_nl_sc(i1,H_ref_bands(1):H_ref_bands(2))=H_nl_sc(i1,H_ref_bands(1):H_ref_bands(2))-         &
     &                                               E_vec_pot(id)*Wk(id,i1,H_ref_bands(1):H_ref_bands(2))
   enddo
 enddo
 !$omp end parallel do
 !
end subroutine Build_W_operator
