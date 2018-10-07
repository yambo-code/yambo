!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine Build_W_operator(V_bands,en,Xk,A_input,ik,i_sp,H_nl_sc)
 !
 ! Build the Hermitian W operator eq. 62 and 63
 ! in PRB 69, 085106 (2004)
 !
 use pars,           ONLY:SP,cI,cZERO,cONE,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels,n_sp_pol
 use electric,       ONLY:Sm1_plus,Sm1_minus,l_force_SndOrd
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands,NL_nbands
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:a
 use vec_operate,    ONLY:k_periodic_idx
 use matrix_operate, ONLY:hermitian
 use fields,         ONLY:gauge_field
 use DIPOLES,        ONLY:DIP_S
 use wrapper_omp,    ONLY:M_by_M_omp
 !
 implicit none
 !
 type(levels), intent(in)      :: en
 type(bz_samp),intent(in)      :: Xk
 complex(SP),  intent(in)      :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 type(gauge_field), intent(in) :: A_input
 integer,      intent(in)      :: ik
 integer,      intent(in)      :: i_sp
 complex(SP),  intent(inout)   :: H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),n_sp_pol)
 !
 ! Work Space
 !
 integer :: i1,i2,iv1,im,idx(3),id,ikp,id_red,ikm
 integer :: ikm_ibz,ikp_ibz,ikbz,istep,max_step
 complex(SP) :: Wk(3,NL_bands(2),NL_bands(2))
 complex(SP) :: Wop(NL_bands(2),NL_bands(2),2)
 complex(SP) :: tmp_Vp(NL_bands(2),en%nbf),tmp_Vm(NL_bands(2),en%nbf)
 complex(SP) :: delta_tV_bnd(en%nbf,NL_bands(2))
 complex(SP) :: E_vec_pot(3)
 !
 ! This subroutine works only without symmetries to be generalized
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
       idx=k_map%k_map_inv(ikbz,:)
       idx(id_red)=idx(id_red)-istep
       idx=k_periodic_idx(idx,k_map)
       ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
       !
       idx=k_map%k_map_inv(ikbz,:)
       idx(id_red)=idx(id_red)+istep
       idx=k_periodic_idx(idx,k_map)
       ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))
       !
       ! Indexes in the IBZ
       ! 
       ikm_ibz=Xk%sstar(ikm,1)
       ikp_ibz=Xk%sstar(ikp,1)
       !
       ! Construct the \tilde V_bands diffence
       ! delta_tV_bnd = \tilde V_bands(k,k+delta) - \tilde V_bands(k,k-delta)
       !
       delta_tV_bnd=cZERO
       !
       ! Build Vp and Vm (stored in tmp_Vp and tmp_Vm)
       !
       ! m,n indexes on valence bands
       !
       ! i,j indexes on all bands CMR basis
       !
       ! Vp(m,i) = \sum_{j} S(k,K+delta)_{i,j}   V(m,j)
       !
       ! Vm(m,i) = \sum_{j} S^H(k-delta,K)_{j,i} V(m,j)
       !
       ! 
       ! Build \tilde V(k+delta) - \tilde V(k-delta)
       ! ===========================================
       ! where \tilde V(k+delta,n,:) = \sum_m \tilde S^-1(k,k+delta)_{m,n} * Vp(m,:)
       ! 
       call M_by_M_omp('N','N',NL_nbands,en%nbf,NL_bands(2),cONE,  &
&        DIP_S(NL_bands(1):,:,id_red+(istep-1)*3,ikbz,i_sp),NL_nbands, &
&        V_bands(:,:,ikp_ibz,i_sp),NL_bands(2),cZERO,tmp_Vp(NL_bands(1):,:),NL_nbands)
       !
       call M_by_M_omp('C','N',NL_nbands,en%nbf,NL_bands(2),cONE,  &
&           DIP_S(:,NL_bands(1):,id_red+(istep-1)*3,ikm ,i_sp),NL_bands(2), &
&           V_bands(:,:,ikm_ibz,i_sp),NL_bands(2),cZERO,tmp_Vm(NL_bands(1):,:),NL_nbands)
       !
       ! Build \tilde V(k+delta) - \tilde V(k-delta)
       ! ===========================================
       ! where \tilde V(k+delta,n,:) = \sum_m \tilde S^-1(k,k+delta)_{m,n} * Vp(m,:)
       ! 
       do im=1,en%nbf 
         forall(i2=NL_bands(1):NL_bands(2),iv1=1:en%nbf)
           delta_tV_bnd(iv1,i2)=delta_tV_bnd(iv1,i2)+Sm1_plus (im,iv1,id_red+(istep-1)*3,ikbz,i_sp)*tmp_Vp(i2,im) &
&                                                   -Sm1_minus(im,iv1,id_red+(istep-1)*3,ikbz,i_sp)*tmp_Vm(i2,im)
         end forall
       enddo
       !  
       ! Build the W operator as
       !
       ! W(i,j) = \sum_m V*(m,i) * [\tilde Vp(m,j) - \tilde Vm(m,j) ]
       !
!$omp parallel do default(shared), private(i2,i1)
       do i2=NL_bands(1),NL_bands(2)
         do i1=NL_bands(1),NL_bands(2)
           Wop(i1,i2,istep)=sum(conjg(V_bands(i2,1:en%nbf,ik,i_sp))*delta_tV_bnd(1:en%nbf,i1))
         enddo
       enddo
!$omp end parallel do
       !
       Wop(NL_bands(1):,NL_bands(1):,istep)=Wop(NL_bands(1):,NL_bands(1):,istep)*cI*k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
       !
     enddo
     !
   else ! non-periodic directions 
     !
     ! <\phi_i | e^{iGr} | \phi_j >
     !
     Wop(NL_bands(1):,NL_bands(1):,:)=cZERO !DIP_S(NL_bands(1):,NL_bands(1):,id_red,ik,i_sp)*cI/(4._SP*pi)   
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
     do id=1,3
       Wk(id,NL_bands(1):,NL_bands(1):)=Wk(id,NL_bands(1):,NL_bands(1):)+Wop(NL_bands(1):,NL_bands(1):,1)*a(id_red,id)
     enddo
     !
   else
     !      
     ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
     ! Eq. 97 of PRB 63, 155107(2001)
     !
     do id=1,3
       Wk(id,NL_bands(1):,NL_bands(1):)=Wk(id,NL_bands(1):,NL_bands(1):)+&
&               (4._SP*Wop(NL_bands(1):,NL_bands(1):,1)-Wop(NL_bands(1):,NL_bands(1):,2))/3._SP*a(id_red,id)
     enddo

   endif
   !
 enddo ! loop on id_red
 !
 ! Make an Hermitian operator
 !
 do id=1,3
   Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))=(Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2)) &
&                                               +hermitian(Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))))
 enddo
 !
 do id=1,3
   H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),i_sp)= &
&  H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),i_sp)-  &
&      E_vec_pot(id)*Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
 enddo
 !
end subroutine Build_W_operator
