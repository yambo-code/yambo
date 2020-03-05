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
subroutine Build_W_operator(V_bands,en,Xk,A_input,ik,i_sp,H_nl_sc)
 !
 ! Build the Hermitian W operator eq. 62 and 63
 ! in PRB 69, 085106 (2004)
 !
 use pars,           ONLY:SP,cI,cZERO,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels,n_sp_pol
 use electric,       ONLY:l_force_SndOrd
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:a
 use matrix_operate, ONLY:hermitian
 use fields,         ONLY:gauge_field
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
 integer :: i1,i2,im,id,id_red
 integer :: ikbz,istep,max_step
 complex(SP) :: Wk(3,NL_bands(2),NL_bands(2))
 complex(SP) :: Wop(NL_bands(2),NL_bands(2),2)
 complex(SP) :: V_tilde(en%nbf,NL_bands(2),2)
 complex(SP) :: E_vec_pot(3)
 !
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
       call Build_tilde_vbands(V_bands,en,Xk,ikbz,i_sp,istep,id_red,V_tilde)
       !
       ! Build the W operator as
       !
       ! W(i,j) = \sum_m V*(m,i) * [\tilde Vp(m,j) - \tilde Vm(m,j) ]
       !
!$omp parallel do default(shared), private(i2,i1)
       do i2=NL_bands(1),NL_bands(2)
         do i1=NL_bands(1),NL_bands(2)
           Wop(i1,i2,istep)=sum(conjg(V_bands(i2,1:en%nbf,ik,i_sp))*(V_tilde(1:en%nbf,i1,1)-V_tilde(1:en%nbf,i1,2)))
         enddo
       enddo
!$omp end parallel do
       !
!$omp parallel do default(shared), private(i1)
       do i1=NL_bands(1),NL_bands(2)
         Wop(i1,NL_bands(1):,istep)=Wop(i1,NL_bands(1):,istep)*cI*k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP))
       enddo
!$omp end parallel do
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
!$omp parallel do default(shared), private(id)
     do id=1,3
       Wk(id,NL_bands(1):,NL_bands(1):)=Wk(id,NL_bands(1):,NL_bands(1):)+Wop(NL_bands(1):,NL_bands(1):,1)*a(id_red,id)
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
       Wk(id,NL_bands(1):,NL_bands(1):)=Wk(id,NL_bands(1):,NL_bands(1):)+&
&               (4._SP*Wop(NL_bands(1):,NL_bands(1):,1)-Wop(NL_bands(1):,NL_bands(1):,2))/3._SP*a(id_red,id)
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
   Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))=(Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2)) &
&                                               +hermitian(Wk(id,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))))
 enddo
!$omp end parallel do
 !
!$omp parallel do default(shared), private(i1,id)
 do i1=NL_bands(1),NL_bands(2)
   do id=1,3
     H_nl_sc(i1,NL_bands(1):NL_bands(2),i_sp)=H_nl_sc(i1,NL_bands(1):NL_bands(2),i_sp)-            &
&                           E_vec_pot(id)*Wk(id,i1,NL_bands(1):NL_bands(2))
   enddo
 enddo
!$omp end parallel do
 !
end subroutine Build_W_operator
