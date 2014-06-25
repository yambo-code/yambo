!
!        Copyright (C) 2000-2014 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_apply_zero_dyn(ik,E,dT,dTp,G_new_k,G_old_k)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:RT_ibz_coo,SC_bands,RT_all2ibz
 use electrons,      ONLY:levels
 use real_time,      ONLY:RIM_ibz_coo,l_RT_uses_E_RIM,REF_lifetime
 !
 implicit none
 !
 integer,      intent(in)     :: ik
 type(levels), intent(in)     :: E
 real(SP),     intent(in)     :: dT,dTp
 !
 complex(SP),  intent(in)     :: G_old_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP),  intent(inout)  :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 ! Work space
 !
 integer  :: i_n,i_np,ik_RIM,ik_ibz
 real(SP) :: deltaE,sumGAMMA,alpha,dec_exp
 complex(SP) :: phase
 !
 ik_ibz=RT_all2ibz(ik)
 ik_RIM=ik-RT_ibz_coo(ik_ibz,1)+RIM_ibz_coo(ik_ibz,1)
 !
 do i_n=SC_bands(1),SC_bands(2)
   do i_np=i_n+1,SC_bands(2)
     if (     l_RT_uses_E_RIM) then
       deltaE=E%E_RIM(i_n,ik_RIM,1)-E%E_RIM(i_np,ik_RIM,1)
       sumGAMMA=(abs(E%W_RIM(i_n,ik_RIM,1))+abs(E%W_RIM(i_np,ik_RIM,1)))/2._SP
     endif
     if (.not.l_RT_uses_E_RIM) then
       deltaE=E%E(i_n,ik,1)-E%E(i_np,ik,1)
       sumGAMMA=(abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik,1)))/2._SP
     endif
     !
     if(dTp>0._SP) then
       alpha=-deltaE*dTp
       phase=cmplx(cos(alpha),sin(alpha))
       dec_exp=exp(-sumGAMMA*dTp)
       G_new_k(i_n,i_np)=G_new_k(i_n,i_np)*      phase *dec_exp
       G_new_k(i_np,i_n)=G_new_k(i_np,i_n)*conjg(phase)*dec_exp
     endif
     !
     if(dT/=dTp) then
       alpha=-deltaE*dT
       phase=cmplx(cos(alpha),sin(alpha))
       dec_exp=exp(-sumGAMMA*dT)
     endif
     !
     G_new_k(i_n,i_np)=G_new_k(i_n,i_np)+G_old_k(i_n,i_np)*      phase *dec_exp
     G_new_k(i_np,i_n)=G_new_k(i_np,i_n)+G_old_k(i_np,i_n)*conjg(phase)*dec_exp
     !
   enddo
   !
   G_new_k(i_n,i_n)=G_new_k(i_n,i_n)+G_old_k(i_n,i_n)
   !
   ! It is higly non satisfactory that 
   !     Gamma_neq[f_eq] /= Gamma_eq                              (1)
   ! This explains the need for the special definition
   !    sumGAMMA=sum(REF_lifetimes(i_n,ik,:))                     (2)
   ! instead of
   !    sumGAMMA=abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik,1))/2._SP     (3)
   ! with i_n=i_np. Notice the 0.5 factor in Eq.(3) so that
   ! Eq.(2)=Eq.(3) in case Eq.(1) holds
   ! 
   ! This difference is due to two reasons, the second one is more relevant:
   ! (1) For the ee scatt REF_lifetimes are computed with a static approx to
   !     the out-of-equilibrium GW
   ! (2) The Gamma_eq are computed in the \eta->0 limit, i.e. no QP approx,
   !     while the Gamma_neq are computed in the QP approx \eta=Gamma_eq
   !
   !sumGAMMA=sum(REF_lifetime(i_n,ik,:))
   !dec_exp=exp(-sumGAMMA*dT)
   !G_new_k(i_n,i_n)=G_new_k(i_n,i_n)*dec_exp
   !
 enddo
 !
end subroutine RT_apply_zero_dyn
