!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_apply_RWA(ik,E,k,dT,dTp,dG_out,dG_in)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use real_time,      ONLY:RT_E_and_k,RT_Deph_Matrix,RT_all2ibz,l_RT_EXP,RT_is_statically_dephased,RT_bands
 !
 implicit none
 !
 integer,      intent(in)     :: ik
 type(levels), intent(in)     :: E
 type(bz_samp),intent(in)     :: k
 real(SP),     intent(in)     :: dT,dTp
 !
 complex(SP),  intent(in)     :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP),  intent(inout)  :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work space
 !
 integer  :: i_n,i_np,ik_ibz
 real(SP) :: deltaE,sumGAMMA,alpha,dec_exp
 complex(SP) :: phase
 !
 ik_ibz=RT_all2ibz(ik)
 !
 do i_n=RT_bands(1),RT_bands(2)
   do i_np=i_n+1,RT_bands(2)
     !
     sumGAMMA=0._SP
     !
     if (RT_is_statically_dephased) sumGAMMA=RT_Deph_Matrix(i_n,i_np,ik_ibz)
     !
     deltaE=RT_E_and_k%E(i_n,ik,1)-RT_E_and_k%E(i_np,ik,1)
     !
     if (l_RT_EXP) then
       !
       alpha=-deltaE*dT
       phase=cmplx(cos(alpha),sin(alpha))
       dec_exp=exp(-sumGAMMA*dT)
       !
     else if(dTp>0._SP) then
       !
       alpha=-deltaE*dTp
       phase=cmplx(cos(alpha),sin(alpha))
       dec_exp=exp(-sumGAMMA*dTp)
       !
     endif
     !
     if (l_RT_EXP.or.dTp>0._SP) then
       !
       dG_out(i_n,i_np)=dG_out(i_n,i_np)*      phase *dec_exp
       dG_out(i_np,i_n)=dG_out(i_np,i_n)*conjg(phase)*dec_exp
       !
     endif
     !
     if(l_RT_EXP) then
       dG_out(i_n,i_np)=dG_out(i_n,i_np)+dG_in(i_n,i_np)
       cycle
     endif
     !
     if (dT/=dTp) then
       !
       alpha=-deltaE*dT
       phase=cmplx(cos(alpha),sin(alpha))
       dec_exp=exp(-sumGAMMA*dT)
       !
     endif
     !
     dG_out(i_n,i_np)=dG_out(i_n,i_np)+dG_in(i_n,i_np)*      phase *dec_exp
     dG_out(i_np,i_n)=dG_out(i_np,i_n)+dG_in(i_np,i_n)*conjg(phase)*dec_exp
     !
   enddo
   !
   dG_out(i_n,i_n)=dG_out(i_n,i_n)+dG_in(i_n,i_n)
   !
   ! It is higly non satisfactory that
   !     Gamma_neq[f_eq] /= Gamma_eq                              (1)
   ! This explains the need for the special definition
   !    sumGAMMA=abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik,1))           (2)
   ! instead of
   !    sumGAMMA=sum(REF_lifetimes(i_n,ik,:))                     (3)
   ! with i_n=i_np.
   ! 
   ! This difference is due to two reasons:
   ! (a) For the ee scatt REF_lifetimes are computed with a static approx to
   !     the out-of-equilibrium GW
   ! (b) The Gamma_eq are computed in the \eta->0 limit, i.e. no QP approx,
   !     while the Gamma_neq are computed in the QP approx \eta=Gamma_eq
   !
   !sumGAMMA=sum(REF_lifetime(i_n,ik,:))
   !dec_exp=exp(-sumGAMMA*dT)
   !dG_out(i_n,i_n)=dG_out(i_n,i_n)*dec_exp
   !
 enddo
 !
end subroutine RT_apply_RWA
