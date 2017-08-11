!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine NL_average_operator(Xen,Xk,V_bands,OMATRIX,TRANS,VALUE) 
 !
 ! Average an operator on the time-dependent valence bands
 !
 use pars,        ONLY:SP,cZERO,cONE
 use electrons,   ONLY:levels,n_sp_pol,spin_occ
 use nl_optics,   ONLY:NL_bands
 use QP_m,        ONLY:QP_nk
 use wrapper_omp, ONLY:M_by_V_omp,Vstar_dot_V_omp
 use R_lattice,   ONLY:bz_samp,nXkbz
 use parallel_m,  ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 type(levels), intent(in)  :: Xen
 type(bz_samp),intent(in)  :: Xk
 complex(SP), intent(in)   :: V_bands(NL_bands(2),Xen%nbf,QP_nk,n_sp_pol)
 complex(SP), intent(in)   :: OMATRIX(NL_bands(2),NL_bands(2),nXkbz,n_sp_pol)
 logical,     intent(in)   :: TRANS
 complex(SP), intent(out)  :: VALUE
 !
 ! Work Space
 !
 integer      :: iv,ik,is,ik_bz,i_star
 complex(SP)  :: V_tmp(NL_bands(2))
 complex(SP)  :: Hk(NL_bands(2),NL_bands(2),n_sp_pol)
 character(1) :: op
 !
 VALUE=cZERO
 !
 ! Calculate the bra-ket
 !
 ! VALUE = sum_{ik_bz,is,iv}   <V(iv,ik,is) | O (ik_bz,is) | V(iv,ik,is) > 
 !
 op='N'
 if(TRANS.eqv..true.) op='T'
 !
 do ik=1,Xk%nibz
   !
   if(.not.PAR_IND_XK_ibz%element_1D(ik)) cycle
   !
   Hk=cZERO
   !
   do i_star=1,Xk%nstar(ik) 
     !
     ik_bz             = i_star
     if ( ik>1 ) ik_bz = sum(Xk%nstar(1:ik-1))+i_star           
     !
     Hk=Hk+OMATRIX(:,:,ik_bz,:)
     !
   enddo
   !   
   do iv=1,Xen%nbf
     do is=1,n_sp_pol
       call M_by_V_omp(op,NL_bands(2),NL_bands(2),cONE,Hk(:,:,is),NL_bands(2),V_bands(:,iv,ik,is),1,cZERO,V_tmp,1)
       VALUE=VALUE+spin_occ*Vstar_dot_V_omp(NL_bands(2),V_bands(:,iv,ik,is),V_tmp) 
     enddo
   enddo
   !
 enddo 
 !
 call PP_redux_wait(VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
 VALUE=VALUE/real(nXkbz,SP)
 !
end subroutine NL_average_operator
