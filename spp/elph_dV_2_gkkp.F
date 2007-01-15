!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine elph_dV_2_gkkp(iq,k2k_tab,X_ng)
!==========================================
 use pars,          ONLY:SP
 use ELPH,          ONLY:ph_modes,elph_nb,elph_dV,elph_dV_psi,elph_gkkp,&
&                        dV_nk,dV_ngx
 use R_lattice,     ONLY:nkbz
 use X_m,           ONLY:X_mat
 implicit none
 integer       :: iq,k2k_tab(nkbz),X_ng
!
!Work Space GPL_END
!
 integer     :: ik,ib1,ib2,il
#if defined _DOUBLE
 complex(SP) :: zdotc
#else
 complex(SP) :: cdotc
#endif
 complex(SP) :: dV_temp(dV_ngx)

 elph_gkkp(:,:,:,:)=(0.,0.)

 if (iq==1) call elph_epsm1_load(X_ng)

 do ik=1,dV_nk

   do ib2=1,elph_nb
 
     do il=1,ph_modes

       dV_temp(:X_ng)=matmul(X_mat(:,:,iq),elph_dV(:X_ng,ib2,ik,il))
       dV_temp(X_ng+1:)=elph_dV(X_ng+1:,ib2,ik,il)

       do ib1=1,elph_nb

         elph_gkkp(k2k_tab(ik),il,ib1,ib2)=&
&                 cdotc(dV_ngx,elph_dV_psi(:,ib1,ik),1,dV_temp,1)

       enddo

     enddo 
   enddo

 enddo

! GPL_START

 end subroutine

