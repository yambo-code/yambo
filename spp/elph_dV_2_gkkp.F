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
 subroutine elph_dV_2_gkkp(iq,k2k_tab)
!=====================================
 use pars,          ONLY:SP
 use ELPH,          ONLY:ph_modes,elph_nb,elph_dV,elph_dV_psi,elph_gkkp,&
&                        dV_nk,dV_ngx
 use R_lattice,     ONLY:nkbz
 implicit none
 integer       :: iq,k2k_tab(nkbz)
!
!Work Space GPL_END
!
 integer     :: ik,ib1,ib2,il
#if defined _DOUBLE
 complex(SP) :: zdotc
#else
 complex(SP) :: cdotc
#endif

 elph_gkkp(:,:,:,:)=(0.,0.)
 
 do ik=1,dV_nk

   do ib1=1,elph_nb
     do ib2=1,elph_nb
 
       do il=1,ph_modes

         elph_gkkp(k2k_tab(ik),il,ib1,ib2)=&
&                 cdotc(dV_ngx,elph_dV_psi(:,ib1,ik),1,elph_dV(:,ib2,ik,il),1)

       enddo

     enddo 
   enddo

 enddo

! GPL_START

 end subroutine

