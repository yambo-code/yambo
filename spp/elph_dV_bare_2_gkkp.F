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
 subroutine elph_dV_bare_2_gkkp(k,iq,k_found)
!============================================
 use pars,          ONLY:SP
 use D_lattice,     ONLY:nsym,i_time_rev,sop_inv
 use R_lattice,     ONLY:bz_samp,g_rot
 use ELPH,          ONLY:ph_modes,elph_bands,elph_dV,elph_dV_bare
 use wave_func,     ONLY:wf_load,wf_ng,wf_state,wf,wf_ncx

 implicit none
 type(bz_samp) :: k
 integer       :: iq,k_found(k%nbz)
!
!Work Space GPL_END
 integer     :: ik_bz,ik_ibz,ik_is_m1,ib1,ib2,il,ifft,ig
#if defined _DOUBLE
 complex(SP) :: zdotc
#else
 complex(SP) :: cdotc
#endif
 complex(SP) :: wf_rotated(wf_ng),dV_bare(wf_ng)
 !
 !WF loading (G-space)
 !
 if (iq==1) call wf_load(0,1,elph_bands,(/1,k%nibz/),title='-ELPH',space='G')
 !
! elph_dV=(0.,0.)
 !
 do ik_bz=1,k%nbz
   !
   if (k_found( ik_bz )==0) cycle
   !
   ik_ibz=k%sstar(ik_bz,1)
   ik_is_m1 = sop_inv(k%sstar(ik_bz,2))
   !
   do ib1=elph_bands(1),elph_bands(2)
     !
     ifft=wf_state(ib1,ik_ibz,1)
     !
     if (ik_is_m1>nsym/(i_time_rev+1)) then
       ik_is_m1=ik_is_m1-nsym/(i_time_rev+1)
       do ig=1,wf_ng
         wf_rotated(ig)=conjg( wf(g_rot(ik_is_m1,ig),ifft) )
       enddo
     else
       do ig=1,wf_ng
         wf_rotated(ig)=wf(g_rot(ik_is_m1,ig),ifft)
       enddo
     endif
     !
     do ib2=elph_bands(1),elph_bands(2)
       !
       do il=1,ph_modes
         !
         do ig=1,wf_ng
           dV_bare(ig)=elph_dV_bare(ig,ik_bz,ib2-elph_bands(1)+1,il)
         enddo
         !
         elph_dV(ik_bz,il,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)=&
&                              cdotc(wf_ng,wf_rotated,1,dV_bare,1)
         !
!DEBUG
! if (iq==2.and.il==1.and.ib1==1.and.ib2==1.and.ik_bz==2) then
!   write (100,'(f20.10)') real(wf_rotated)
!   write (200,'(f20.10)') aimag(wf_rotated)
!   write (300,'(f20.10)') real(dV_bare)
!   write (400,'(f20.10)') aimag(dV_bare)
!   write (999,'(/7i3)') iq,ik_bz,ib1,ib2,il,ik_ibz,ik_is_m1
!   write (999,'(2f20.15)') elph_dV(ik_bz,il,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)
!   write (999,'(2f20.15)') cdotc(wf_ng,wf_rotated,1,dV_bare,1)
!   write (100,'(2f20.15)') elph_dV(ik_bz,il,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)
!   write (200,'(2f20.15)') cdotc(wf_ng,wf_rotated,1,dV_bare,1)
! endif
         !
       enddo
     enddo
   enddo
 enddo

! GPL_START

 end subroutine

