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
subroutine NL_build_dG_lesser(en,V_bands,dG) 
 !
 ! Construct dG = G_lesser(t) - G_lesser(t=0) 
 ! from the time-dependent valence bands
 !
 use pars,        ONLY:SP,cI,cZERO
 use electrons,   ONLY:levels,spin_occ,n_sp_pol
 use nl_optics,   ONLY:NL_bands
 use wrapper_omp, ONLY:Vstar_dot_V_omp
 use QP_m,        ONLY:QP_nk
 use parallel_m,  ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
#if defined _TIMING
 use timing_m,         ONLY:timing
#endif
 !
 implicit none
 !
 type(levels), intent(in)  :: en
 complex(SP),  intent(in)  :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 complex(SP),  intent(out) :: dG(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer :: i1,i2
 integer :: ik
 !
#if defined _TIMING
 call timing('NL build_dG',OPR='start')
#endif
 !
 dG=cZERO
 !
 do ik=1,QP_nk
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   do i1=NL_bands(1),NL_bands(2)             
     do i2=i1,NL_bands(2)
       dG(i1,i2,ik)=Vstar_dot_V_omp(en%nbf,V_bands(i2,:,ik,1),V_bands(i1,:,ik,1))
       dG(i2,i1,ik)=conjg(dG(i1,i2,ik))
     enddo
   enddo
 enddo
 !
 dG=cI*spin_occ*dG
 !
 call PP_redux_wait(dG,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 !
 do i1=NL_bands(1),en%nbf
   dG(i1,i1,:)=dG(i1,i1,:)-cI*spin_occ
 enddo
 !
#if defined _TIMING
 call timing('NL build_dG',OPR='stop')
#endif
 !
end subroutine NL_build_dG_lesser
