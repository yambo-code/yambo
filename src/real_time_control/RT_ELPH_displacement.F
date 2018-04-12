!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM 
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
subroutine RT_ELPH_displacement(k,E,dG)
 !
 ! Evalute force potential that define the phonon displacement
 !
 ! D(\nu,t) \propto -\sum_ijk <\psi_i(k)(r)| dV_\nu\(r)|\psi_j(k)(r)> \delta \gr_{ji}(t)
 !
 use pars,           ONLY:SP,cZERO,pi,cI
 use R_lattice,      ONLY:bz_samp,nqbz
 use electrons,      ONLY:levels
 use real_time,      ONLY:l_NE_dynamics,l_RT_PH_oscillations,RT_bands,elph_gkkp_gamma,RT_nk,&
&                         RT_D_pol,RT_D_occ,RT_PH_displaced,N_RT_PH_displaced,elph_gkkp_gamma_diagonal,&
&                         RT_Nk_ibz,RT_ibz_coo
 use ELPH,           ONLY:ph_freqs_sq,ph_modes
 use RT_interfaces,  ONLY:RT_average_operator
 use parallel_m,     ONLY:PAR_Xk_nibz,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PP_redux_wait,PAR_Xk_ibz_index
 !
 implicit none
 !
 type(bz_samp),     intent(in) :: k
 type(levels),      intent(in) :: E
 complex(SP),       intent(in) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Spave
 !
 integer     :: il,ic,ik,ib,ik_rim,ik_mem
 real(SP)    :: ph_freq,POL
 !
 ! GPL_EXCLUDE_START
 !
 if ( .not. l_NE_dynamics .or. .not. l_RT_PH_oscillations ) return
 !
 ! Polarization + Occupations
 !
 RT_D_pol=cZERO
 call RT_average_operator(N_V=ph_modes,V_VALUE=RT_D_pol,V_MATRIX=elph_gkkp_gamma,G=dG,k=k,en=E,nK_mem=PAR_Xk_nibz)
 !
 ! Occupations
 !
 RT_D_occ=cZERO
 do ik=1,k%nibz
   ik_mem=PAR_Xk_ibz_index(ik)
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle   
   do il=1,ph_modes
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       do ib=RT_bands(1),RT_bands(2)
          RT_D_occ(il)=RT_D_occ(il)-cI*conjg(elph_gkkp_gamma_diagonal(il,ib,ik_mem))*&
&                                      dG(ib,ib,ik_rim)*k%weights(ik)/real(RT_Nk_ibz(ik))
        enddo
     enddo
   enddo
 enddo
 call PP_redux_wait(RT_D_occ,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 do ic=1,N_RT_PH_displaced
   !
   il=RT_PH_displaced(ic)
   !
   ph_freq=abs(sqrt(ph_freqs_sq(1,il)))
   !
   POL=RT_D_pol(il)-RT_D_occ(il)
   RT_D_pol(il)=-1/pi*sqrt(ph_freq/nqbz)*POL
   RT_D_occ(il)=-1/pi*sqrt(ph_freq/nqbz)*RT_D_occ(il)
   !
 enddo
 !
 ! GPL_EXCLUDE_END
 !
end subroutine RT_ELPH_displacement
