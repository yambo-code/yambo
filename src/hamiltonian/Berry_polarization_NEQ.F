!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine Berry_polarization_NEQ(Xen,Xk,V_bands,Polarization)
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:n_sp_pol,levels
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,nXkbz
 use real_time,      ONLY:RT_bands
 use QP_m,           ONLY:QP_nk
 use electric,       ONLY:All_S_det,Sm1_plus,Sm1_minus
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none           
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 complex(SP),   intent(in)  :: V_bands(RT_bands(2),Xen%nbf,QP_nk,n_sp_pol)
 complex(SP),   intent(out) :: Polarization(3)
 !
 ! Work Space
 !
 integer       :: id,is,ik_bz
 complex(SP)   :: Polarization_red(3)
 !
#if defined _TIMING
 call timing('NL Berry Pol NEQ',OPR='start')
#endif
 !
 Sm1_minus=cZERO
 Sm1_plus =cZERO
 All_S_det=cZERO
 !
 do ik_bz=1,nXkbz
   !
   if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   do is=1,n_sp_pol
      call Build_Overlaps_det_NEQ(Xen,Xk,is,ik_bz,V_bands,Sm1_plus(:,:,:,ik_bz,is),Sm1_minus(:,:,:,ik_bz,is), &
&         All_S_det(:,ik_bz,is)) 
   enddo
   !
 enddo
 !
 do is=1,n_sp_pol
   call PP_redux_wait(Sm1_minus(:,:,:,:,is),COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
   call PP_redux_wait( Sm1_plus(:,:,:,:,is),COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
 enddo
 call PP_redux_wait(All_S_det,COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
 call Berry_polarization_EQ(Polarization_Red,All_S_det,Xen,Xk,k_map)
 !
 ! Transform in cartesian coordinates
 !
!$omp parallel do default(shared), private(id)
 do id=1,3
   Polarization(id)=sum(Polarization_red(:)*a(:,id))/DL_vol
 enddo
!$omp end parallel do
 !
#if defined _TIMING
 call timing('NL Berry Pol NEQ',OPR='stop')
#endif
 !
end subroutine Berry_polarization_NEQ
