!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
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
subroutine RT_Lifetimes_use_delta_Occupations( )
 !
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt,l_elphoton_scatt
 use electrons,      ONLY:spin_occ
 use pars,           ONLY:SP
 use parallel_m,     ONLY:ncpu,PAR_COM_PLASMA_INDEX
 use real_time,      ONLY:RT_occupations_locked,RT_E_occupations_reference,RT_H_occupations_reference,&
&                         RT_life_delta_occupations,l_RT_relaxation_update_reference,&
&                         RT_H_occupations,RT_E_occupations,RT_delta_occupations,&
&                         SAVED_lifetime,Life_MEM_steps,RT_bands,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,&
&                         E_PHOT_Hole_lifetime,E_PHOT_Electron_lifetime,&
&                         E_P_Hole_lifetime,E_P_Electron_lifetime, &
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,&
&                         G_lesser_reference,RT_nk,l_RT_relaxation_update_reference
 !
 implicit none
 !
 ! Work Space
 !
 integer :: ik,ib
 real(SP):: f_E_occ,f_H_occ,f_E_occ_reference,f_H_occ_reference,df_max
 !
 if (l_RT_relaxation_update_reference) then
   !
   df_max=0._SP
   !
   do ik=1,RT_nk
     do ib=RT_bands(1),RT_bands(2)
       !
       f_E_occ=RT_E_occupations(ib,ik)
       f_H_occ=RT_H_occupations(ib,ik)
       f_E_occ_reference=RT_E_occupations_reference(ib,ik)
       f_H_occ_reference=RT_H_occupations_reference(ib,ik)
       !
       if(f_E_occ<=0.5*spin_occ) RT_life_delta_occupations(ib,ik) =   f_E_occ-f_E_occ_reference
       if(f_E_occ> 0.5*spin_occ) RT_life_delta_occupations(ib,ik) = -(f_H_occ-f_H_occ_reference)
       !
       df_max=max(df_max,abs(RT_life_delta_occupations(ib,ik)))
       !
     enddo
   enddo
   !
 endif
 !
 do ik=1,RT_nk
   do ib=RT_bands(1),RT_bands(2)
     !
     RT_occupations_locked(ib,ik)=.FALSE. 
     !
     if (l_RT_relaxation_update_reference) then
       !
       !DEBUG<
       !if (abs( RT_delta_occupations(ib,ik) )<1.E-4*df_max) RT_occupations_locked(ib,ik)=.TRUE.
       !DEBUG>
       !
       !DEBUG<
       !if( RT_occupations_locked(ib,ik) ) write(*,*) 'state= ',ib,ik,' locked'
       !DEBUG>
       !
       ! March 2015. AndreaM. The npcu fraction here is because the Lifetimes go trough
       ! an ALL2ALL later. BUT the e-p & e-g lifetimes use only ncpu/PAR_COM_Plasma_INDEX%n_CPU CPU's.
       !
       if (l_elphoton_scatt) then
         E_PHOT_Hole_lifetime(ib,ik)     =SAVED_lifetime(ib,ik,7,Life_MEM_steps)/&
&                                         real(ncpu)*real(PAR_COM_PLASMA_INDEX%n_CPU)
         E_PHOT_Electron_lifetime(ib,ik) =SAVED_lifetime(ib,ik,8,Life_MEM_steps)/&
&                                         real(ncpu)*real(PAR_COM_PLASMA_INDEX%n_CPU)
       endif
       if (l_elel_scatt) then
         E_E_Hole_lifetime(ib,ik)     =SAVED_lifetime(ib,ik,1,Life_MEM_steps)/real(ncpu)
         E_E_Electron_lifetime(ib,ik) =SAVED_lifetime(ib,ik,2,Life_MEM_steps)/real(ncpu)
       endif
       if (l_elph_scatt) then
         E_P_Hole_lifetime(ib,ik)     =SAVED_lifetime(ib,ik,3,Life_MEM_steps)/&
&                                      real(ncpu)*real(PAR_COM_PLASMA_INDEX%n_CPU)
         E_P_Electron_lifetime(ib,ik) =SAVED_lifetime(ib,ik,4,Life_MEM_steps)/&
&                                      real(ncpu)*real(PAR_COM_PLASMA_INDEX%n_CPU)
         E_P_Ho_abs_lifetime(ib,ik)   =SAVED_lifetime(ib,ik,5,Life_MEM_steps)/&
&                                      real(ncpu)*real(PAR_COM_PLASMA_INDEX%n_CPU)
         E_P_El_abs_lifetime(ib,ik)   =SAVED_lifetime(ib,ik,6,Life_MEM_steps)/&
&                                      real(ncpu)*real(PAR_COM_PLASMA_INDEX%n_CPU)
       endif
       !
     else
       !
       RT_life_delta_occupations(ib,ik) =RT_delta_occupations(ib,ik)
       RT_E_occupations_reference(ib,ik)=         aimag(G_lesser_reference(ib,ib,ik))
       RT_H_occupations_reference(ib,ik)=spin_occ-aimag(G_lesser_reference(ib,ib,ik))
       !
       if (l_elphoton_scatt) then
         E_PHOT_Hole_lifetime(ib,ik)=    0._SP
         E_PHOT_Electron_lifetime(ib,ik)=0._SP
       endif
       if (l_elel_scatt) then
         E_E_Hole_lifetime(ib,ik)=    0._SP
         E_E_Electron_lifetime(ib,ik)=0._SP
       endif
       if (l_elph_scatt) then
         E_P_Hole_lifetime(ib,ik)=    0._SP
         E_P_Electron_lifetime(ib,ik)=0._SP
         E_P_Ho_abs_lifetime(ib,ik)=  0._SP
         E_P_El_abs_lifetime(ib,ik)=  0._SP
       endif
     endif
     !
   enddo
 enddo
 !
end subroutine RT_Lifetimes_use_delta_Occupations
