!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine OCCUPATIONS_out_of_equilibrium(Xk,Xen,En,Ken)
 !
 use pars,           ONLY:SP
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels,n_sp_pol,E_duplicate
 use R_lattice,      ONLY:bz_samp,nkibz
 use drivers,        ONLY:l_rt_bse
 use real_time,      ONLY:RT_BSE_time,rt
 use rt_ctl,         ONLY:RT_OCC_LIFE_and_REF_IO,delta_f
 use parser_m,       ONLY:parser
 use com,            ONLY:msg
 !
 implicit none
 !
 type(bz_samp)::XK
 type(levels) ::Xen,En,Ken
 !
 type(levels)      :: RT_db1_E
 type(bz_samp)     :: RT_db1_k
 !
 real(SP)     :: N_carriers,Max_change
 !
 integer      :: n_T_steps,nkibz_save,ik,i1
 logical      :: l_update_screening
 !
 call parser('RT_BSE',l_rt_bse)
 !
 if(.not.l_RT_BSE) return
 !
 call parser('UpScr',l_update_screening)
 !
 ! Check the RT DBs and load the dimensions. Also chech the time boundaries
 call RT_variables_load(Xen,Xk,RT_db1_E,RT_db1_k,n_T_steps)
 !
 nkibz_save=nkibz
 ! Now load the occupations, the lifetimes, etc at the correct time
 call RT_OCC_LIFE_and_REF_IO(RT_db1_E,'G',k=RT_db1_k,Time=RT_BSE_time)
 call RT_alloc_def_or_free(rt,RT_db1_E,RT_db1_k,2)
 nkibz=nkibz_save
 !
 ! Fit them on the grid for absorption
 ! For the moment only the delta_f variable is considered for the occupations
 allocate(delta_f(rt%nb(1):rt%nb(2),Xk%nibz,n_sp_pol))
 call Nearest_kpt_interpolation(rt%nk,Xk%nibz,rt%nb,rt%nstates,1,1,rt%table,rt%k,Xk%pt,rt%wk,Xk%weights,rt%df,delta_f,'BSE',.true.)
 !
 Max_change=maxval(abs(delta_f))
 N_carriers=0._SP
 do ik=1,Xk%nibz
   N_carriers=N_carriers+sum(abs(delta_f(:,ik,:)))*Xk%weights(ik)/2._SP
 enddo
 !
 ! Finally update the occupations and the bands range
 call E_duplicate(Xen,Ken)
 !
 Xen%nbm=max(rt%nb(2)  ,Xen%nbm)
 Xen%nbf=min(rt%nb(1)-1,Xen%nbf)
 if(l_update_screening) Xen%f(rt%nb(1):rt%nb(2),:,:)=Xen%f(rt%nb(1):rt%nb(2),:,:)+delta_f(rt%nb(1):rt%nb(2),:,:)
 !
 En%nbm=max(rt%nb(2)  ,En%nbm)
 En%nbf=min(rt%nb(1)-1,En%nbf)
 En%f(rt%nb(1):rt%nb(2),:,:)=En%f(rt%nb(1):rt%nb(2),:,:)+delta_f(rt%nb(1):rt%nb(2),:,:)
 !
 Ken%nbm=max(rt%nb(2)  ,Ken%nbm)
 Ken%nbf=min(rt%nb(1)-1,Ken%nbf)
 Ken%f(rt%nb(1):rt%nb(2),:,:)=Ken%f(rt%nb(1):rt%nb(2),:,:)+delta_f(rt%nb(1):rt%nb(2),:,:)
 !
 deallocate(delta_f)
 call RT_alloc_def_or_free(rt,RT_db1_E,RT_db1_k,3)
 !
 call msg('rns','[RT] Updated occupations using f(t) from NEQ run at time [fs]: ',RT_BSE_time/FS2AUT)
 call msg('r',  '[RT] N full bands, N metallic bands                          : ', (/Ken%nbf,Ken%nbm/)) 
 call msg('r',  '[RT] Max change in occupations / N of carriers per cell      : ', (/Max_change,N_carriers/)) 
 !
end subroutine
