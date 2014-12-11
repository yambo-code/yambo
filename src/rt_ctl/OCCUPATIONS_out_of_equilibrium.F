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
subroutine OCCUPATIONS_out_of_equilibrium(bands_limit,En,Xk,call_kind)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_corr
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,nkibz
 use real_time,      ONLY:RT_PP_time,RT_carriers,RIM_ibz_coo,RIM_bz_coo,RT_ibz_coo,RT_Nk_ibz,RT_all2ibz
 use rt_ctl,         ONLY:RT_OCC_LIFE_and_REF_IO,delta_f
 use com,            ONLY:msg
 use interfaces,     ONLY:RT_carriers_object
 !
 implicit none
 !
 integer      :: bands_limit(2)
 type(levels) :: En
 type(bz_samp):: Xk
 character(3) :: call_kind
 !
 ! Work space
 !
 type(levels)      :: RT_db1_E
 type(bz_samp)     :: RT_db1_k
 !
 real(SP)     :: N_carriers,Max_change
 !
 integer      :: n_T_steps,nkibz_save,ik,i1,bnd(2)
 logical      :: l_elel_corr_save
 !
 l_elel_corr_save=l_elel_corr
 nkibz_save      =nkibz
 !
 ! Check the RT DBs and load the dimensions. Also chech the time boundaries
 call RT_variables_load(En,Xk,RT_db1_E,RT_db1_k,n_T_steps)
 !
 ! Now load the occupations, the lifetimes, etc at the correct time
 call RT_OCC_LIFE_and_REF_IO(RT_db1_E,'G',k=RT_db1_k,Time=RT_PP_time)
 call RT_carriers_object(RT_carriers,WHAT='fill')
 !
 nkibz      =nkibz_save
 l_elel_corr=l_elel_corr_save
 !
 ! Fit them on the grid for absorption
 ! For the moment only the delta_f variable is considered for the occupations
 allocate(delta_f(RT_carriers%nb(1):RT_carriers%nb(2),Xk%nibz,n_sp_pol))
 call Nearest_kpt_interpolation(RT_carriers%nk,Xk%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                               RT_carriers%kpt,Xk%pt,RT_carriers%k_weight,Xk%weights,RT_carriers%delta_f,delta_f,call_kind,.true.)
 !
 Max_change=maxval(abs(delta_f))
 N_carriers=0._SP
 do ik=1,Xk%nibz
   N_carriers=N_carriers+sum(abs(delta_f(:,ik,:)))*Xk%weights(ik)/2._SP
 enddo
 !
 bnd(1)=max(Bands_limit(1),RT_carriers%nb(1))
 bnd(2)=min(Bands_limit(2),RT_carriers%nb(2))
 !
 En%nbm=max(RT_carriers%nb(2)  ,En%nbm)
 En%nbf=min(RT_carriers%nb(1)-1,En%nbf)
 En%f( bnd(1):bnd(2),:,:)=En%f(bnd(1):bnd(2),:,:)+delta_f(bnd(1):bnd(2),:,:)
 !
 deallocate(delta_f)
 call RT_carriers_object(RT_carriers,WHAT='free')
 if(allocated(RIM_ibz_coo)) deallocate(RIM_ibz_coo,RIM_bz_coo,RT_ibz_coo,RT_Nk_ibz,RT_all2ibz)
 !
 call msg('nrs','[RT] Updated occupations using f(t) from NEQ run at time [fs]: ',RT_PP_time/FS2AUT)
 call msg('r',  '[RT] N of bands updated                                      : ', (/bnd(1),bnd(2)/)) 
 call msg('r',  '[RT] Max change in occupations / N of carriers per cell      : ', (/Max_change,N_carriers/)) 
 call msg('r',  '[RT] N full bands, N metallic bands                          : ', (/En%nbf,En%nbm/)) 
 !
 call REPORT_OCCUPATIONS(En)
 !
end subroutine
