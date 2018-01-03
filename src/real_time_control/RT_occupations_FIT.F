! 
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_occupations_FIT(T_fit,Ef_fit,Occ_bk)
 !
 ! Check if both the electrons and the holes are below a given temperature
 ! In case locks the occupations of states which are not expected to change
 ! any more
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_carriers
 use functions,      ONLY:Fermi_fnc_fit
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 real(SP), intent(in)    :: Occ_bk(RT_carriers%nb(1):RT_carriers%nb(2),RT_carriers%nk)   
 real(SP), intent(inout) :: T_fit(2),Ef_fit(2)
 !
 real(SP) :: Occ_rt(RT_carriers%nstates)
 integer  :: nfit,i_RT,ib,ik_RT
 real(SP) :: err_fit(4)
 !
#if defined _TIMING
 call timing('RT occupations fit',OPR='start')
#endif
 !
 do i_RT=1,RT_carriers%nstates
   ib   =RT_carriers%table(i_RT,1)
   ik_RT=RT_carriers%table(i_RT,3)
   Occ_rt(i_RT)=Occ_bk(ib,ik_RT)
 enddo
 !
 nfit=Fermi_fnc_fit(Occ_rt,RT_carriers%nstates,T_fit,Ef_fit,err_fit)
 !
 if(err_fit(1)>0.05) then
   T_fit(1) =-T_fit(1)
   Ef_fit(1)=0._SP
 endif
 if(err_fit(2)>0.05) then
   T_fit(2) =-T_fit(2)
   Ef_fit(2)=0._SP
 endif
 !
#if defined _TIMING
 call timing('RT occupations fit',OPR='stop')
#endif
 !
 return
 !
end subroutine RT_occupations_FIT
