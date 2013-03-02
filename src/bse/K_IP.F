!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine K_IP(iq,Ken,Xk,Wd)
 !
 use pars,         ONLY:SP,IP
 use LOGO,         ONLY:pickup_a_random
 use memory_m,     ONLY:mem_est
 use electrons,    ONLY:levels,n_sp_pol
 use R_lattice,    ONLY:bz_samp
 use com,          ONLY:error
 use BS,           ONLY:BS_eh_win,BS_columns,BS_eh_E,BS_identifier, &
&                       BS_blk_dim
 !
 implicit none
 type(levels)  ::Ken 
 type(bz_samp) ::Xk
 integer       ::iq
 !
 complex(SP):: Wd
 real(SP)   :: E_eh_range(2),S_eh_range(2)
 !
 ! Setups
 !
 if (BS_columns<=0.or.BS_columns>Xk%nibz) BS_columns=Xk%nibz
 !
 ! Dimensions and Tables
 ! 
 allocate(BS_blk_dim(Xk%nibz))
 !
 call mem_est("BS_blk_dim",(/Xk%nbz/),(/IP/))
 !
 call K_eh_setup(iq,Ken,Xk,Wd)
 if (any(BS_blk_dim==0)) then
   call error(' Null BSE kernel block dimension(s) found. Increase e/h range')
 endif
 !
 E_eh_range=(/minval(abs(BS_eh_E))-1.E-5,maxval(abs(BS_eh_E))/)
 S_eh_range=(/BS_eh_win(1)/100.*(E_eh_range(2)-E_eh_range(1)),& 
&             BS_eh_win(2)/100.*(E_eh_range(2)-E_eh_range(1))/)
 !
 ! Test the spatial Inversion
 !
 call WF_spatial_invertion(Ken,Xk)
 !
 ! DB identifier 
 !
 BS_identifier=pickup_a_random(10000._SP)
 !
 call section('+','Main loop')
 !
end subroutine
