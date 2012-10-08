!
! Copyright (C) 2000-2011 D. Sangalli and the YAMBO team 
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
subroutine K_kerr_output()
 !
 use pars,          ONLY:SP,pi,cI
 use memory_m,      ONLY:mem_est
 use units,         ONLY:DEG2RAD,HA2EV,HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI
 use KERR,          ONLY:BSS_rhoq0_kerr
 use X_m,           ONLY:X_epsilon
 use X_output,      ONLY:off_file_name,kerr_file_name,X_setup_headers,headers
 use BS,            ONLY:BSS_n_freqs,BSS_mode
 use drivers,       ONLY:l_kerr,l_optics,l_bse,l_bss
 use KERR,          ONLY:KERR_alloc,l_anomalous_Hall,B_Hall
 use com,           ONLY:msg,of_open_close,warning,error
 !
 implicit none
 !
 integer           :: iw
 real(SP)          :: output_data(5)
 !
 ! Kerr parameters
 !
 X_epsilon(6,:)=-X_epsilon(4,:)/( (X_epsilon(3,:)-1.0_SP)*sqrt(X_epsilon(3,:)) ) /DEG2RAD
 if(.not.trim(BSS_mode)=='none') &
&   X_epsilon(7,:)=-X_epsilon(5,:)/( (X_epsilon(2,:)-1.0_SP)*sqrt(X_epsilon(2,:)) ) /DEG2RAD
 !
 ! 1 - Headers
 !
 ! File names are defined in mod_X_output
 ! If .not. l_kerr I need to destroy the names
 if (.not.l_kerr) then
   off_file_name=' '
   kerr_file_name=' '
 endif
 call of_open_close(off_file_name,'ot')
 call of_open_close(off_file_name,'oa')
 !
 call msg('o off','#')
 call X_setup_headers('off')
 call msg('o off','#',headers(:5),INDENT=0,USE_TABS=.true.)    
 !call msg('o off','#',headers(:3),INDENT=0,USE_TABS=.true.)    
 call msg('o off','#')
 !
 call of_open_close(off_file_name)
 call of_open_close(kerr_file_name)
 !
 ! 2 - Data
 !
 !! epsilon_xy
 call of_open_close(off_file_name,'oa')
 !
 do iw=1,BSS_n_freqs 
   output_data=(/real(X_epsilon(1,iw))*HA2EV,aimag(X_epsilon(5,iw)),real(X_epsilon(5,iw)),&
&                  aimag(X_epsilon(4,iw)),real(X_epsilon(4,iw))/)
   !call msg('o off','',output_data(:3),INDENT=-2,USE_TABS=.true.)
   call msg('o off','',output_data,INDENT=-2,USE_TABS=.true.)
 enddo
 !
 if(l_anomalous_Hall) then
   call msg('o off','#','Anomalous Hall effect sH',INDENT=0,USE_TABS=.true.)
   headers(:4)=(/'sH ','   ','sH0','   '/)
   call msg("o off",'#',headers(:4),INDENT=0,USE_TABS=.TRUE.)
   headers(:4)=(/'  [fs]^-1  ','[Ohm*cm]^-1','  [fs]^-1  ','[Ohm*cm]^-1'/)
   call msg("o off",'#',headers(:4),INDENT=0,USE_TABS=.TRUE.)
   !
   B_Hall(:)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
   output_data(1)=real(B_Hall(2))
   output_data(3)=real(B_Hall(1))
   B_Hall(:)=B_Hall(:)*1.E5/(SPEED_OF_LIGHT_SI)**2/FEMTO
   output_data(2)=real(B_Hall(2))
   output_data(4)=real(B_Hall(1))
   call msg('o off','#',output_data(:4),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
 call of_open_close(off_file_name)
 !
 !! kerr_parameters
 !
 call of_open_close(kerr_file_name,'ot')
 call of_open_close(kerr_file_name,'oa')
 !
 call msg('o kerr','#')
 call X_setup_headers('kerr')
 call msg('o kerr','#',headers(:5),INDENT=0,USE_TABS=.true.)    
 !call msg('o kerr','#',headers(:3),INDENT=0,USE_TABS=.true.)    
 call msg('o kerr','#')
 !
 call of_open_close(kerr_file_name,'oa')
 !
 do iw=1,BSS_n_freqs 
   output_data=(/real(X_epsilon(1,iw))*HA2EV,real(X_epsilon(7,iw)),aimag(X_epsilon(7,iw)),&
&                  real(X_epsilon(6,iw)),aimag(X_epsilon(6,iw))/)
   !call msg('o kerr','',output_data(:3),INDENT=-2,USE_TABS=.true.)
   call msg('o kerr','',output_data,INDENT=-2,USE_TABS=.true.)
 enddo
 !
 call of_open_close(kerr_file_name)
 !
 ! CLEAN
 !
 call Kerr_alloc('DIP_q_dot_iR')
 !
 deallocate(BSS_rhoq0_kerr)
 call mem_est("BSS_rhoq0_kerr")
 !
end subroutine
