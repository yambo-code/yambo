!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_TRabs_output(what,W,TIME,l_BS,l_IP,l_OFF)
 !
 use pars,           ONLY:SP
 use OUTPUT_simple,  ONLY:OUTPUT_add_column
 use RT_output_m,    ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use interfaces,     ONLY:DESC_write
 use YPP_real_time,  ONLY:TRabsWHAT
 use X_m,            ONLY:Epsilon_ii
 use frequency,      ONLY:w_samp
 use stderr,         ONLY:STRING_match
 !
#include<memory.h>
 !
 character(*) :: what
 type(w_samp) :: W
 real(SP)     :: TIME
 logical      :: l_BS,l_IP,l_OFF
 !
 ! Work Space
 !
 logical     :: l_TRANS,l_REFL
 integer     :: i_freq,N_col
 real(SP)    :: output(2)
 !
 l_TRANS=STRING_match(TRabsWHAT,"trans")
 l_REFL =STRING_match(TRabsWHAT,"refl")
 !
 if (what=="open".or.what=="close") then
   !
   if (l_BS) call OUTPUT_add_column("YPP-TR_delta_eps_BS",action=what)
   if (l_IP) call OUTPUT_add_column("YPP-TR_delta_eps_IP",action=what)
   if (l_OFF) then
     if (l_BS) call OUTPUT_add_column("YPP-TR_delta_off_BS",action=what)
     if (l_IP) call OUTPUT_add_column("YPP-TR_delta_off_IP",action=what)
   endif
   !
   !if (l_TRANS) call RT_output(what//" TRtransmission")
   !if (l_REFL ) call RT_output(what//" TRreflectivity")
   !
   if (what=="open") then
     call elemental_desc_write('delta_eps_BS')
     call elemental_desc_write('delta_eps_IP')
     call elemental_desc_write('delta_off_BS')
     call elemental_desc_write('delta_off_IP')
   endif
   !
   return
   !
 endif
 !
 do i_freq=1,W%n_freqs
   N_col=0
   if (l_BS) then
     N_col=1
     call elemental_TIME_E_write("delta_eps_BS",(/aimag(Epsilon_ii(i_freq,1)),real(Epsilon_ii(i_freq,1))/))
   endif
   if (l_IP) then
     N_col=N_col+1
     call elemental_TIME_E_write("delta_eps_IP",(/aimag(Epsilon_ii(i_freq,N_col)),real(Epsilon_ii(i_freq,N_col))/))
   endif
 enddo
 !
 contains
   !
   subroutine elemental_TIME_E_write(file,data)
     character(*) :: file
     real(SP)     :: data(2)
     call OUTPUT_add_column(file,TITLES=(/"Time"/),R_VALUES=(/TIME/),UNIT="fs",KIND="Time")
     call OUTPUT_add_column(file,TITLES=(/"Energy"/),R_VALUES=(/real(W%p(i_freq))/),UNIT="eV")
     call OUTPUT_add_column(file,TITLES=(/"Eps-Im","Eps-Re"/),R_VALUES=data)
     call OUTPUT_add_column(file,action="write")
   end subroutine
   !
   subroutine elemental_desc_write(file)
     character(*) :: file
     call DESC_write('o '//file,'#',RT_desc)     
     call DESC_write('o '//file,'#',Gr_desc)     
     call DESC_write('o '//file,'#',TIME_STEP_desc)     
   end subroutine
   !
end subroutine RT_TRabs_output
