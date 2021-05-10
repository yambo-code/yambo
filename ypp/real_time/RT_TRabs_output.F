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
subroutine RT_TRabs_output(what,N_col,W,TIME,l_BS,l_IP,l_OFF,l_ABS)
 !
 use pars,            ONLY:SP,schlen
 use OUTPUT_simple,   ONLY:OUTPUT_add_column
 use RT_output_m,     ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use interfaces,      ONLY:DESC_write
 use YPP_real_time,   ONLY:TRabsWHAT
 use X_m,             ONLY:Epsilon_ii
 use frequency,       ONLY:w_samp
 use BS,              ONLY:BS_K_dim
 use BS_solvers,      ONLY:BSS_desc
 use stderr,          ONLY:STRING_match
 !
#include<memory.h>
 !
 character(*) :: what
 integer      :: N_col
 type(w_samp) :: W
 real(SP)     :: TIME
 logical      :: l_BS,l_IP,l_OFF,l_ABS
 !
 ! Work Space
 !
 logical           :: l_TRANS,l_REFL
 integer           :: i_freq,i_f,n_files
 character(schlen) :: ofile(10)
 !
 l_TRANS=STRING_match(TRabsWHAT,"trans")
 l_REFL =STRING_match(TRabsWHAT,"refl")
 !
 n_files=0
 if (l_ABS) then
   if (l_BS)  n_files=n_files+1
   if (l_BS)  ofile(n_files)="YPP-TR_delta_eps_BS"
   if (l_IP)  n_files=n_files+1
   if (l_IP)  ofile(n_files)="YPP-TR_delta_eps_IP"
 endif
 if (l_OFF) then
   if (l_BS)  n_files=n_files+1
   if (l_BS)  ofile(n_files)="YPP-TR_delta_off_BS"
   if (l_IP)  n_files=n_files+1
   if (l_IP)  ofile(n_files)="YPP-TR_delta_off_IP"
 endif
 !
 if (what=="open".or.what=="close") then
   do i_f=1,n_files
    call OUTPUT_add_column(trim(ofile(i_f)),action=what)
    if (what=="open") then
      call elemental_desc_write(trim(ofile(i_f)))
    endif
   enddo
   return
 endif
 !
 do i_f=1,n_files
   do i_freq=1,W%n_freqs
     call elemental_TIME_E_write(trim(ofile(i_f)),(/aimag(Epsilon_ii(i_freq,i_f)),real(Epsilon_ii(i_freq,i_f))/))
   enddo
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
     if (BS_K_dim(1)>0) call DESC_write('o '//file,'#',BSS_desc)
   end subroutine
   !
end subroutine RT_TRabs_output
