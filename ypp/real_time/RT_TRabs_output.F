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
subroutine RT_TRabs_output(what,how,ofile,N_dirs,N_files,W,TIME,l_OFF,l_ABS)
 !
 use pars,            ONLY:SP,schlen,pi,cI
 use units,           ONLY:HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI,HA2EV
 use OUTPUT_simple,   ONLY:OUTPUT_add_column
 use RT_output_m,     ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use interfaces,      ONLY:DESC_write
 use YPP_real_time,   ONLY:TRabsWHAT,TRabsMODE,Epsilon_ij,B_Hall,TRabsDIP_plane
 use frequency,       ONLY:w_samp
 use com,             ONLY:msg
 use BS,              ONLY:BS_K_dim
 use BS_solvers,      ONLY:BSS_desc
 use stderr,          ONLY:STRING_match,set_real_printed_length
 !
#include<memory.h>
 !
 character(*)      :: what,how
 character(schlen) :: ofile(20)
 integer           :: N_dirs,N_files
 type(w_samp)      :: W
 real(SP)          :: TIME
 logical           :: l_OFF,l_ABS
 !
 ! Work Space
 !
 logical           :: l_TRANS,l_REFL
 integer           :: i_freq,i_f,i_dir,j_dir
 real(SP)          :: B_data(3)
 !
 l_TRANS=STRING_match(TRabsWHAT,"trans")
 l_REFL =STRING_match(TRabsWHAT,"refl")
 !
 if (what=="init") then
   do i_dir=1,N_dirs
     do j_dir=1,N_dirs
       if (l_ABS.and.i_dir==j_dir) call gimme_a_name(i_dir,j_dir,how)
       if (l_OFF.and.i_dir <j_dir) call gimme_a_name(i_dir,j_dir,how)
     enddo
   enddo
   if (l_OFF) then
     N_files=N_files+1
     ofile(N_files)="YPP-TR_Anomalous_Hall_"//how
   endif
   return
 endif
 !
 if (what=="open".or.what=="close") then
   do i_f=1,N_files
    call OUTPUT_add_column(trim(ofile(i_f)),action=what)
    if (what=="open") then
      call elemental_desc_write(trim(ofile(i_f)))
    endif
   enddo
   return
 endif
 !
 call set_real_printed_length(f_length=12,g_length=15)
 !
 ! Epsilon
 do i_dir=1,N_dirs
   do j_dir=1,N_dirs
     if ((l_ABS.and.i_dir==j_dir).or.(l_OFF.and.i_dir <j_dir)) then 
       do i_freq=1,W%n_freqs
         call elemental_TIME_E_write("eps_"//label(i_dir,j_dir)//"_"//how,&
&                                   (/aimag(Epsilon_ij(i_freq,i_dir,j_dir)),&
&                                      real(Epsilon_ij(i_freq,i_dir,j_dir))/))
       enddo
     endif
   enddo
 enddo
 !
 ! Hall coefficient
 if (l_OFF) then
   call OUTPUT_add_column("Hall_"//how,TITLES=(/"Time"/),R_VALUES=(/TIME/),KIND="Time",UNIT="fs")
   B_Hall=B_Hall*HA2EV/(4*pi*cI*HBAR_eVfs)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   if (N_dirs==2) then
     B_data(1)=real(B_Hall(1,2))
     call OUTPUT_add_column("Hall_"//how,TITLES=(/"B_"//trim(TRabsDIP_plane)/),R_VALUES=B_data(1:1),UNIT="(Ohm*cm)^-1")
   endif
   if (N_dirs==3) then
     B_data=real((/B_Hall(1,2),B_Hall(1,3),B_Hall(2,3)/))
     call OUTPUT_add_column("Hall_"//how,TITLES=(/"B_xy","B_xz","B_yz"/),R_VALUES=B_Data,UNIT="(Ohm*cm)^-1")
   endif
   call OUTPUT_add_column("Hall_"//how,action="write")
 endif
 !
 call set_real_printed_length( )
 !
 contains
   !
   function label(i,j)
     character(2) :: label
     integer      :: i,j
     if (i==1.and.j==1) label="xx" 
     if (i==2.and.j==2) label="yy" 
     if (i==3.and.j==3) label="zz" 
     if (i==1.and.j==2) label="xy" 
     if (i==1.and.j==3) label="xz" 
     if (i==2.and.j==3) label="yz" 
   end function
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
   subroutine gimme_a_name(i,j,desc)
     integer :: i,j
     character(*) :: desc
     character(10) :: base
     N_files=N_files+1
     base="delta_eps"
     if (STRING_match(TRabsMODE,"eq")) base="eps"
     ofile(N_files)="YPP-TR_"//trim(base)//"_"//label(i,j)//"_"//desc
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
