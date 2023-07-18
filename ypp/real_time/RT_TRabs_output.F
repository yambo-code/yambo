!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine RT_TRabs_output(what,how,ofile,N_files,W,TIME)
 !
 use pars,            ONLY:SP,schlen,pi,cI
 use units,           ONLY:HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI,HA2EV
 use OUTPUT,          ONLY:OUTPUT_driver
 use RT_output_m,     ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use interfaces,      ONLY:DESC_write
 use YPP_real_time,   ONLY:TRabsWHAT,TRabsMODE,Epsilon_ij,B_Hall,TRabs_EPS_label,&
&                          TRabs_lEPS
 use frequency,       ONLY:w_samp
 use BS,              ONLY:BS_K_dim
 use BS_solvers,      ONLY:BSS_desc
 use stderr,          ONLY:STRING_match,set_real_printed_length
 !
#include<memory.h>
 !
 character(*)      :: what,how
 character(schlen) :: ofile(20)
 integer           :: N_files
 type(w_samp)      :: W
 real(SP)          :: TIME
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
   do i_dir=1,3
     do j_dir=1,3
       if (TRabs_lEPS(i_dir,j_dir).and.STRING_match(TRabsWHAT,"abs"))  call gimme_a_name(i_dir,j_dir,how)
     enddo
   enddo
   if (allocated(B_Hall)) then
     N_files=N_files+1
     ofile(N_files)="YPP-TR_Anomalous_Hall_"//how
   endif
   return
 endif
 !
 if (what=="open".or.what=="close") then
   do i_f=1,N_files
    call OUTPUT_driver(trim(ofile(i_f)),action=what)
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
 if (STRING_match(TRabsWHAT,"abs")) then
   do i_dir=1,3
     do j_dir=1,3
       if (.not.TRabs_lEPS(i_dir,j_dir)) cycle
       do i_freq=1,W%n_freqs
         call elemental_TIME_E_write("eps_"//TRabs_EPS_label(i_dir,j_dir)//"_"//how,&
&                                   (/aimag(Epsilon_ij(i_freq,i_dir,j_dir)),&
&                                      real(Epsilon_ij(i_freq,i_dir,j_dir))/))
       enddo
     enddo
   enddo
 endif
 !
 ! Hall coefficient
 if (STRING_match(TRabsWHAT,"kerr").and.allocated(B_Hall)) then
   call OUTPUT_driver("Hall_"//how,TITLES=(/"Time"/),R_VALUES=(/TIME/),KIND="Time",UNIT="fs")
   B_Hall=B_Hall*HA2EV/(4*pi*cI*HBAR_eVfs)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   do i_dir=1,3
     do j_dir=1,3
       if (.not.TRabs_lEPS(i_dir,j_dir).or.i_dir==j_dir) cycle
       B_data(1)=real(B_Hall(i_dir,j_dir))
       call OUTPUT_driver("Hall_"//how,TITLES=(/"B_"//TRabs_EPS_label(i_dir,j_dir)/),&
&                             R_VALUES=(/real(B_Hall(i_dir,j_dir))/),UNIT="(Ohm*cm)^-1")
     enddo
   enddo
   call OUTPUT_driver("Hall_"//how,action="write")
 endif
 !
 call set_real_printed_length( )
 !
 contains
   !
   subroutine elemental_TIME_E_write(file,data)
     character(*) :: file
     real(SP)     :: data(2)
     call OUTPUT_driver(file,TITLES=(/"Time"/),R_VALUES=(/TIME/),UNIT="fs",KIND="Time")
     call OUTPUT_driver(file,TITLES=(/"Energy"/),R_VALUES=(/real(W%p(i_freq))/),UNIT="eV")
     call OUTPUT_driver(file,TITLES=(/"Eps-Im","Eps-Re"/),R_VALUES=data)
     call OUTPUT_driver(file,action="write")
   end subroutine
   !
   subroutine gimme_a_name(i,j,desc)
     integer :: i,j
     character(*) :: desc
     character(10) :: base
     N_files=N_files+1
     base="delta_eps"
     if (STRING_match(TRabsMODE,"eq")) base="eps"
     ofile(N_files)="YPP-TR_"//trim(base)//"_"//TRabs_EPS_label(i,j)//"_"//desc
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
