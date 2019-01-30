!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine fft_check_size(iGo_max,NG_xc,NG_H)
 !
 use fft_m,             ONLY:fft_size
 use parser_m,          ONLY:parser
 use R_lattice,         ONLY:ng_in_shell,n_g_shells
 use com,               ONLY:warning
 use stderr,            ONLY:intc
 !
 implicit none
 !
 integer, intent(in)    :: NG_xc,iGo_max
 integer, intent(inout) :: NG_H
 !
 ! Workspace
 !
 logical :: l_USE_all_Gexx,l_USE_all_GHAR
 integer :: H_fft_size,xc_fft_size,i_shell,NG_H_save,lower_shell,i_exp,n
 !
 !
 if ( (NG_xc>=NG_H) .or. NG_xc==0 .or. NG_H==0  ) return
 !
 call parser('ALLGexx',l_USE_all_Gexx)
 call parser('ALLGHAR',l_USE_all_GHAR)
 !
 call fft_setup(NG_xc,iGo_max,.true.)
 xc_fft_size=fft_size
 !
 call fft_setup(NG_H,iGo_max,.true.)
 H_fft_size=fft_size
 if (fft_size>xc_fft_size) then
   !
   NG_H_save=NG_H
   !
   lower_shell=2
   i_exp=5
   n=10000
   do while(n>1)
     n=10**i_exp
     i_exp=i_exp-1
     if(n>n_g_shells/10.and.n>1) cycle
     do i_shell=lower_shell,n_g_shells,n
       if (ng_in_shell(i_shell)<=NG_xc) cycle
       call fft_setup(ng_in_shell(i_shell),iGo_max,.true.)
       lower_shell=i_shell-n
       if (fft_size>xc_fft_size) exit
     enddo
   enddo
   !
   if(l_USE_all_Gexx .or. l_USE_all_GHAR) then
     call warning(' S/K NG_H= '//trim(intc(NG_H))//' imposes FFT size= '//trim(intc(H_fft_size)))
     call warning(' S/K NG_xc= '//trim(intc(NG_xc))//' needs FFT size= '//trim(intc(xc_fft_size))//&
&                 ', suggested NG_H= '//trim(intc(ng_in_shell(i_shell-1))))
     call warning(' Bigger FFT kept due to ALLG input. Slow XC S/K computation.')
   else
     NG_H=ng_in_shell(i_shell-1)
     call warning(' Hartree S/K FFT size is too big. NG_H reduced: '//trim(intc(NG_H_save))//' --> '//trim(intc(NG_H)))
     call warning(' Bigger FFT discarded to avoid slow XC S/K computation.')
   endif
   !
 endif
 !
end subroutine
