!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine fft_check_size(iGo_max,NG_C,NG_HF,mode)
 !
 use fft_m,             ONLY:fft_size
 use parser_m,          ONLY:parser
 use R_lattice,         ONLY:ng_in_shell,n_g_shells
 use stderr,            ONLY:intc
 !
 implicit none
 !
 integer,      intent(in)    :: NG_C,iGo_max
 integer,      intent(inout) :: NG_HF
 character(2), intent(in)    :: mode
 !
 ! Workspace
 !
 logical :: l_USE_all_Gexx,l_USE_all_GHAR,l_USE_all_HF
 integer :: HF_fft_size,C_fft_size,i_shell,NG_HF_save,lower_shell,i_exp,n
 character(4) :: NGname
 character(6) :: SKname
 character(8) :: Kindname
 !
 if ( (NG_C>=NG_HF) .or. NG_C==0 .or. NG_HF==0  ) return
 !
 call parser('ALLGexx',l_USE_all_Gexx)
 call parser('ALLGHAR',l_USE_all_GHAR)
 !
 if ( mode=="SH" ) then
   l_USE_all_HF=l_USE_all_GHAR
   NGname="NG_H"
   SKname="Sigma"
   Kindname="Hartree"
 endif
 !
 if ( mode=="SF" ) then
   l_USE_all_HF=l_USE_all_Gexx
   NGname="NG_F"
   SKname="Sigma"
   Kindname="Fock"
 endif
 !
 if ( mode=="Kx" ) then
   ! DS: here I use Gexx because in BSE the hartree kernel is the eh-exchange
   l_USE_all_HF=l_USE_all_Gexx
   NGname="NG_X"
   SKname="Kernel"
   Kindname="Exchange"
 endif
 !
 call fft_setup(NG_C,iGo_max,.true.)
 C_fft_size=fft_size
 !
 call fft_setup(NG_HF,iGo_max,.true.)
 HF_fft_size=fft_size
 if (fft_size>C_fft_size) then
   !
   NG_HF_save=NG_HF
   !
   lower_shell=2
   i_exp=5
   n=10000
   do while(n>1)
     n=10**i_exp
     i_exp=i_exp-1
     if(n>n_g_shells/10.and.n>1) cycle
     do i_shell=lower_shell,n_g_shells,n
       if (ng_in_shell(i_shell)<=NG_C) cycle
       call fft_setup(ng_in_shell(i_shell),iGo_max,.true.)
       lower_shell=i_shell-n
       if (fft_size>C_fft_size) exit
     enddo
   enddo
   !
   if(l_USE_all_HF) then
     call warning(' '//trim(Kindname)//' '//trim(SKname)//' '//&
                 &   NGname//'= '//trim(intc(NG_HF))//' imposes FFT size= '//trim(intc(HF_fft_size)))
     call warning(' '//trim(SKname)//' NG_C= '//trim(intc(NG_C))//' needs FFT size= '//trim(intc(C_fft_size))//&
                 &  ', suggested '//NGname//'= '//trim(intc(ng_in_shell(i_shell-1))))
     call warning(' Bigger FFT kept due to ALLG input. Slow computation for corr part of '//trim(SKname))
   else
     NG_HF=ng_in_shell(i_shell-1)
     call warning(' '//trim(Kindname)//' '//trim(SKname)//' FFT size is too big. '// &
                 &   NGname//' reduced '//trim(intc(NG_HF_save))//' --> '//trim(intc(NG_HF)))
     call warning(' Bigger FFT discarded to avoid slow computation for corr part of '//trim(SKname))
   endif
   !
 endif
 !
end subroutine
