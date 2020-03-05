!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine plot_gnuplot(eval_only)
 !
 use pars,        ONLY:SP,schlen
 use units,       ONLY:BO2ANG
 use com,         ONLY:msg
 use YPPm,        ONLY:nr,v2plot,r_hole,v2plot2D,l_norm_to_one,WF_multiplier,plot_dim,&
&                      plot_title,output_string,l_wavefunction,l_current,use_xcrysden,&
&                      plot_is_1D,plot_is_2D,l_free_hole
 use LIVE_t,      ONLY:live_timing
 !
#include<memory.h>
 logical    :: eval_only
 ! 
 ! Work Space...
 !
 integer           :: i1,i2,i3,ir,nr2plot(2),dir2plot(2)
 character(schlen) :: titles(4)
 real(SP)          :: v_max,r_plot_center(3)
 !
 if (plot_dim==3) return
 !
 if (len_trim(plot_title)==0) then
   call live_timing('3D Merge',nr(1))
 else
   call live_timing('3D Merge of '//trim(plot_title),nr(1))
 endif
 !
 if (.not.eval_only) then
   !
   titles=" "
   !
   titles(:3)=(/'||a1 [A]','||a2 [A]','||a3 [A]'/)
   if (l_wavefunction) titles(4) = '|wf|^2 [1 at max]'
   if (l_current)      titles(4) = 'J [nAmpere]'
   !
 endif
 !
 ! SET CENTER OF THE PLOT
 !
 r_plot_center=0.
 if (.not.l_free_hole) r_plot_center=r_hole
 !
 ! DIMENSIONs
 !
 select case(plot_dim)
   !
   case(1)
     !
     do i1=1,3 
       if (plot_is_1D(i1)) nr2plot=nr(i1)
       if (plot_is_1D(i1)) dir2plot=i1
       if (plot_is_1D(i1)) call msg(output_string,"#",(/titles(1),titles(4)/),INDENT=0,USE_TABS=.true.)
     enddo
     call msg(output_string,'#')
     YAMBO_ALLOC(v2plot2D,(nr2plot(1),1))
     v2plot2D=0._SP
     ir=0
     do i1 = 0,nr(1)-1
       do i2 = 0,nr(2)-1
         do i3 = 0,nr(3)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           if (plot_is_1D(1)) v2plot2D(i1+1,1)=v2plot2D(i1+1,1)+v2plot(ir)
           if (plot_is_1D(2)) v2plot2D(i2+1,1)=v2plot2D(i2+1,1)+v2plot(ir)
           if (plot_is_1D(3)) v2plot2D(i3+1,1)=v2plot2D(i3+1,1)+v2plot(ir)
         enddo
       enddo
       call live_timing(steps=1)
     enddo
     v_max=maxval(v2plot2D(:,1))
     ! 
     ! Daniele [15/7/2007]
     ! Questo e' corretto, e' la normalizzazione. Io in una versione precedente
     ! di questa subroutine, la parte 1d di gnuplot non la avevo normalizzata
     ! a proposito. Siccome e' il plot piu' veloce, lo usavo per vedere il
     ! valore assoluto. Quando si mette la hole, magari un po' casaccio, cosi'
     ! vedi se stai pescando valori della wf, oppure no. Appunto nel caso che
     ! uno mette la hole in un punto con poca densita'.
     !
     if (l_norm_to_one) v2plot2D=v2plot2D/v_max  
     v2plot2D=v2plot2D*WF_multiplier
     !
     do i1=0,nr2plot(1)-1
       call msg(output_string,'',(/projection_(i1,dir2plot(1)),&
&                            v2plot2D(i1+1,1)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     YAMBO_FREE(v2plot2D)
     !
   case(2)
     !
     if (.not.use_xcrysden) then
       if (plot_is_2D(1)) nr2plot=(/nr(1),nr(2)/)
       if (plot_is_2D(2)) nr2plot=(/nr(1),nr(3)/)
       if (plot_is_2D(3)) nr2plot=(/nr(2),nr(3)/)
     else
       if (plot_is_2D(1)) nr2plot=(/nr(1)+1,nr(2)+1/)
       if (plot_is_2D(2)) nr2plot=(/nr(1)+1,nr(3)+1/)
       if (plot_is_2D(3)) nr2plot=(/nr(2)+1,nr(3)+1/)
     endif
     if (plot_is_2D(1)) dir2plot=(/1,2/)
     if (plot_is_2D(2)) dir2plot=(/1,3/)
     if (plot_is_2D(3)) dir2plot=(/2,3/)
     YAMBO_ALLOC(v2plot2D,(nr2plot(1),nr2plot(2)))
     v2plot2D=0.
     ir=0
     do i1 = 0,nr(1)-1
       do i2 = 0,nr(2)-1
         do i3 = 0,nr(3)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           if (plot_is_2D(1)) v2plot2D(i1+1,i2+1)=v2plot2D(i1+1,i2+1)+v2plot(ir)
           if (plot_is_2D(2)) v2plot2D(i1+1,i3+1)=v2plot2D(i1+1,i3+1)+v2plot(ir)
           if (plot_is_2D(3)) v2plot2D(i2+1,i3+1)=v2plot2D(i2+1,i3+1)+v2plot(ir)
         enddo
       enddo
       call live_timing(steps=1)
     enddo
     if(use_xcrysden) then
       v2plot2D(nr2plot(1),:nr2plot(2)-1)=v2plot2D(1,:nr2plot(2)-1)
       v2plot2D(:nr2plot(1)-1,nr2plot(2))=v2plot2D(:nr2plot(1)-1,1)
       v2plot2D(nr2plot(1),nr2plot(2))=v2plot2D(1,1)
     endif
     !
     v_max=maxval(v2plot2D(:,:))
     if (l_norm_to_one) v2plot2D=v2plot2D/v_max  
     v2plot2D=v2plot2D*WF_multiplier
     !
     if (.not.eval_only) then
       if (plot_is_2D(1)) call msg(output_string,"#",&
&                             (/titles(1),titles(2),titles(4)/),INDENT=0,USE_TABS=.true.)
       if (plot_is_2D(2)) call msg(output_string,"#",&
&                             (/titles(1),titles(3),titles(4)/),INDENT=0,USE_TABS=.true.)
       if (plot_is_2D(3)) call msg(output_string,"#",&
&                             (/titles(2),titles(3),titles(4)/),INDENT=0,USE_TABS=.true.)
       call msg(output_string,"#")
       do i1=0,nr2plot(1)-1
         do i2=0,nr2plot(2)-1
           call msg(output_string,'',(/projection_(i1,dir2plot(1)),&
&                   projection_(i2,dir2plot(2)),v2plot2D(i1+1,i2+1)/),&
&                   INDENT=-2,USE_TABS=.true.)
         enddo
       enddo
     endif
     !
     if (.not.use_xcrysden) then
       !
       YAMBO_FREE(v2plot2D)
       !
     endif
     !
 end select
 !
 call live_timing()
 !
 contains
   !
   real(SP) function projection_(I,dir)
     use D_lattice,   ONLY:a  
     use FFT_m,       ONLY:fft_dim
     use vec_operate, ONLY:v_norm
     integer :: I,dir
     real(SP):: rv(3)
     !
     rv(:)=I*a(dir,:)/fft_dim(dir)-r_plot_center(:) 
     projection_=dot_product(rv,a(dir,:))/v_norm(a(dir,:))*BO2ANG
     !
   end function
   !
end subroutine
