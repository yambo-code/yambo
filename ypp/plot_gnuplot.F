!
! Copyright (C) 2000-2009 D. Varsano, A. Marini and the YAMBO team
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
subroutine plot_gnuplot(use_1d,use_2d,eval_only)
 !
 use pars,        ONLY:SP,schlen
 use units,       ONLY:BOHR
 use com,         ONLY:msg
 use FFT_m,       ONLY:fft_dim
 use D_lattice,   ONLY:a
 use YPP,         ONLY:nr,v2plot,r_hole,v2plot2D,l_norm_to_one,WF_multiplier,plot_dim,&
&                      plot_title
 use timing,      ONLY:live_timing
 !
 implicit none
 logical    :: use_1d(3),use_2d(3),eval_only
 ! 
 ! Work Space...
 !
 integer           :: i1,i2,i3,ir,nr2plot(2),dir2plot(2)
 character(schlen) :: titles(4)
 real(SP)          :: v_max
 !
 if (len_trim(plot_title)==0) then
   call live_timing('3D Merge',nr(1),SERIAL=.true.)
 else
   call live_timing('3D Merge of '//trim(plot_title),nr(1),SERIAL=.true.)
 endif
 !
 if (.not.eval_only) then
   !
   titles(:3)=(/'||a1 [A]','||a2 [A]','||a3 [A]'/)
   titles(4) = '|wf|^2 [1 at max]'
   !
 endif
 !
 ! DIMENSIONs
 !
 select case(plot_dim)
   !
   case(1)
     !
     do i1=1,3 
       if (use_1d(i1)) nr2plot=nr(i1)
       if (use_1d(i1)) dir2plot=i1
       if (use_1d(i1)) call msg('o exc den mag wf',"#",(/titles(1),titles(4)/),INDENT=0,USE_TABS=.true.)
     enddo
     call msg('o exc den mag wf','#')
     allocate(v2plot2D(nr2plot(1),1))
     v2plot2D=0.
     ir=0
     do i1 = 0,nr(1)-1
       do i2 = 0,nr(2)-1
         do i3 = 0,nr(3)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           if (use_1d(1)) v2plot2D(i1+1,1)=v2plot2D(i1+1,1)+v2plot(ir)
           if (use_1d(2)) v2plot2D(i2+1,1)=v2plot2D(i2+1,1)+v2plot(ir)
           if (use_1d(3)) v2plot2D(i3+1,1)=v2plot2D(i3+1,1)+v2plot(ir)
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
       call msg('o exc den mag wf','',(/projection_(i1,dir2plot(1)),&
&                            v2plot2D(i1+1,1)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     deallocate(v2plot2D)
     !
   case(2)
     !
     if (use_2d(1)) nr2plot=(/nr(1),nr(2)/)
     if (use_2d(2)) nr2plot=(/nr(1),nr(3)/)
     if (use_2d(3)) nr2plot=(/nr(2),nr(3)/)
     if (use_2d(1)) dir2plot=(/1,2/)
     if (use_2d(2)) dir2plot=(/1,3/)
     if (use_2d(3)) dir2plot=(/2,3/)
     allocate(v2plot2D(nr2plot(1),nr2plot(2)))
     v2plot2D=0.
     ir=0
     do i1 = 0,nr(1)-1
       do i2 = 0,nr(2)-1
         do i3 = 0,nr(3)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           if (use_2d(1)) v2plot2D(i1+1,i2+1)=v2plot2D(i1+1,i2+1)+v2plot(ir)
           if (use_2d(2)) v2plot2D(i1+1,i3+1)=v2plot2D(i1+1,i3+1)+v2plot(ir)
           if (use_2d(3)) v2plot2D(i2+1,i3+1)=v2plot2D(i2+1,i3+1)+v2plot(ir)
         enddo
       enddo
       call live_timing(steps=1)
     enddo
     !
     v_max=maxval(v2plot2D(:,:))
     if (l_norm_to_one) v2plot2D=v2plot2D/v_max  
     v2plot2D=v2plot2D*WF_multiplier
     !
     if (.not.eval_only) then
       if (use_2d(1)) call msg('o exc den mag wf',"#",&
&                             (/titles(1),titles(2),titles(4)/),INDENT=0,USE_TABS=.true.)
       if (use_2d(2)) call msg('o exc den mag wf',"#",&
&                             (/titles(1),titles(3),titles(4)/),INDENT=0,USE_TABS=.true.)
       if (use_2d(3)) call msg('o exc den mag wf',"#",&
&                             (/titles(2),titles(3),titles(4)/),INDENT=0,USE_TABS=.true.)
       call msg('o exc den mag wf',"#")
       do i1=0,nr2plot(1)-1
         do i2=0,nr2plot(2)-1
           call msg('o exc den mag wf','',(/projection_(i1,dir2plot(1)),&
&                   projection_(i2,dir2plot(2)),v2plot2D(i1+1,i2+1)/),&
&                   INDENT=-2,USE_TABS=.true.)
         enddo
       enddo
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
     rv(:)=I*a(dir,:)/fft_dim(dir)-r_hole(:)
     projection_=dot_product(rv,a(dir,:))/v_norm(a(dir,:))*BOHR
     !
   end function
   !
end subroutine
