!
! Copyright (C) 2000-2005 D. Varsano, A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
subroutine exc_wf_gnuplot(use_1d,use_2d,plot_dim,eval_only)
 !
 use pars,        ONLY:SP,schlen,BOHR
 use com,         ONLY:of_open_close,msg
 use FFT_m,       ONLY:fft_dim
 use D_lattice,   ONLY:a
 use stderr,      ONLY:intc
 use SPP,         ONLY:nr,nr_tot,exc_wf,p_dir,exc_wf,r_hole,lambda,wf_2d
 use BS,          ONLY:BSS_description,BSS_n_descs
 use timing,      ONLY:live_timing
 !
 implicit none
 integer    :: plot_dim
 logical    :: use_1d(3),use_2d(3),eval_only
 ! 
 ! Work Space...
 !
 integer           :: i1,i2,i3,ir,nr2plot(2),dir2plot(2)
 character(schlen) :: titles(4)
 real(SP)          :: wf_max
 !
 if (eval_only) then
   call live_timing('2D WF',nr(1))
 else
   call live_timing('GnuPlot',nr(1))
   !
   titles(:3)=(/'||a1 [A]','||a2 [A]','||a3 [A]'/)
   titles(4) = '|wf|^2 [1 at max]'
   !
   ! Open the output file
   !
   call of_open_close('exc_wf_gnu_'//trim(intc(lambda))//'_'//trim(p_dir),'ot')
   do i1=1,BSS_n_descs
     call msg('o exc',"#",trim(BSS_description(i1)),INDENT=0)
   enddo
   call msg('o exc',"#")
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
       if (use_1d(i1)) call msg('o exc',"#",(/titles(1),titles(4)/),INDENT=0,USE_TABS=.true.)
     enddo
     call msg('o exc','#')
     allocate(wf_2d(nr2plot(1),1))
     wf_2d=0.
     ir=0
     do i1 = 0,nr(1)-1
       do i2 = 0,nr(2)-1
         do i3 = 0,nr(3)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           if (use_1d(1)) wf_2d(i1+1,1)=wf_2d(i1+1,1)+abs(exc_wf(ir))**2
           if (use_1d(2)) wf_2d(i2+1,1)=wf_2d(i2+1,1)+abs(exc_wf(ir))**2
           if (use_1d(3)) wf_2d(i3+1,1)=wf_2d(i3+1,1)+abs(exc_wf(ir))**2
         enddo
       enddo
       call live_timing(steps=1)
     enddo
     wf_max=maxval(wf_2d(:,1))
     ! 
     ! Daniele [15/7/2007]
     ! Questo e' corretto, e' la normalizzazione. Io in una versione precedente
     ! di questa subroutine, la parte 1d di gnuplot non la avevo normalizzata
     ! a proposito. Siccome e' il plot piu' veloce, lo usavo per vedere il
     ! valore assoluto. Quando si mette la hole, magari un po' casaccio, cosi'
     ! vedi se stai pescando valori della wf, oppure no. Appunto nel caso che
     ! uno mette la hole in un punto con poca densita'.
     !
     wf_2d=wf_2d/wf_max 
     !
     do i1=0,nr2plot(1)-1
       call msg('o exc','',(/exc_wf_projection(i1,dir2plot(1)),&
&                            wf_2d(i1+1,1)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     deallocate(wf_2d)
     !
   case(2)
     !
     if (use_2d(1)) nr2plot=(/nr(1),nr(2)/)
     if (use_2d(2)) nr2plot=(/nr(1),nr(3)/)
     if (use_2d(3)) nr2plot=(/nr(2),nr(3)/)
     if (use_2d(1)) dir2plot=(/1,2/)
     if (use_2d(2)) dir2plot=(/1,3/)
     if (use_2d(3)) dir2plot=(/2,3/)
     allocate(wf_2d(nr2plot(1),nr2plot(2)))
     wf_2d=0.
     ir=0
     do i1 = 0,nr(1)-1
       do i2 = 0,nr(2)-1
         do i3 = 0,nr(3)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           if (use_2d(1)) wf_2d(i1+1,i2+1)=wf_2d(i1+1,i2+1)+abs(exc_wf(ir))**2
           if (use_2d(2)) wf_2d(i1+1,i3+1)=wf_2d(i1+1,i3+1)+abs(exc_wf(ir))**2
           if (use_2d(3)) wf_2d(i2+1,i3+1)=wf_2d(i2+1,i3+1)+abs(exc_wf(ir))**2
         enddo
       enddo
       call live_timing(steps=1)
     enddo
     wf_max=maxval(wf_2d(:,:))
     wf_2d=wf_2d/wf_max
     if (.not.eval_only) then
       if (use_2d(1)) call msg('o exc',"#",&
&                             (/titles(1),titles(2),titles(4)/),INDENT=0,USE_TABS=.true.)
       if (use_2d(2)) call msg('o exc',"#",&
&                             (/titles(1),titles(3),titles(4)/),INDENT=0,USE_TABS=.true.)
       if (use_2d(3)) call msg('o exc',"#",&
&                             (/titles(2),titles(3),titles(4)/),INDENT=0,USE_TABS=.true.)
       do i1=0,nr2plot(1)-1
         do i2=0,nr2plot(2)-1
           call msg('o exc','',(/exc_wf_projection(i1,dir2plot(1)),&
&                   exc_wf_projection(i2,dir2plot(2)),wf_2d(i1+1,i2+1)/),&
&                   INDENT=-2,USE_TABS=.true.)
         enddo
       enddo
     endif
     !
 end select
 !
 if (.not.eval_only) &
&   call of_open_close('exc_wf_gnu_'//trim(intc(lambda))//'_'//trim(p_dir))
 !
 contains
   !
   real(SP) function exc_wf_projection(I,dir)
     use D_lattice,   ONLY:a  
     use FFT_m,       ONLY:fft_dim
     use vec_operate, ONLY:v_norm
     integer :: I,dir
     real(SP):: rv(3)
     !
     rv(:)=I*a(dir,:)/fft_dim(dir)
     exc_wf_projection=(v_norm(rv)-r_hole(dir))*BOHR
     !
   end function
   !
end subroutine
