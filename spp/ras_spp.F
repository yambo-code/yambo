!
! Copyright (C) 2000-2005 C. Hogan and the SELF team
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
 subroutine ras_spp
 !--------------------------
 ! RAS in 3 layer model:
 ! \frac{\delta R}{R} = \frac{4\omega\d_surf}{c} \times
 !                      \Im \frac{ \epsilon^s_x - \epsilon^s_y}{\epsilon_b - 1}
 !
 ! The polarizability is:
 ! \epsilon = \frac{4 \pi}{\Omega} \alpha + 1
 ! and for a surface, \Omega = d_z (the cell height)
 ! Hence if polarizability is read, the data should be scaled by
 !                \frac{4 \pi}{d_z} * d_surf, and add  + 1
 !
 ! Input can be epsilon over cell, or alpha over cell.
 ! User needs to distinguish the two cases, and give d_cell.
 ! In case epsilon: d_surf = d_cell is passed to RAS_spectrum, with
 !   no scaling
 ! In case alpha  : d_surf = d_cell, and alpha is scaled by the
 !   above factor (the + 1 cancels in all difference spectra)
 !
 ! Input of d_cell is not strictly necessary for the RAS from alpha, but will 
 ! give incorrect eps_x-eps_y in the output file.
 !
 use pars,                ONLY:DP, lchlen,schlen,HARTREE, PI
 use com,                 ONLY:msg, error
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,&
&                              OP_RD_CL,VERIFY
 use timing,              ONLY:live_timing
 use SPP
 use bulkeps,             only : lbulkerr, GetBulkEps
 use ras_module,          only : CalcRAS
 implicit none
 !
 !Work Space
 !
 integer, parameter         :: maxlines = 10001
 complex(SP)                :: f(maxlines)
 real(SP)                   :: freq(maxlines) , sscale
 integer            ::iq , i,nw, nwy
 logical                    :: lfail = .false., do_ras = .true., epsilon_data
 character(lchlen)          :: errmsg
 complex(SP), allocatable   :: eps_x(:),eps_y(:),eps_b(:)
 real(SP),    allocatable   :: hw(:)

 if( trim(surftype).eq."e" ) epsilon_data = .true.
 if( trim(surftype).eq."a" ) epsilon_data = .false.
 if(epsilon_data) then
   sscale = 1.0_SP
 else ! alpha
   sscale = 4.0_SP*PI/d_cellin
 endif

 call read_spectral_data(xdata,xdatacol,nw,lfail)
 allocate(eps_x(nw),hw(nw))
 eps_x(1:nw) = f(1:nw)
 forall(i=1:nw) eps_x(i) = eps_x(i)*sscale
 hw(1:nw) = freq(1:nw)/HARTREE + sshift ! shift is internally converted

 if(ydata.eq.'none') do_ras = .false.
 if(do_ras) then
   call read_spectral_data(ydata,ydatacol,nwy,lfail)
   if(nwy.ne.nw) call error('X/Y datafiles have inconsistent data')
   allocate(eps_y(nw), eps_b(nw))
   eps_y(1:nw) = f(1:nw)
   forall(i=1:nw) eps_y(i) = eps_y(i)*sscale

   lbulkerr = .false.
   call GetBulkEps(hw, nw, eps_b, errmsg)
   call msg('nrs',trim(errmsg))

   if(.not.lbulkerr) call CalcRAS(hw, eps_x, eps_y, eps_b, nw, d_cellin)

 endif


! Add Gaussian broadening

 contains
  
   subroutine read_spectral_data(filename,col,nw,lfail)
     use com,              only : file_exists
     implicit none
     character*(*), intent(in) :: filename
     logical, intent(inout)    :: lfail
     integer, intent(out)      :: nw
     character(2), intent(in)  :: col
     character(lchlen)         :: cline
     integer, parameter        :: un = 60
     real(SP)                  :: rimag,rreal,rdum
     
     if(.not.(file_exists(trim(filename))) ) then
       errmsg='Unable to find file '//trim(filename)
       lfail = .true.
       return
     endif

     open(unit=un,file=trim(filename),err=998)
     nw = 0
     do while(.true.)
       read(un,*,end=99) cline
       if(index(cline,"#") > 0) cycle
       backspace(un)
       nw = nw + 1
       if(nw.gt.maxlines) goto 1000
       if(epsilon_data) then
         if(col.eq.'ir') read(un,*,err=999) freq(nw),rimag,rreal
         if(col.eq.'ri') read(un,*,err=999) freq(nw),rreal,rimag
       else
         if(col.eq.'ir') read(un,*,err=999) freq(nw),rdum,rdum,rimag,rreal
         if(col.eq.'ri') read(un,*,err=999) freq(nw),rdum,rdum,rreal,rimag
       endif
       f(nw) = cmplx(rreal,rimag)
     enddo
99   continue
     return

998  continue
     close(un)
     errmsg='Problem opening for reading the file '//trim(filename)
     lfail = .true.
     return

999  continue
     errmsg='Strange problem reading the file '//trim(filename)
     lfail = .true.
     close(un)
     return

1000 continue
     errmsg='File too large! '//trim(filename)
     lfail = .true.
     close(un)
     return
     
   end subroutine read_spectral_data
   
 end subroutine ras_spp
