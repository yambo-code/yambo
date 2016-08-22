!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
 subroutine ras_ypp
 !--------------------------
 ! RAS in 3 layer model:
 ! \frac{\delta R}{R} = \frac{4\omega\d_surf}{c} \times
 !                      \Im \frac{ \epsilon^s_x - \epsilon^s_y}{\epsilon_b - 1}
 !
 ! Nota bene: the following was based on definitions in previous versions of yambo,
 !            and may not be correct!
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
 use pars,                ONLY:DP, lchlen,schlen, PI
 use units,               ONLY:HA2EV
 use com,                 ONLY:msg, error
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,&
&                              OP_RD_CL,VERIFY
 use LIVE_t,              ONLY:live_timing
 use YPP
 use bulkeps,             ONLY : lbulkerr, GetBulkEps
 use ras_module,          ONLY : CalcRAS
 implicit none
 !
 !Work Space
 !
 integer, parameter         :: maxlines = 10001
 complex(SP)                :: f(maxlines)
 real(SP)                   :: freq(maxlines) , sscale
 integer            ::iq , i,nw, nwy, pol_type
 logical                    :: lfail = .false., do_ras = .true.
 character(lchlen)          :: errmsg
 complex(SP), allocatable   :: eps_x(:),eps_y(:),eps_b(:)
 real(SP),    allocatable   :: hw(:)

 call section('*',"== RAS postprocessor ==")
 call msg('nrs','NB: Calculate d factor with care!')
 if( index(datatype,"eps").eq.1 ) pol_type = 0
 if( index(datatype,"a_r").eq.1 ) pol_type = 1
 if( index(datatype,"a_t").eq.1 ) pol_type = 2
 call msg('nrs','d parameter (au): ',d_cellin)

 call read_spectral_data(xdata,datatype,nw,lfail)
 allocate(eps_x(nw),hw(nw))
 eps_x(1:nw) = f(1:nw)
 hw(1:nw) = freq(1:nw)/HA2EV + sshift ! shift is internally converted
 sscale = 4.0_SP*PI/d_cellin

 select case(pol_type)
 case(0)
   call msg('rs','Processing epsilon data: no scaling.')
 case(1) ! Resonant, anti resonant, default BSE
   call msg('rs','Processing alpha data for resonant case: scaling by 4*pi/d_cell')
   forall(i=1:nw) eps_x(i) = eps_x(i)*sscale
 case(2) ! Retarded, time-ordered, default RPA, RAS
   sscale = 4.0_SP*PI/d_cellin
   forall(i=1:nw) eps_x(i) = 1.0_SP/( 1.0_SP - eps_x(i)*sscale )
   call msg('rs','Processing alpha data for causal: scaling by 4*pi/d_cell and inverting')
 end select
 if( index(datatype,"23").eq.4 ) call msg('rs','Imag/Real data extracted from columns 2,3')
 if( index(datatype,"45").eq.4 ) call msg('rs','Imag/Real data extracted from columns 4,5')


 if(ydata.eq.'none') do_ras = .false.
 if(do_ras) then
   call read_spectral_data(ydata,datatype,nwy,lfail)
   if(nwy.ne.nw) call error('X/Y datafiles have inconsistent data')
   allocate(eps_y(nw), eps_b(nw))
   eps_y(1:nw) = f(1:nw)

   select case(pol_type)
   case(1)
     forall(i=1:nw) eps_y(i) = eps_y(i)*sscale
   case(2) 
     forall(i=1:nw) eps_y(i) = 1.0_SP/( 1.0_SP - eps_y(i)*sscale )
   end select

   lbulkerr = .false.
   call GetBulkEps(hw, nw, eps_b, errmsg)
   call msg('nrs',trim(errmsg))

   if(.not.lbulkerr) call CalcRAS(hw, eps_x, eps_y, eps_b, nw, d_cellin,"-ypp")

 endif

 contains
  
   subroutine read_spectral_data(filename,col,nw,lfail)
     use com,              ONLY : file_exists
     implicit none
     character*(*), intent(in) :: filename
     logical, intent(inout)    :: lfail
     integer, intent(out)      :: nw
     character(5), intent(in)  :: col
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
       if( index(col,"23").eq.4 ) then
         read(un,*,err=999) freq(nw),rimag,rreal
       else if( index(col,"45").eq.4 ) then
         read(un,*,err=999) freq(nw),rdum,rdum,rimag,rreal
       else
         errmsg='Unable read data with datatype: '//trim(datatype)
         lfail = .true.
         return
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
   
 end subroutine ras_ypp
