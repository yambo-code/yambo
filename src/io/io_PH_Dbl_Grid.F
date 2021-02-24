!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
integer function io_PH_Dbl_Grid(Freqs,q,ID)
 !
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 !
#include<memory.h>
 !
 type(levels) :: Freqs
 type(bz_samp):: q
 integer      :: ID 
 !
 ! Work Space
 !
 call timing('io_PH_Double_Grid',OPR='start')
 !
 io_PH_Dbl_Grid=-1
 !
 io_PH_Dbl_Grid=io_connect(desc='PH_Double_Grid',type=2,ID=ID)
 !
 if (io_PH_Dbl_Grid/=0) goto 1
 !
 io_PH_Dbl_Grid=io_header(ID,R_LATT=l_R_LATT,IMPOSE=l_R_LATT,NOWARN_SER=.true.)
 !
end function
