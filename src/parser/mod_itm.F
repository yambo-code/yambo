!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module it_m
 !
 use pars,       ONLY:schlen,lchlen,SP
 implicit none
 !
 character(lchlen)  :: infile
 !
 ! Units Names
 !
 character(2),parameter :: MEM_unit(3)=(/'Kb','Mb','Gb'/)
 character(2),parameter :: E_unit='eV'
 character(2),parameter :: G_unit='RL'
 character(2),parameter :: T_unit='eV'
 character(2),parameter :: Time_unit(3)=(/'fs','ps','as'/)
 character(2),parameter :: Bfield_unit='T'
 character(4),parameter :: Efield_unit='V/mm'
 character(5),parameter :: I_unit='kWLm2'
 character(3),parameter :: Angle_unit(2)=(/'deg','rad'/)
 ! 
 ! Verbosity
 !===========
 !
 integer, parameter :: V_RL=1
 integer, parameter :: V_kpt=2
 integer, parameter :: V_sc=3
 integer, parameter :: V_qp=4
 integer, parameter :: V_io=5
 integer, parameter :: V_general=6
 integer, parameter :: V_resp=7
 integer, parameter :: V_real_time=8
 integer, parameter :: V_parallel=9
 integer, parameter :: V_nl_optics=10
 integer, parameter :: V_all=99
 !
 integer, parameter :: maxrnlvls=50
 integer, parameter :: maxflags =50
 integer, parameter :: maxi1vars=100
 integer, parameter :: maxi2vars=50
 integer, parameter :: maxi3vars=50
 integer, parameter :: maxr1vars=150
 integer, parameter :: maxr2vars=50
 integer, parameter :: maxr3vars=100
 integer, parameter :: maxr4vars=50
 integer, parameter :: maxc1vars=50
 integer, parameter :: maxchvars=100
 integer, parameter :: nvars=(maxi1vars+2*maxi2vars+2*maxi3vars+&
&                             maxr1vars+2*maxr2vars+2*maxr3vars+4*maxr4vars+&
&                             maxc1vars+maxchvars)
 integer :: nrnlvls,nflags,ni1v,ni2v,ni3v,nr1v,nr2v,nr3v,&
&           nr4v,nc1v,nchv,ns,infile_verbosity,initmode
 character(schlen) :: rnlvls(maxrnlvls,2)
 integer ::      rstatus(maxrnlvls) 
 character(schlen) :: flags(maxflags,2)
 integer ::      fstatus(maxflags)
 character(schlen) :: i1vars(maxi1vars,3)
 integer ::           i1st(maxi1vars)
 character(schlen) :: i2vars(maxi2vars,3)
 integer ::           i2st(maxi2vars)
 character(schlen) :: i3vars(maxi3vars,3)
 integer ::           i3st(maxi3vars)
 character(schlen) :: r1vars(maxr1vars,3)
 integer ::           r1st(maxr1vars)
 character(schlen) :: r2vars(maxr2vars,3)
 integer ::           r2st(maxr2vars)
 character(schlen) :: r3vars(maxr3vars,3)
 integer ::           r3st(maxr3vars)
 character(schlen) :: r4vars(maxr4vars,3)
 integer ::           r4st(maxr4vars)
 character(schlen) :: c1vars(maxc1vars,3)
 integer ::           c1st(maxc1vars)
 character(schlen) :: chvars(maxchvars,2)
 integer ::           chst(maxchvars)
 integer :: nifpos
 !
 ! Input file
 !============
 integer :: nflines
 integer :: nADDITIONAL_flines=0
 character(lchlen) :: flines(nvars+maxflags+maxrnlvls)
 character(lchlen) :: ADDITIONAL_flines(100)
 !
 ! To handle the variables verbosity I need to create a copy
 ! of the standard variable defs and an array with the verbosity 
 ! level
 !
 integer           :: n_verbose_Ivars,&
&                     it_verbose_Ilevel(maxi1vars+maxi2vars+maxi3vars),&
&                     it_verbose_Itable(3,maxi1vars)
 integer           :: it_verbose_Idefs(maxi1vars+maxi2vars+maxi3vars,3) 
 character(schlen) :: it_verbose_Ivars(maxi1vars+maxi2vars+maxi3vars)
 integer           :: n_verbose_Rvars,&
&                     it_verbose_Rlevel(maxr1vars+maxr2vars+maxr3vars+maxr4vars),&
&                     it_verbose_Rtable(4,maxr1vars)
 real(SP)          :: it_verbose_Rdefs(maxr1vars+maxr2vars+maxr3vars+maxr4vars,4) 
 character(schlen) :: it_verbose_Rvars(maxr1vars+maxr2vars+maxr3vars+maxr4vars)
 integer           :: n_verbose_Cvars,it_verbose_Clevel(maxc1vars),&
&                     it_verbose_Ctable(maxc1vars)
 complex(SP)       :: it_verbose_Cdefs(maxc1vars) 
 character(schlen) :: it_verbose_Cvars(maxc1vars)
 integer           :: n_verbose_Chvars,it_verbose_Chlevel(maxchvars),&
&                     it_verbose_Chtable(maxchvars)
 character(schlen) :: it_verbose_Chvars(maxchvars)
 character(lchlen) :: it_verbose_Chdefs(maxchvars)
 integer           :: n_verbose_Fvars,it_verbose_Flevel(maxflags),&
&                     it_verbose_Ftable(maxflags)
 character(schlen) :: it_verbose_Fvars(maxflags)
 !
 type initdefs
   integer     :: i1def(maxi1vars)
   integer     :: i2def(maxi2vars,2)
   integer     :: i3def(maxi3vars,3)
   real(SP)    :: r1def(maxr1vars)
   real(SP)    :: r2def(maxr2vars,2)
   real(SP)    :: r3def(maxr3vars,3)
   real(SP)    :: r4def(maxr4vars,4)
   complex(SP) :: c1def(maxc1vars)
   character(lchlen) :: chdef(maxchvars)
 end type initdefs
 !
end module it_m
