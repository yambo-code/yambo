!
!        Copyright (C) 2000-2019 the YAMBO team
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
 interface it
   module procedure loadrnlvlflag,loadi1var,loadinvar,loadr1var,loadrnvar,&
&                   loadc1var,loadchvar
 end interface it
 ! 
 ! Work Space
 !
 integer :: i1
 !
 contains
   !
   subroutine it_reset(mode)
     use stderr,    ONLY:cstr
     use PARSER_lib,ONLY:iparse_init,iparse_end
     integer, optional :: mode
     integer           :: ierr
     if (mode<=0) call iparse_end()
     if (mode>=0) ierr=iparse_init(cstr(trim(infile)),cstr('-'))
     if (mode==0) return
     nifpos=1
     infile_verbosity=0
     i1vars=' '
     i2vars=' '
     i3vars=' '
     r1vars=' '
     r2vars=' '
     r3vars=' '
     r4vars=' '
     c1vars=' '
     n_verbose_Ivars=0
     n_verbose_Rvars=0
     n_verbose_Cvars=0
     n_verbose_Chvars=0
     n_verbose_Fvars=0
     it_verbose_Ilevel=0
     it_verbose_Itable=0
     it_verbose_Rlevel=0
     it_verbose_Rtable=0
     it_verbose_Clevel=0
     it_verbose_Ctable=0
     it_verbose_Chlevel=0
     it_verbose_Chtable=0
     it_verbose_Flevel=0
     it_verbose_Ftable=0
     nrnlvls=0
     nflags=0
     nflines=0
     ni1v=0
     ni2v=0
     ni3v=0
     nr1v=0
     nr2v=0
     nr3v=0
     nr4v=0
     nc1v=0
     nchv=0
     ns=0
   end subroutine
   !
   logical function check_verbosity(what)
     use drivers, ONLY:infile_editing
     character(*) :: what
     if (what=="kpt") check_verbosity=infile_verbosity==V_kpt.or.infile_verbosity==V_all
     if (what=="RL" ) check_verbosity=infile_verbosity==V_RL.or.infile_verbosity==V_all
     if (what=="sc" ) check_verbosity=infile_verbosity==V_sc.or.infile_verbosity==V_all
     if (what=="qp" ) check_verbosity=infile_verbosity==V_qp.or.infile_verbosity==V_all
     if (what=="io" ) check_verbosity=infile_verbosity==V_io.or.infile_verbosity==V_all
     if (what=="general"  ) check_verbosity=infile_verbosity==V_general.or.infile_verbosity==V_all
     if (what=="resp"     ) check_verbosity=infile_verbosity==V_resp.or.infile_verbosity==V_all
     if (what=="real_time") check_verbosity=infile_verbosity==V_real_time.or.infile_verbosity==V_all
     if (what=="parallel" ) check_verbosity=infile_verbosity==V_parallel.or.infile_verbosity==V_all
     if (.not.infile_editing) check_verbosity=.TRUE.
   end function
   !
   subroutine infile_dump()
     character(lchlen) :: ch
     open(unit=12,file=trim(infile))
     do i1=1,10000
       read(12,'(a)',end=1) ch
       flines(i1)=ch
       if (index(ch,'#')>0) flines(i1)=ch(:index(ch,'#')-1)
       nflines=i1
     enddo
1    close(12)
   end subroutine
   !
   subroutine ofiles_append(line,defs)
     use com,           ONLY: nofs,of
     character(*),optional  :: line
     type(initdefs),optional::defs
     integer :: i2
     do i2=1,nofs
       if (len_trim(of(i2))==0) cycle
       open(20,file=trim(of(i2)),position='append')
       if (present(defs)) call initinfio(defs,20)
       if (present(line)) write (20,'(2a)') '# ',line
       close(20)
     enddo
   end subroutine
   !
   logical function runlevel_is_on(runlevel_name)
     character(*) :: runlevel_name
     integer      :: i2
     runlevel_is_on=.false.
     do i2=1,nrnlvls
       if (trim(rnlvls(i2,1))==runlevel_name) runlevel_is_on=rstatus(i2)/=0
     enddo
   end function
   !
   subroutine switch_off_runlevel(off_name,on_name)
     use stderr,    ONLY:STRING_split
     character(*)     :: off_name,on_name
     !
     ! Work Space
     !
     character(schlen):: on_names(maxrnlvls)
     integer          :: i2,i3
     call STRING_split(on_name,on_names)
     ext_loop: do i2=1,nrnlvls
       if (rstatus(i2)==0) cycle
       do i3=1,maxrnlvls
         if (len_trim(on_names(i3))==0) cycle
         if (trim(rnlvls(i2,1))==trim(on_names(i3))) cycle ext_loop
       enddo
       if (trim(rnlvls(i2,1))==off_name.or.off_name=='all') rstatus(i2)=0
     enddo ext_loop
   end subroutine
   ! 
   !@@@@@@@@@@@@@@
   ! INTERFACE IT
   !@@@@@@@@@@@@@@
   !
   ! Modes
   !
   ! initmode 0:'load'      : defs% <- Default from def+stdefs routines
   !                          initdefs is initialized
   ! initmode 1:'todef'     : Like 'load' but as DBs have been read
   !                          defs% are redefined
   ! initmode 2:'Gclose'    : like  'todef' + Close the G-vectors to the G-shells
   ! initmode 3:'GameOver'  : Like 'todef' but verbosity defaults are not updated.
   !                          To be used at the run end
   ! initmode 4:'parserload': defs% (and associated variables of the mods) 
   !                          are overwritten with the input file values
   !
   subroutine loadrnlvlflag(what,defs,name,description,verb_level)
     use drivers,      ONLY:infile_editing
     use parser_m,     ONLY:parser
     character(1)::what
     character(*)::name,description
     integer,optional :: verb_level
     type(initdefs)::defs
     ! 
     ! Work Space
     !
     logical :: ldef 
     !
     select case (what)
       case ('r')
         if (initmode==0) then
           rstatus=0
           nrnlvls=nrnlvls+1
           rnlvls(nrnlvls,1)=name
           rnlvls(nrnlvls,2)=description
         else if (initmode==4) then
           do i1=1,nrnlvls
             if (trim(rnlvls(i1,1))==trim(name)) then
               call parser(trim(name),ldef)
               if (ldef.and..not.infile_editing) rstatus(i1)=1
               exit
             endif
           enddo
         endif
       case ('f')
         if (initmode==0) then
           fstatus=0
           nflags=nflags+1
           flags(nflags,1)=name
           flags(nflags,2)=description
           if (present(verb_level)) call initverbosity(name,nflags,'f',defs,verb_level)
         else if (initmode==4) then
           do i1=1,nflags
             if (trim(flags(i1,1))==trim(name)) then
               call parser(trim(name),ldef)
               if (ldef) fstatus(i1)=-999
               exit
             endif
           enddo
         endif
     end select
   end subroutine
   !
   subroutine loadi1var(defs,name,description,defval,unit,verb_level)
     use parser_m,       ONLY:parser
     character(*)::name,description
     character(*), optional :: unit
     integer,optional :: verb_level
     integer :: defval
     type(initdefs)::defs
     integer :: v_id
     if (initmode==0) then
       i1st=0
       ni1v=ni1v+1
       if (ni1v>maxi1vars) stop 'MAXi1VARS'
       i1vars(ni1v,1)=name
       i1vars(ni1v,2)=description
       if (present(unit)) i1vars(ni1v,3)=unit
       defs%i1def(ni1v)=defval
       v_id=ni1v
     else
       do i1=1,ni1v
         if (trim(i1vars(i1,1))==trim(name)) then
           if (initmode==4) call parser(trim(name),defs%i1def(i1))
           if (initmode==2) call PARSER_convert_INT(defval,i1vars(i1,3),.TRUE.)
           if (initmode<=3) defs%i1def(i1)=defval
           defval=defs%i1def(i1)
           v_id=i1
         endif
       enddo
     endif
     !
     ! Verbosity variables defaults update
     !
     if (initmode<=1.and.present(verb_level)) call initverbosity(name,v_id,'i1',defs,verb_level)
     !
   end subroutine
   !
   subroutine loadinvar(defs,name,description,defval,unit,verb_level)
     use parser_m,       ONLY:parser
     character(*)::name,description
     character(*), optional :: unit
     integer,optional :: verb_level
     integer :: defval(:)
     type(initdefs)::defs
     integer :: i2,v_id,dim !ws
     dim=product(shape(defval))
     if (initmode==0) then
       select case(dim)
       case(2)
         i2st=0
         ni2v=ni2v+1
         if (ni2v>maxi2vars) stop 'MAXi2VARS'
         i2vars(ni2v,1)=name
         i2vars(ni2v,2)=description
         if (present(unit)) i2vars(ni2v,3)=unit
         defs%i2def(ni2v,:)=defval
         v_id=ni2v
       case(3)
         i3st=0
         ni3v=ni3v+1
         if (ni3v>maxi3vars) stop 'MAXi3VARS' 
         i3vars(ni3v,1)=name
         i3vars(ni3v,2)=description
         if (present(unit)) i3vars(ni3v,3)=unit
         defs%i3def(ni3v,:)=defval
         v_id=ni3v
       end select
     else
       select case(dim)
       case(2)
         do i1=1,ni2v
           if (trim(i2vars(i1,1))==trim(name)) then
             v_id=i1
             if (initmode==4) call parser(trim(name),defs%i2def(i1,:))
             if (initmode==2) then
               do i2=1,dim
                 call PARSER_convert_INT(defval(i2),i2vars(i1,3),.TRUE.)
               enddo
             endif
             if (initmode<=3) defs%i2def(i1,:)=defval
             defval=defs%i2def(i1,:)
             exit
           endif
         enddo
       case(3)
         do i1=1,ni3v
           if (trim(i3vars(i1,1))==trim(name)) then
             v_id=i1
             if (initmode==4) call parser(trim(name),defs%i3def(i1,:))
             if (initmode==2) then
               do i2=1,dim
                 call PARSER_convert_INT(defval(i2),i3vars(i1,3),.TRUE.)
               enddo
             endif
             if (initmode<=3) defs%i3def(i1,:)=defval
             defval=defs%i3def(i1,:)
             exit
           endif
         enddo
       end select
     endif
     !
     !Verbosity variables defaults update
     !
     if (initmode<=1.and.present(verb_level)) then
       if (dim==2) call initverbosity(name,v_id,'i2',defs,verb_level)
       if (dim==3) call initverbosity(name,v_id,'i3',defs,verb_level)
     endif
     !
   end subroutine
   !
   subroutine loadr1var(defs,name,description,defval,unit,verb_level)
     use parser_m,       ONLY:parser
     character(*)::name,description
     character(*), optional :: unit
     integer,optional :: verb_level
     real(SP) :: defval
     type(initdefs)::defs
     integer::v_id
     if (initmode==0) then
       r1st=0
       nr1v=nr1v+1
       if (nr1v>maxr1vars) stop 'MAXr1VARS'
       r1vars(nr1v,1)=name
       r1vars(nr1v,2)=description
       if (present(unit)) r1vars(nr1v,3)=unit
       defs%r1def(nr1v)=defval
       v_id=nr1v
     else
       do i1=1,nr1v
         if (trim(r1vars(i1,1))==trim(name)) then
           v_id=i1
           if (initmode==4) call parser(trim(name),defs%r1def(i1))
           if (initmode<=3) defs%r1def(i1)=defval
           defval=defs%r1def(i1)
           exit
         endif
       enddo
     endif
     !
     !Verbosity variables defaults update
     !
     if (initmode<=1.and.present(verb_level)) call initverbosity(name,v_id,'r1',defs,verb_level)
   end subroutine
   !
   subroutine loadc1var(defs,name,description,defval,unit,verb_level)
     use parser_m,       ONLY:parser
     character(*)::name,description
     character(*), optional :: unit
     integer,optional :: verb_level
     complex  (SP) :: defval
     type(initdefs)::defs
     integer :: v_id
     if (initmode==0) then
       c1st=0
       nc1v=nc1v+1
       if (nc1v>maxc1vars) stop 'MAXc1VARS'
       c1vars(nc1v,1)=name
       c1vars(nc1v,2)=description
       if (present(unit)) c1vars(nc1v,3)=unit
       defs%c1def(nc1v)=defval
       v_id=nc1v
     else
       do i1=1,nc1v
         if (trim(c1vars(i1,1))==trim(name)) then
           v_id=i1
           if (initmode==4) call parser(trim(name),defs%c1def(i1))
           if (initmode<=3) defs%c1def(i1)=defval
           defval=defs%c1def(i1)
           exit
         endif
       enddo
     endif
     !
     !Verbosity variables defaults update
     !
     if (initmode<=1.and.present(verb_level)) call initverbosity(name,v_id,'c1',defs,verb_level)
     !
   end subroutine
   !
   subroutine loadrnvar(defs,name,description,defval,unit,verb_level,protect)
     use drivers,       ONLY:infile_editing
     use parser_m,      ONLY:parser
     character(*)::name,description
     character(*), optional :: unit
     integer,optional :: verb_level
     logical, optional::protect
     real(SP) :: defval(:)
     type(initdefs)::defs
     integer :: dim,v_id
     dim=product(shape(defval))
     if (initmode==0) then
       select case(dim)
       case(2)
         r2st=0
         nr2v=nr2v+1
         if (nr2v>maxr2vars) stop 'MAXr2VARS'
         r2vars(nr2v,1)=name
         r2vars(nr2v,2)=description
         if (present(unit)) r2vars(nr2v,3)=unit
         defs%r2def(nr2v,:)=defval
         v_id=nr2v
       case(3)
         r3st=0
         nr3v=nr3v+1
         if (nr3v>maxr3vars) stop 'MAXr3VARS'
         r3vars(nr3v,1)=name
         r3vars(nr3v,2)=description
         if (present(unit)) r3vars(nr3v,3)=unit
         defs%r3def(nr3v,:)=defval
         v_id=nr3v
       case(4)
         r4st=0
         nr4v=nr4v+1
         if (nr4v>maxr4vars) stop 'MAXr4VARS'
         r4vars(nr4v,1)=name
         r4vars(nr4v,2)=description
         if (present(unit)) r4vars(nr4v,3)=unit
         defs%r4def(nr4v,:)=defval
         v_id=nr4v
       end select
     else
       select case(dim)
       case(2)
         do i1=1,nr2v
           if (trim(r2vars(i1,1))==trim(name)) then
             v_id=i1
             if (initmode==4) then
               if (present(protect)) then
                 if (.not.protect.and.infile_editing) cycle
               endif
               call parser(trim(name),defs%r2def(i1,:))
             endif
             if (initmode<=3) defs%r2def(i1,:)=defval
             defval=defs%r2def(i1,:)
             exit
           endif
         enddo
       case(3)
         do i1=1,nr3v
           if (trim(r3vars(i1,1))==trim(name)) then
             v_id=i1
             if (initmode==4) then
               if (present(protect)) then
                 if (.not.protect.and.infile_editing) cycle
               endif
               call parser(trim(name),defs%r3def(i1,:))
             endif
             if (initmode<=3) defs%r3def(i1,:)=defval
             defval=defs%r3def(i1,:)
             exit
           endif
         enddo
       case(4)
         do i1=1,nr4v
          if (trim(r4vars(i1,1))==trim(name)) then
            v_id=i1
            if (initmode==4) then
              if (present(protect)) then
                if (.not.protect.and.infile_editing) cycle
              endif
              call parser(trim(name),defs%r4def(i1,:))
            endif
            if (initmode<=3) defs%r4def(i1,:)=defval
            defval=defs%r4def(i1,:)
            exit
          endif
        enddo
      end select
     endif
     !
     !Verbosity variables defaults update
     !
     if (initmode<=1.and.present(verb_level)) then
       if (dim==2) call initverbosity(name,v_id,'r2',defs,verb_level)
       if (dim==3) call initverbosity(name,v_id,'r3',defs,verb_level)
       if (dim==4) call initverbosity(name,v_id,'r4',defs,verb_level)
     endif
   end subroutine
   !
   subroutine loadchvar(defs,name,description,defval,verb_level,protect,case)
     use drivers,        ONLY:infile_editing
     use parser_m,       ONLY:parser
     use stderr,         ONLY:STRING_to_UPPER,STRING_to_LOWER
     character(*)::name,description,defval
     type(initdefs)        ::defs
     integer               ::v_id
     integer,      optional::verb_level 
     logical,      optional::protect
     character(1), optional::case
     if (initmode==0) then
       chst=0
       nchv=nchv+1
       if (nchv>maxchvars) stop 'MAXchVARS' 
       chvars(nchv,1)=name
       chvars(nchv,2)=description
       defs%chdef(nchv)=trim(defval)
       if (present(case)) then
         if (case=="A") call STRING_to_UPPER(defs%chdef(nchv))
         if (case=="a") call STRING_to_LOWER(defs%chdef(nchv))
       endif
       v_id=nchv
     else
       do i1=1,nchv
         if (trim(chvars(i1,1))==trim(name)) then
           v_id=i1
           if (initmode==4) then
             if (present(protect)) then
               if (.not.protect.and.infile_editing) cycle
             endif
             call parser(trim(name),defs%chdef(i1))
           endif
           if (initmode<=3) then
             defs%chdef(i1)=defval
             if (present(case)) then
               if (case=="A") call STRING_to_UPPER(defs%chdef(i1))
               if (case=="a") call STRING_to_LOWER(defs%chdef(i1))
             endif
           endif
           defval=defs%chdef(i1)
           exit
         endif
       enddo
     endif
     !
     !Verbosity variables defaults update
     !
     if (initmode<=1.and.present(verb_level)) call initverbosity(name,v_id,'ch',defs,verb_level)
     !
   end subroutine
   !
   !@@@@@@@@@@@@@@@@@@
   ! END INTERFACE IT
   !@@@@@@@@@@@@@@@@@@
   !
   !@@@@@@@@@@@@@@@@@@
   !    VERBOSITY 
   !@@@@@@@@@@@@@@@@@@
   !
   subroutine initverbosity(var_name,var_id,var_type,defs,verbosity_level)
     integer        ::var_id,verbosity_level
     character(*)   ::var_name,var_type
     type(initdefs) ::defs
     !
     if (verbosity_level==0) return
     !
     select case (var_type)
       case('i1')
         n_verbose_Ivars=n_verbose_Ivars+1
         if (it_verbose_Itable(1,var_id)>0) then
           n_verbose_Ivars=it_verbose_Itable(1,var_id)
         else
           it_verbose_Ilevel(n_verbose_Ivars)=verbosity_level
           it_verbose_Itable(1,var_id)=n_verbose_Ivars
         endif
         it_verbose_Ivars(n_verbose_Ivars)=var_name
         it_verbose_Idefs(n_verbose_Ivars,1)=defs%i1def(var_id)
       case('i2')
         n_verbose_Ivars=n_verbose_Ivars+1
         if (it_verbose_Itable(2,var_id)>0) then
           n_verbose_Ivars=it_verbose_Itable(2,var_id)
         else
           it_verbose_Ilevel(n_verbose_Ivars)=verbosity_level
           it_verbose_Itable(2,var_id)=n_verbose_Ivars
         endif
         it_verbose_Ivars(n_verbose_Ivars)=var_name
         it_verbose_Idefs(n_verbose_Ivars,:2)=defs%i2def(var_id,:)
       case('i3')
         n_verbose_Ivars=n_verbose_Ivars+1
         if (it_verbose_Itable(3,var_id)>0) then
           n_verbose_Ivars=it_verbose_Itable(3,var_id)
         else
           it_verbose_Ilevel(n_verbose_Ivars)=verbosity_level
           it_verbose_Itable(3,var_id)=n_verbose_Ivars
         endif
         it_verbose_Ivars(n_verbose_Ivars)=var_name
         it_verbose_Idefs(n_verbose_Ivars,:3)=defs%i3def(var_id,:)
       case('r1')
         n_verbose_Rvars=n_verbose_Rvars+1
         if (it_verbose_Rtable(1,var_id)>0) then
           n_verbose_Rvars=it_verbose_Rtable(1,var_id)
         else
           it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
           it_verbose_Rtable(1,var_id)=n_verbose_Rvars
         endif
         it_verbose_Rvars(n_verbose_Rvars)=var_name
         it_verbose_Rdefs(n_verbose_Rvars,1)=defs%r1def(var_id)
       case('r2')
         n_verbose_Rvars=n_verbose_Rvars+1
         if (it_verbose_Rtable(2,var_id)>0) then
           n_verbose_Rvars=it_verbose_Rtable(2,var_id)
         else
           it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
           it_verbose_Rtable(2,var_id)=n_verbose_Rvars
         endif
         it_verbose_Rvars(n_verbose_Rvars)=var_name
         it_verbose_Rdefs(n_verbose_Rvars,:2)=defs%r2def(var_id,:)
       case('r3')
         n_verbose_Rvars=n_verbose_Rvars+1
         if (it_verbose_Rtable(3,var_id)>0) then
           n_verbose_Rvars=it_verbose_Rtable(3,var_id)
         else
           it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
           it_verbose_Rtable(3,var_id)=n_verbose_Rvars
         endif
         it_verbose_Rvars(n_verbose_Rvars)=var_name
         it_verbose_Rdefs(n_verbose_Rvars,:3)=defs%r3def(var_id,:)
       case('r4')
         n_verbose_Rvars=n_verbose_Rvars+1
         if (it_verbose_Rtable(4,var_id)>0) then
           n_verbose_Rvars=it_verbose_Rtable(4,var_id)
         else
           it_verbose_Rlevel(n_verbose_Rvars)=verbosity_level
           it_verbose_Rtable(4,var_id)=n_verbose_Rvars
         endif
         it_verbose_Rvars(n_verbose_Rvars)=var_name
         it_verbose_Rdefs(n_verbose_Rvars,:4)=defs%r4def(var_id,:)
       case('c1')
         n_verbose_Cvars=n_verbose_Cvars+1
         if (it_verbose_Ctable(var_id)>0) then
           n_verbose_Cvars=it_verbose_Ctable(var_id)
         else
           it_verbose_Clevel(n_verbose_Cvars)=verbosity_level
           it_verbose_Ctable(var_id)=n_verbose_Cvars
         endif
         it_verbose_Cvars(n_verbose_Cvars)=var_name
         it_verbose_Cdefs(n_verbose_Cvars)=defs%c1def(var_id)
       case('ch')
         n_verbose_Chvars=n_verbose_Chvars+1
         if (it_verbose_Chtable(var_id)>0) then
           n_verbose_Chvars=it_verbose_Chtable(var_id)
         else
           it_verbose_Chlevel(n_verbose_Chvars)=verbosity_level
           it_verbose_Chtable(var_id)=n_verbose_Chvars
         endif
         it_verbose_Chvars(n_verbose_Chvars)=var_name
         it_verbose_Chdefs(n_verbose_Chvars)=defs%chdef(var_id)
       case('f')
         n_verbose_Fvars=n_verbose_Fvars+1
         if (it_verbose_Ftable(var_id)>0) then
           n_verbose_Fvars=it_verbose_Ftable(var_id)
         else
           it_verbose_Flevel(n_verbose_Fvars)=verbosity_level
           it_verbose_Ftable(var_id)=n_verbose_Fvars
         endif
         it_verbose_Fvars(n_verbose_Fvars)=var_name
     end select
   end subroutine
   !
   !@@@@@@@@@@@@@@@@@@
   ! END VERBOSITY 
   !@@@@@@@@@@@@@@@@@@
   !
   !@@@@@@@@@@@@@@@@@@@@@
   ! INPUT FILE WRITING
   !@@@@@@@@@@@@@@@@@@@@@
   ! 
   subroutine initinfio(defs,ou)
     use stderr,       ONLY: intc
     use drivers,      ONLY: infile_editing
     use stderr,       ONLY: gen_fmt
     use com,          ONLY: write_the_logo,repfile
     use parallel_m,   ONLY: master_cpu
     type(initdefs)::defs
     integer       :: ou
     ! 
     ! Work Space
     !
     integer :: i2,ifpos,ch_length
     real(SP):: unit_factor,v(2)
     character(lchlen)::ch(2)  
     !
     if (ou==11) open(unit=11,file=trim(repfile),position='append')
     !
     nflines=0
     flines=' '
     do ifpos=1,nifpos
       do i2=1,nrnlvls
         if (rstatus(i2)==ifpos) then
           nflines=nflines+1
           write (flines(nflines),'(a,t32,2a)') trim(rnlvls(i2,1)),'# ',trim(rnlvls(i2,2))
         endif
       enddo
       !
       ! Variables
       !
       do i2=1,nchv
         if (chst(i2)==ifpos) then
           if (.not.Chverbose_enough(trim(chvars(i2,1)),defs%chdef(i2))) cycle
           nflines=nflines+1
           !
           ch_length=max(len_trim(defs%chdef(i2))+len_trim(chvars(i2,1))+6,32)
           !  
           ch(1)='(4a,t'//trim(intc(ch_length))//',2a)'
           write (flines(nflines),trim(ch(1))) &                                                     !
&            trim(chvars(i2,1)),'= "',trim(defs%chdef(i2)),'"','# ',trim(chvars(i2,2))               !
         endif
       enddo
       do i2=1,ni1v
         if (i1st(i2)==ifpos) then
           if (.not.Iverbose_enough(trim(i1vars(i2,1)),1,defs%i1def(i2))) cycle
           nflines=nflines+1
           if (ou/=20) call PARSER_convert_INT(defs%i1def(i2),i1vars(i2,3),.FALSE.)
           ch(2)=gen_fmt((/defs%i1def(i2)/))
           write (ch(1),'(3a)') '(2a,',trim(ch(2)),',t24,a,t32,2a)'
           write (flines(nflines),trim(ch(1))) trim(i1vars(i2,1)),'=',&
&                defs%i1def(i2),trim(i1vars(i2,3)),'# ',trim(i1vars(i2,2))
         endif
       enddo
       do i2=1,ni2v
         if (i2st(i2)==ifpos) then
           if (.not.Iverbose_enough(trim(i2vars(i2,1)),2,defs%i2def(i2,:))) cycle
           if (ou/=20) call PARSER_convert_INT(defs%i2def(i2,1),i2vars(i2,3),.FALSE.)
           if (ou/=20) call PARSER_convert_INT(defs%i2def(i2,2),i2vars(i2,3),.FALSE.)
           nflines=nflines+1
           write (flines(nflines),'(2a)') '% ',trim(i2vars(i2,1))
           ch(2)=gen_fmt(defs%i2def(i2,:))
           write (ch(1),'(3a)') '(2(',trim(ch(2)),',1x,a),t24,a,t32,2a)'
           nflines=nflines+1
           write (flines(nflines),trim(ch(1))) &
&           defs%i2def(i2,1),'|',defs%i2def(i2,2),'|',&
&           trim(i2vars(i2,3)),'# ',trim(i2vars(i2,2))
           nflines=nflines+1
           write (flines(nflines),'(a)') '% '
         endif
       enddo
       do i2=1,ni3v
         if (i3st(i2)==ifpos) then
           if (.not.Iverbose_enough(trim(i3vars(i2,1)),3,defs%i3def(i2,:))) cycle
           if (ou/=20) call PARSER_convert_INT(defs%i3def(i2,1),i3vars(i2,3),.FALSE.)
           if (ou/=20) call PARSER_convert_INT(defs%i3def(i2,2),i3vars(i2,3),.FALSE.)
           if (ou/=20) call PARSER_convert_INT(defs%i3def(i2,3),i3vars(i2,3),.FALSE.)
           nflines=nflines+1
           write (flines(nflines),'(2a)') '% ',trim(i3vars(i2,1))
           ch(2)=gen_fmt(defs%i3def(i2,:))
           write (ch(1),'(3a)') '(3(',trim(ch(2)),',1x,a),t36,a,t42,2a)'
           nflines=nflines+1
           write (flines(nflines),trim(ch(1))) &
&           defs%i3def(i2,1),'|',defs%i3def(i2,2),'|',defs%i3def(i2,3),'|',&
&           trim(i3vars(i2,3)),'# ',trim(i3vars(i2,2))
           nflines=nflines+1
           write (flines(nflines),'(a)') '% '
         endif
       enddo
       do i2=1,nc1v
         if (c1st(i2)==ifpos) then
           if (.not.Cverbose_enough(trim(c1vars(i2,1)),1,defs%c1def(i2))) cycle
           unit_factor=1.
           call PARSER_convert_REAL(unit_factor,c1vars(i2,3))
           nflines=nflines+1
           v(1)=real(defs%c1def(i2))*unit_factor
           v(2)=aimag(defs%c1def(i2))*unit_factor
           ch(2)=gen_fmt(r_v=v)
           write (ch(1),'(3a)') '(2a,2(',trim(ch(2)),',1x,a),t36,a,t40,2a)'
           write (flines(nflines),trim(ch(1))) &
&                trim(c1vars(i2,1)),'= (',real(defs%c1def(i2))*unit_factor,',',&
&                aimag(defs%c1def(i2))*unit_factor,')',&
&                trim(c1vars(i2,3)),'# ',trim(c1vars(i2,2))
         endif
       enddo
       do i2=1,nr1v
         if (r1st(i2)==ifpos) then
           if (.not.Rverbose_enough(trim(r1vars(i2,1)),1,defs%r1def(i2))) cycle
           unit_factor=1.
           call PARSER_convert_REAL(unit_factor,r1vars(i2,3))
           nflines=nflines+1
           ch(2)=gen_fmt(r_v=(/defs%r1def(i2)*unit_factor/))
           write (ch(1),'(3a)') '(2a,',trim(ch(2)),',t24,a,t32,2a)'
           write (flines(nflines),trim(ch(1))) &
&                trim(r1vars(i2,1)),'=',defs%r1def(i2)*unit_factor,&
&                trim(r1vars(i2,3)),'# ',trim(r1vars(i2,2))
         endif
       enddo
       do i2=1,nr2v
         if (r2st(i2)==ifpos) then
           if (.not.Rverbose_enough(trim(r2vars(i2,1)),2,defs%r2def(i2,:))) cycle
           unit_factor=1.
           call PARSER_convert_REAL(unit_factor,r2vars(i2,3))
           nflines=nflines+1
           write (flines(nflines),'(2a)') '% ',trim(r2vars(i2,1))
           ch(2)=gen_fmt(r_v=defs%r2def(i2,:)*unit_factor)
           write (ch(1),'(3a)') '(2(',trim(ch(2)),',1x,a),t24,a,t32,2a)'
           nflines=nflines+1
           write (flines(nflines),trim(ch(1))) &
&           defs%r2def(i2,1)*unit_factor,'|',defs%r2def(i2,2)*unit_factor,&
&           '|',trim(r2vars(i2,3)),'# ',trim(r2vars(i2,2))
           nflines=nflines+1
           write (flines(nflines),'(a)') '% '
         endif
       enddo
       do i2=1,nr3v
         if (r3st(i2)==ifpos) then
           if (.not.Rverbose_enough(trim(r3vars(i2,1)),3,defs%r3def(i2,:))) cycle
           unit_factor=1.
           call PARSER_convert_REAL(unit_factor,r3vars(i2,3))
           nflines=nflines+1
           write (flines(nflines),'(2a)') '% ',trim(r3vars(i2,1))
           ch(2)=gen_fmt(r_v=defs%r3def(i2,:)*unit_factor)
           write (ch(1),'(3a)') '(3(',trim(ch(2)),',1x,a),t36,a,t42,2a)'
           nflines=nflines+1
           write (flines(nflines),trim(ch(1))) &
&           defs%r3def(i2,1)*unit_factor,'|',defs%r3def(i2,2)*unit_factor,&
&           '|',defs%r3def(i2,3)*unit_factor,'|',&
&           trim(r3vars(i2,3)),'# ',trim(r3vars(i2,2))
           nflines=nflines+1
           write (flines(nflines),'(a)') '% '
         endif
       enddo
       do i2=1,nr4v
         if (r4st(i2)==ifpos) then
           if (.not.Rverbose_enough(trim(r4vars(i2,1)),4,defs%r4def(i2,:))) cycle
           unit_factor=1.
           call PARSER_convert_REAL(unit_factor,r4vars(i2,3))
           nflines=nflines+1
           write (flines(nflines),'(2a)') '% ',trim(r4vars(i2,1))
           ch(2)=gen_fmt(r_v=defs%r4def(i2,:)*unit_factor)
           write (ch(1),'(3a)') '(4(',trim(ch(2)),',1x,a),t46,a,t50,2a)'
           nflines=nflines+1
           write (flines(nflines),trim(ch(1))) &
&           defs%r4def(i2,1)*unit_factor,'|',defs%r4def(i2,2)*unit_factor,&
&           '|',defs%r4def(i2,3)*unit_factor,'|', defs%r4def(i2,4)*unit_factor,'|',&
&           trim(r4vars(i2,3)),'# ',trim(r4vars(i2,2))
           nflines=nflines+1
           write (flines(nflines),'(a)') '% '
         endif
       enddo
       do i2=1,nflags
         if (fstatus(i2)==ifpos) then
           if (.not.Fverbose_enough(trim(flags(i2,1)),i2)) cycle
           nflines=nflines+1
           write (flines(nflines),'(2a,t32,2a)') '#',trim(flags(i2,1)),'# ',trim(flags(i2,2))
         else if (fstatus(i2)==-ifpos) then
           if (.not.Fverbose_enough(trim(flags(i2,1)),i2)) cycle
           nflines=nflines+1
           write (flines(nflines),'(a,t32,2a)') trim(flags(i2,1)),'# ',trim(flags(i2,2))
         endif
       enddo
     enddo
     !
     if (.not.infile_editing.and.ou==12) return
     if (master_cpu) then
       if (ou==12) call write_the_logo(ou,'#')
       if (ou==11) write (ou,'(2a)') ' .-Input file : ',trim(infile)
     endif
     if (ou/=11.and.ou/=12) write (ou,'(a/2a)') '#','# .-Input file : ',trim(infile)
     do i1=1,nflines
       if (master_cpu) then
         if (ou==12) write (ou,'(a)') trim(flines(i1))
         if (ou==11) write (ou,'(2a)') ' | ',trim(flines(i1))
       endif
       if (ou/=11.and.ou/=12) write (ou,'(2a)') '# | ',trim(flines(i1))
      enddo
     call PARSER_dump_the_additional_lines(ou)
     !
     if (ou==11) close(11)
     !
   end subroutine
   !
   logical function Iverbose_enough(var_name,nvar,var_val)
     integer :: nvar
     character(*)::var_name
     integer :: var_val(nvar)
     Iverbose_enough=.false.
     do i1=1,n_verbose_Ivars
       if (it_verbose_Ilevel(i1)==0) cycle
       if (trim(it_verbose_Ivars(i1))==trim(var_name))then
         if(all(var_val==it_verbose_Idefs(i1,:nvar)).and.&
&           infile_verbosity/=V_all.and.&
&           infile_verbosity/=it_verbose_Ilevel(i1)) return
       endif
     enddo
     Iverbose_enough=.true.
   end function
   !
   logical function Rverbose_enough(var_name,nvar,var_val)
     integer     ::nvar
     character(*)::var_name
     real(SP)    ::var_val(nvar)
     ! 
     ! Work Space
     !
     Rverbose_enough=.false.
     do i1=1,n_verbose_Rvars
       if (it_verbose_Rlevel(i1)==0) cycle
       if (trim(it_verbose_Rvars(i1))==trim(var_name))then
         if(all(var_val==it_verbose_Rdefs(i1,:nvar)).and.&
&           infile_verbosity/=V_all.and.&
&               infile_verbosity/=it_verbose_Rlevel(i1)) return
       endif
     enddo
     Rverbose_enough=.true.
   end function
   !
   logical function Cverbose_enough(var_name,nvar,var_val)
     integer     ::nvar
     character(*)::var_name
     complex(SP) ::var_val(nvar)
     ! 
     ! Work Space
     !
     Cverbose_enough=.false.
     do i1=1,n_verbose_Cvars
       if (it_verbose_Clevel(i1)==0) cycle
       if (trim(it_verbose_Cvars(i1))==trim(var_name))then
         if(var_val(1)==it_verbose_Cdefs(i1).and.&
&           infile_verbosity/=V_all.and.&
&           infile_verbosity/=it_verbose_Clevel(i1)) return
       endif
     enddo
     Cverbose_enough=.true.
   end function
   !
   logical function Chverbose_enough(var_name,var_val)
     character(*)::var_name
     character(*)::var_val
     Chverbose_enough=.false.
     do i1=1,n_verbose_Chvars
       if (it_verbose_Chlevel(i1)==0) cycle
       if (trim(it_verbose_Chvars(i1))==trim(var_name))then
         if(var_val==it_verbose_Chdefs(i1).and.&
&           infile_verbosity/=V_all.and.&
&           infile_verbosity/=it_verbose_Chlevel(i1)) return
       endif
     enddo
     Chverbose_enough=.true.
   end function
   !
   logical function Fverbose_enough(var_name,ivar)
     integer     ::ivar
     character(*)::var_name
     ! 
     ! Work Space
     !
     Fverbose_enough=.false.
     do i1=1,n_verbose_Fvars
      if (it_verbose_Flevel(i1)==0) cycle
      if (trim(it_verbose_Fvars(i1))==trim(var_name))then
        if (fstatus(ivar)>0.and.&
&           infile_verbosity/=V_all.and.&
&          infile_verbosity/=it_verbose_Flevel(i1)) return
       endif
     enddo
     Fverbose_enough=.true.
   end function
   !
   !@@@@@@@@@@@@@@@@@@@@@@@@
   ! END INPUT FILE WRITING
   !@@@@@@@@@@@@@@@@@@@@@@@@
   !
   !@@@@@@@@@@@@@@@@@@@@@@@@
   !  ELEMENTS ACTIVATION
   !@@@@@@@@@@@@@@@@@@@@@@@@
   !
   subroutine initactivate(mode,names)
     !
     ! mode =  1 : activate var names
     ! mode = -1 : deactivate var names
     !
     integer ::      mode
     character(*) :: names
     !
     ! Work Space
     !
     integer ipos,mxpos
     !
     !
     mxpos=-1
     do i1=1,nrnlvls
       ipos=index(names,trim(rnlvls(i1,1)))
       if (ipos==0) cycle
       if (ipos> 1) then 
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(rstatus(i1),ipos)
     enddo
     do i1=1,ni1v
       ipos=index(names,trim(i1vars(i1,1)))
       if (ipos==0) cycle
       if (ipos> 1) then
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(i1st(i1),ipos)
     enddo
     do i1=1,ni2v
       ipos=index(names,trim(i2vars(i1,1)))
       if (ipos==0) cycle
       if (ipos>1) then
         if(names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(i2st(i1),ipos)
     enddo
     do i1=1,ni3v
       ipos=index(names,trim(i3vars(i1,1)))
       if (ipos==0) cycle
       if (ipos>1) then
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(i3st(i1),ipos)
     enddo
     do i1=1,nr1v
       ipos=index(names,trim(r1vars(i1,1)))
       if (ipos==0) cycle
       if (mode==2) it_verbose_Rlevel(it_verbose_Rtable(1,i1))=0
       if (ipos>1 ) then
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(r1st(i1),ipos)
     enddo
     do i1=1,nr2v
       ipos=index(names,trim(r2vars(i1,1)))
       if (ipos>0 .and. mode==2) it_verbose_Rlevel(it_verbose_Rtable(2,i1))=0
       if (ipos>1) then
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(r2st(i1),ipos)
     enddo
     do i1=1,nr3v
       ipos=index(names,trim(r3vars(i1,1)))
       if (ipos==0) cycle
       if (mode==2) it_verbose_Rlevel(it_verbose_Rtable(3,i1))=0
       if (ipos>1) then
         if(names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(r3st(i1),ipos)
     enddo
     do i1=1,nr4v
       ipos=index(names,trim(r4vars(i1,1)))
       if (ipos==0) cycle
       if (mode==2) it_verbose_Rlevel(it_verbose_Rtable(4,i1))=0
       if (ipos>1) then
         if(names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(r4st(i1),ipos)
     enddo
     do i1=1,nc1v
       ipos=index(names,trim(c1vars(i1,1)))
       if (ipos==0) cycle
       if (ipos>1) then
         if( names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(c1st(i1),ipos)
     enddo
     do i1=1,nchv
       ipos=index(names,trim(chvars(i1,1)))
       if (ipos==0) cycle
       if (ipos>1) then
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(chst(i1),ipos)
     enddo
     do i1=1,nflags
       ipos=index(names,trim(flags(i1,1)))
       if (ipos==0) cycle
       if (ipos>1) then
         if (names(ipos-1:ipos-1)/=" ") cycle
       endif
       call activate(fstatus(i1),ipos)
     enddo
     nifpos=nifpos+mxpos+1
     !
     contains
       !
       subroutine activate(iflg,ip)
         integer :: iflg,ip
         if (ip==0) return
         if (mode<0) then
           iflg=-1001 ! pushed to -1001 to not be considered at all
           return
         endif
         mxpos=max(mxpos,ip)
         if (iflg/=0.and.iflg<-1000) return
         if (iflg==0) iflg=nifpos+ip
         if (iflg< 0) iflg=-nifpos-ip
       end subroutine
       !
   end subroutine
   !
   !@@@@@@@@@@@@@@@@@@@@@@@@@
   ! END ELEMENTS ACTIVATION
   !@@@@@@@@@@@@@@@@@@@@@@@@@
   !
end module it_m
