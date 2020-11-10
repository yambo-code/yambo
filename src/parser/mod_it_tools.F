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
module it_tools
 !
 use it_m
 !
 implicit none
 !
 interface it
   module procedure loadrnlvlflag,loadi1var,loadinvar,loadr1var,loadrnvar,&
&                   loadc1var,loadchvar
 end interface it
 !
 interface
   !
   subroutine PARSER_force_command_line_value(defs,name,I1,R1,CHAR)
     use it_m,          ONLY:initdefs
     use pars,          ONLY:SP
     type(initdefs)        :: defs
     character(*)          :: name
     integer, optional     :: I1
     real(SP),optional     :: R1
     character(*),optional :: CHAR
   end subroutine
   !
 end interface
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
     use com,  ONLY:inpfile_unit
     character(lchlen) :: ch
     open(unit=inpfile_unit,file=trim(infile))
     do i1=1,10000
       read(inpfile_unit,'(a)',end=1) ch
       flines(i1)=ch
       if (index(ch,'#')>0) flines(i1)=ch(:index(ch,'#')-1)
       nflines=i1
     enddo
1    close(inpfile_unit)
   end subroutine
   !
   subroutine ofiles_append(line,defs)
     use com,           ONLY: n_ofs_defined,of_name,write_to_of
     character(*),optional  :: line
     type(initdefs),optional::defs
     integer :: i2
     do i2=1,n_ofs_defined
       if (len_trim(of_name(i2))==0) cycle
       if (.not.write_to_of) cycle
       open(20,file=trim(of_name(i2)),position='append')
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
     character(schlen):: on_names(maxrnlvls),off_names(maxrnlvls)
     integer          :: i2,i3
     call STRING_split(on_name,on_names)
     call STRING_split(off_name,off_names)
     ext_loop: do i2=1,nrnlvls
       if (rstatus(i2)==0) cycle
       do i3=1,maxrnlvls
         if (len_trim(on_names(i3))==0) cycle
         if (trim(rnlvls(i2,1))==trim(on_names(i3))) cycle ext_loop
       enddo
       do i3=1,maxrnlvls
         if (len_trim(off_names(i3))==0) cycle
         if (trim(off_names(i3))=="all"             ) rstatus(i2)=0
         if (trim(rnlvls(i2,1))==trim(off_names(i3))) rstatus(i2)=0
       enddo
     enddo ext_loop
   end subroutine
   ! 
   !@@@@@@@@@@@@@@
   ! INTERFACE IT
   !@@@@@@@@@@@@@@
   !
   ! Modes
   !
   ! initmode 0'load'      : defs% <- Default from def+stdefs routines
   !                          initdefs is initialized
   ! initmode 1'todef'     : Like 'load' but as DBs have been read
   !                          defs% are redefined
   ! initmode 2'Gclose'    : like  'todef' + Close the G-vectors to the G-shells
   ! initmode 3'GameOver'  : Like 'todef' but verbosity defaults are not updated.
   !                          To be used at the run end
   ! initmode 4'parserload': defs% (and associated variables of the mods) 
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
   !@@@@@@@@@@@@@@@@@@@@@
   ! INPUT FILE WRITING
   !@@@@@@@@@@@@@@@@@@@@@
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
end module it_tools
