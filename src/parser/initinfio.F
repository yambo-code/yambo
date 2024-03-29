!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine initinfio(defs,ou)
 !
 !@@@@@@@@@@@@@@@@@@@@@
 ! INPUT FILE WRITING
 !@@@@@@@@@@@@@@@@@@@@@
 ! 
 use drivers,      ONLY: infile_editing
 use stderr,       ONLY: gen_fmt,intc
 use com,          ONLY: repfile,inpfile_unit,repfile_unit,write_to_report
 use parallel_m,   ONLY: master_cpu
 use it_tools
 use it_m
 !
 implicit none
 !
 type(initdefs)::defs
 integer       :: ou
 ! 
 ! Work Space
 !
 integer          :: i2,ifpos,ch_length
 real(SP)         :: unit_factor,v(2)
 character(lchlen)::ch(2)
 !
 ! Positioning
 !
 integer          :: desc_pos(4)
 character(schlen):: unit_fmt(4),desc_fmt(4) ! 1D, 2D, 3D and 4D vars
 !
 desc_pos=(/34,38,42,46/)
 do i2=1,4
   unit_fmt(i2)=",t"//trim(intc(desc_pos(i2)-6))//",a"
   desc_fmt(i2)=",t"//trim(intc(desc_pos(i2)))//",2a)"
 enddo
 !
 if (ou==repfile_unit) then
   if(.not.write_to_report) return
   open(unit=repfile_unit,file=trim(repfile),position='append')
 endif
 !
 nflines=0
 flines=' '
 do ifpos=1,nifpos
   do i2=1,nrnlvls
     if (rstatus(i2)==ifpos) then
       nflines=nflines+1
       write (flines(nflines),'(a'//trim(desc_fmt(1))) trim(rnlvls(i2,1)),'# ',trim(rnlvls(i2,2))
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
       ch_length=max(len_trim(defs%chdef(i2))+len_trim(chvars(i2,1))+6,desc_pos(1))
       !  
       ch(1)='(4a,t'//trim(intc(ch_length))//',2a)'
       write (flines(nflines),trim(ch(1))) &
&            trim(chvars(i2,1)),'= "',trim(defs%chdef(i2)),'"','# ',trim(chvars(i2,2))
     endif
   enddo
   do i2=1,ni1v
     if (i1st(i2)==ifpos) then
       if (.not.Iverbose_enough(trim(i1vars(i2,1)),1,defs%i1def(i2))) cycle
       nflines=nflines+1
       if (ou/=20) call PARSER_convert_INT(defs%i1def(i2),i1vars(i2,3),.FALSE.)
       ch(2)=gen_fmt((/defs%i1def(i2)/))
       write (ch(1),'(2a)') '(2a,',trim(ch(2))//trim(unit_fmt(1))//trim(desc_fmt(1))
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
       write (ch(1),'(3a)') '(2(',trim(ch(2)),',1x,a)'//trim(unit_fmt(2))//trim(desc_fmt(2))
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
       write (ch(1),'(3a)') '(3(',trim(ch(2)),',1x,a)'//trim(unit_fmt(3))//trim(desc_fmt(3))
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
       write (ch(1),'(3a)') '(2a,2(',trim(ch(2)),',1x,a)'//trim(unit_fmt(3))//trim(desc_fmt(3))
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
       write (ch(1),'(2a)') '(2a,',trim(ch(2))//trim(unit_fmt(1))//trim(desc_fmt(1))
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
       write (ch(1),'(3a)') '(2(',trim(ch(2)),',1x,a)'//trim(unit_fmt(2))//trim(desc_fmt(2))
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
       write (ch(1),'(3a)') '(3(',trim(ch(2)),',1x,a)'//trim(unit_fmt(3))//trim(desc_fmt(3))
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
       write (ch(1),'(3a)') '(4(',trim(ch(2)),',1x,a)'//trim(unit_fmt(4))//trim(desc_fmt(4))
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
 if (.not.infile_editing.and.ou==inpfile_unit) return
 if (master_cpu) then
   if (ou==inpfile_unit) call write_the_logo(ou,'#')
   if (ou==repfile_unit) write (ou,'(2a)') ' .-Input file  ',trim(infile)
 endif
 if (ou/=repfile_unit.and.ou/=inpfile_unit) write (ou,'(a/2a)') '#','# .-Input file  ',trim(infile)
 do i1=1,nflines
   if (master_cpu) then
     if (ou==inpfile_unit) write (ou,'(a)') trim(flines(i1))
     if (ou==repfile_unit) write (ou,'(2a)') ' | ',trim(flines(i1))
   endif
   if (ou/=repfile_unit.and.ou/=inpfile_unit) write (ou,'(2a)') '# | ',trim(flines(i1))
  enddo
 call PARSER_dump_the_additional_lines(ou)
 !
 if (ou==repfile_unit) close(repfile_unit)
 !
end subroutine
