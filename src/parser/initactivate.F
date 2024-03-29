!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine initactivate(mode,names)
 !
 use it_tools
 use it_m
 !
 ! mode =  2 : force activate regardless of the verbosity
 ! mode =  1 : activate var names
 ! mode = -1 : deactivate var names
 !
 implicit none
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
