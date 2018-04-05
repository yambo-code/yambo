!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine PARSER_array(mode,VAR,N,M,DESC,CH_v,R_v,I_v)
 !
 use pars,      ONLY:lchlen,SP
 use parser_m,  ONLY:parser
 use it_m,      ONLY:nADDITIONAL_flines,ADDITIONAL_flines
 !
 implicit none
 !
 character(*)                ::mode
 integer                     ::N,M
 character(*)                ::VAR
 character(*),      OPTIONAL ::DESC
 character(lchlen), OPTIONAL ::CH_v(:,:)
 integer,           OPTIONAL ::I_v(:,:)
 real(SP),          OPTIONAL ::R_v(:,:)
 !
 ! Work Space
 !
 integer           :: i1,i2
 character(lchlen)              :: ch
 character(lchlen), allocatable :: TMP_CH_v(:,:)
 integer,           allocatable :: TMP_I_v(:,:)
 real(SP),          allocatable :: TMP_R_v(:,:)
 logical                        :: l_I,l_R,l_CH,l_last
 !
 l_I = index(mode,"INT ")>0.or.present(I_v)
 l_R = index(mode,"REAL ")>0.or.present(R_v)
 l_CH= index(mode,"CH ")>0.or.present(CH_v)
 !
 if (index(mode,"dimensions")>0) then
   l_last=.FALSE.
   N=1
   do while(.not.l_last)
     if (l_CH) then
       allocate(TMP_CH_v(N,M))
       TMP_CH_v= " "
       call parser(VAR,TMP_CH_v(1:N,:))
       l_last=len_trim(TMP_CH_v(N,1))==0
       deallocate(TMP_CH_v)
     else if (l_I) then
       allocate(TMP_I_v(N,M))
       TMP_I_v=-99999
       call parser(VAR,TMP_I_v(1:N,:))
       l_last=TMP_I_v(N,1)==-99999
       deallocate(TMP_I_v)
     else if (l_R) then
       allocate(TMP_R_v(N,M))
       TMP_R_v=-99999.
       call parser(VAR,TMP_R_v(1:N,:))
       l_last=(abs(TMP_R_v(N,1)+99999.)<1.)
       deallocate(TMP_R_v)
     endif
     if (.not.l_last) N=N+1
   enddo
   N=N-1
 else if (trim(mode)=="read") then
   if (l_CH) call parser(VAR,CH_v(1:N,:))
   if (l_R)  call parser(VAR,R_v(1:N,:))
   if (l_I)  call parser(VAR,I_v(1:N,:))
 else if (trim(mode)=="write") then
   write (ADDITIONAL_flines(nADDITIONAL_flines+1),'(a,t32,a)') '%'//trim(VAR),'# '//DESC
   nADDITIONAL_flines=nADDITIONAL_flines+1
   do i1=1,N
     ADDITIONAL_flines(nADDITIONAL_flines+1)=""
     if (present(CH_v)) then
       do i2=1,M
         ch=trim(ADDITIONAL_flines(nADDITIONAL_flines+1))
         ADDITIONAL_flines(nADDITIONAL_flines+1)=trim(ch)//'"'//trim(CH_v(i1,i2))//'" |'
       enddo
     endif
     if (present(I_v)) then
       do i2=1,M
         ch=trim(ADDITIONAL_flines(nADDITIONAL_flines+1))
         write (ADDITIONAL_flines(nADDITIONAL_flines+1),'(a,i3,a)') trim(ch),I_v(i1,i2),'|'
       enddo
     endif
     if (present(R_v)) then
       do i2=1,M
         ch=trim(ADDITIONAL_flines(nADDITIONAL_flines+1))
         write (ADDITIONAL_flines(nADDITIONAL_flines+1),'(a,f4.1,a)') trim(ch),R_v(i1,i2),'|'
       enddo
     endif
     nADDITIONAL_flines=nADDITIONAL_flines+1
   enddo
   ADDITIONAL_flines(nADDITIONAL_flines+1)="%"
   nADDITIONAL_flines=nADDITIONAL_flines+1
 endif
 !
end subroutine
