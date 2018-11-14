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
subroutine PARSER_array(mode,VAR,N,M,DESC,CH_v,R_v,I_v,Kn)
 !
 use pars,      ONLY:lchlen,SP,schlen
 use parser_m,  ONLY:parser
 use stderr,    ONLY:gen_fmt,intc
 use it_m,      ONLY:nADDITIONAL_flines,ADDITIONAL_flines
 !
 implicit none
 !
 character(*)                ::mode
 integer                     ::N,M
 character(*)                ::VAR
 character(*),      OPTIONAL ::DESC
 character(*),      OPTIONAL ::CH_v(N,M)
 integer,           OPTIONAL ::I_v(N,M)
 real(SP),          OPTIONAL ::R_v(N,M)
 character(1),      OPTIONAL ::Kn(M)
 !
 ! Work Space
 !
 integer           :: i1,i2
 character(schlen)              :: fmt
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
   if (l_CH) then
     CH_v=" "
     call parser(VAR,CH_v(1:N,:))
   endif
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
         ADDITIONAL_flines(nADDITIONAL_flines+1)=trim(ch)//trim(intc(I_v(i1,i2)))//'|'
       enddo
     endif
     if (present(R_v)) then
       do i2=1,M
         fmt=gen_fmt(r_v=(/R_v(i1,i2)/))
         ch=trim(ADDITIONAL_flines(nADDITIONAL_flines+1))
         if (present(Kn)) then
           if (Kn(i2)=="i") then
             ADDITIONAL_flines(nADDITIONAL_flines+1)=trim(ch)//trim(intc(int(R_v(i1,i2))))//'|'
           else
             write (ADDITIONAL_flines(nADDITIONAL_flines+1),'(a,'//trim(fmt)//',a)') trim(ch),R_v(i1,i2),'|'
           endif
         else
           write (ADDITIONAL_flines(nADDITIONAL_flines+1),'(a,'//trim(fmt)//',a)') trim(ch),R_v(i1,i2),'|'
         endif
       enddo
     endif
     nADDITIONAL_flines=nADDITIONAL_flines+1
   enddo
   ADDITIONAL_flines(nADDITIONAL_flines+1)="%"
   nADDITIONAL_flines=nADDITIONAL_flines+1
 endif
 !
end subroutine
