!
! Copyright (C) 2000-2016 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine init_q_pts(l_WRITE)
 !
 use pars,        ONLY:SP
 use memory_m,    ONLY:mem_est
 use it_m,        ONLY:nflines,flines,infile,infile_verbosity,V_kpt,V_all
 use R_lattice,   ONLY:nqibz,q_pt,bz_samp,bz_samp_reset
 use vec_operate, ONLY:c2a
 implicit none
 !
 logical :: l_WRITE
 !
 ! Work Space
 !
 integer       :: i1,nflines_save
 type(bz_samp) :: q
 !
 if (l_WRITE.and. (infile_verbosity==V_kpt.or.infile_verbosity==V_all)) then
   nflines_save=nflines
   write (flines(nflines+1),'(a,t30,a)') '% QptCoord',&
&        '# [KPT] [iku] Q-points coordinates (compatibility)'
   nflines=nflines+1
   if (nqibz>0) then
     do i1=1,nqibz
       write (flines(nflines+1),'(3(f10.3,a))') q_pt(i1,1),'|',&
&             q_pt(i1,2),'|',q_pt(i1,3),'|'
       nflines=nflines+1
     enddo
   endif
   flines(nflines+1)='%'
   nflines=nflines+1
 else
   call bz_samp_reset(q)
   call parse_K_list(q,'QptCoord')
   nqibz=q%nibz
   if (nqibz>0)  then
     allocate(q_pt(nqibz,3))
     q_pt=q%pt
     call mem_est("q_pt",(/nqibz*3/),(/SP/))  
   endif
   call bz_samp_reset(q)
   return
 endif 
 !
 ! Write to input file
 !
 open(unit=12,file=trim(infile),position='append')
 do i1=nflines_save+1,nflines
   write (12,'(a)') trim(flines(i1))
 enddo 
 close(12)
 !
end subroutine
