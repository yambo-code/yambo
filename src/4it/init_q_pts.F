!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine init_q_pts()
 !
 use pars,      ONLY:SP
 use drivers,   ONLY:infile_editing
 use memory_m,  ONLY:mem_est
 use it_m,      ONLY:nflines,flines,infile,infile_verbosity,V_more
 use R_lattice, ONLY:nqibz,q_pt
 use parser_m,  ONLY:parser
 implicit none
 !
 integer :: i1,nflines_save,i_nqibz ! Work Space
 !
 if (infile_editing.and.infile_verbosity==V_more) then
   nflines_save=nflines
   write (flines(nflines+1),'(a,i4,t30,a)') 'nQptIBZ =',nqibz,&
&        '# [KPT] N. of Q-points in the IBZ (compatibility)'
   nflines=nflines+1
   write (flines(nflines+1),'(a,t30,a)') '% QptCoord',&
&        '# [KPT] [iku] Q-points coordinates (compatibility)'
   nflines=nflines+1
   write (flines(nflines+1),'(3(f5.2,1x,a))') 0.,'|',0.,'|',0.,'|'
   nflines=nflines+1
   flines(nflines+1)='%'
   nflines=nflines+1
 else
   i_nqibz=0
   call parser('nQptIBZ',i_nqibz)
   if (i_nqibz==0) return
   allocate(q_pt(i_nqibz,3))
   q_pt=0.
   nqibz=i_nqibz
   call parser('QptCoord',q_pt)    
   call mem_est("q_pt",(/nqibz*3/),(/SP/))  
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
