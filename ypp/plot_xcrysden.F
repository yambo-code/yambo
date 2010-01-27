!
! Copyright (C) 2000-2010 D. Varsano, A. Marini and the YAMBO team
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
subroutine plot_xcrysden(use_2d)
 !
 use pars,        ONLY:SP,schlen
 use units,       ONLY:BOHR
 use LOGO,        ONLY:code_version
 use com,         ONLY:msg
 use YPP,         ONLY:nr,v2plot,ncell,v2plot2D,r_hole,plot_dim,l_free_hole,&
&                      l_norm_to_one,WF_multiplier,l_exc_wf,plot_title
 use D_lattice,   ONLY:n_atomic_species,n_atoms_species,a,atom_pos,Z_species,n_atoms
 use timing,      ONLY:live_timing
 !
 implicit none
 logical  :: use_2d(3)
 !
 ! Work Space...
 !
 integer  :: i1,i2,i3,ir,is,ia,dir2plot(2),nr2plot(2)
 real(SP) :: a_angs(3,3)    !lattice vectors in Angstrom
 real(SP) :: rv(3),max_
 character(schlen) :: ch
 ! 
 !the atomic cell is given in a.u. and I want
 !to write the output of xcrysden in \AA
 !
 a_angs(:,:) = a(:,:)*BOHR
 !
 !This is the output file common at both grids
 !
 call msg('o exc den mag wf','CRYSTAL')
 call msg('o exc den mag wf','PRIMVEC')
 call msg('o exc den mag wf','',ncell(1)*a_angs(1,:))
 call msg('o exc den mag wf','',ncell(2)*a_angs(2,:))
 call msg('o exc den mag wf','',ncell(3)*a_angs(3,:))
 call msg('o exc den mag wf','PRIMCOORD')
 if (.not.l_free_hole.and.l_exc_wf) then
   call msg('o exc','',(/n_atoms*ncell(1)*ncell(2)*ncell(3)+1,1/))
   !
   ! write Hole position
   ! 
   write(ch,'(i2,f10.5,f10.5,f10.5)') -1,r_hole*BOHR
   call msg('o exc den mag wf','',ch,INDENT=0,USE_TABS=.FALSE.)
 else
   call msg('o exc den mag wf','',(/n_atoms*ncell(1)*ncell(2)*ncell(3),1/))
 endif
 !
 ! write the translated atoms of the cell
 ! 
 do is=1,n_atomic_species
   do ia=1,n_atoms_species(is)
     do i1=0,ncell(1)-1
       do i2=0,ncell(2)-1
         do i3=0,ncell(3)-1
           rv(1)=atom_pos(1,ia,is)*BOHR+i1*a_angs(1,1)+i2*a_angs(2,1)+i3*a_angs(3,1)
           rv(2)=atom_pos(2,ia,is)*BOHR+i1*a_angs(1,2)+i2*a_angs(2,2)+i3*a_angs(3,2)
           rv(3)=atom_pos(3,ia,is)*BOHR+i1*a_angs(1,3)+i2*a_angs(2,3)+i3*a_angs(3,3)   
           write(ch,'(i5,3f10.5)') Z_species(is),rv(:)
           call msg('o exc den mag wf','',ch,INDENT=0,USE_TABS=.FALSE.)
         enddo
       enddo
     enddo
   enddo
 enddo
 !
 ! DIMENSIONs
 !
 select case(plot_dim)
   !
   case(2)
     if (use_2d(1)) nr2plot=(/nr(1),nr(2)/)
     if (use_2d(2)) nr2plot=(/nr(1),nr(3)/)
     if (use_2d(3)) nr2plot=(/nr(2),nr(3)/)
     if (use_2d(1)) dir2plot=(/1,2/)
     if (use_2d(2)) dir2plot=(/1,3/)
     if (use_2d(3)) dir2plot=(/2,3/)
     !
     call msg('o exc den mag wf','BEGIN_BLOCK_DATAGRID_2D')
     call msg('o exc den mag wf','Generated with YPP',code_version)
     call msg('o exc den mag wf','BEGIN_DATAGRID_2D')
     !
     ! here it depends on the plane
     ! number of data-points in each direction 
     !
     call msg('o exc den mag wf','',nr2plot) 
     !
     ! Origin of the datagrid
     !
     if (use_2d(1)) call msg('o exc den mag wf','',ncell(3)*a_angs(3,:)/2)
     if (use_2d(2)) call msg('o exc den mag wf','',ncell(2)*a_angs(2,:)/2) 
     if (use_2d(3)) call msg('o exc den mag wf','',ncell(1)*a_angs(1,:)/2)
     !
     ! First spanning vector of the datagrid
     !
     if (use_2d(1).or.use_2d(2)) call msg('o exc den mag wf','',ncell(1)*a_angs(1,:))
     if (use_2d(3)) call msg('o exc den mag wf','',ncell(2)*a_angs(2,:))
     !
     !second spanning vector of the datagrid
     !
     if (use_2d(1))call msg('o exc den mag wf','',ncell(2)*a_angs(2,:))
     if (use_2d(2).or.use_2d(3)) call msg('o exc den mag wf','',ncell(3)*a_angs(3,:))
     !
     do i2=1,nr2plot(2) 
       do i1=1,nr2plot(1)
         call msg('o exc den mag wf','',v2plot2D(i1,i2))
       enddo
     enddo  
     !
     call msg('o exc den mag wf','','END_DATAGRID_2D')
     call msg('o exc den mag wf','','END_BLOCK_DATAGRID_2D')
     !
     deallocate(v2plot2D)
     !
   case(3)
     !
     call msg('o exc den mag wf','BEGIN_BLOCK_DATAGRID_3D')
     call msg('o exc den mag wf','Generated with YPP',code_version)
     call msg('o exc den mag wf','BEGIN_DATAGRID_3D')
     !
     call msg('o exc den mag wf','',nr)
     call msg('o exc den mag wf','',(/0._SP,0._SP,0._SP/))
     call msg('o exc den mag wf','',ncell(1)*a_angs(1,:))
     call msg('o exc den mag wf','',ncell(2)*a_angs(2,:))
     call msg('o exc den mag wf','',ncell(3)*a_angs(3,:))
     !
     ir = 0
     max_=maxval(v2plot)
     if (.not.l_norm_to_one) max_=1.
     !
     if (len_trim(plot_title)==0) then
       call live_timing('3D Plot',nr(3),SERIAL=.true.)
     else
       call live_timing('3D Plot of '//trim(plot_title),nr(3),SERIAL=.true.)
     endif
     !
     do i3 = 0, nr(3)-1
       do i2 = 0, nr(2)-1
         do i1 = 0, nr(1)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           call msg('o exc den mag wf','',v2plot(ir)/max_*WF_multiplier)
         enddo
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
     call live_timing()
     !
     call msg('o exc den mag wf','','END_DATAGRID_3D')
     call msg('o exc den mag wf','','END_BLOCK_DATAGRID_3D')
     !
 end select
 !
end subroutine
