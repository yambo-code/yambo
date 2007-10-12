!
! Copyright (C) 2000-2005 D. Varsano, A. Marini and the SELF team
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
subroutine exc_wf_xcrysden(use_2d,plot_dim)
 !
 use pars,        ONLY:BOHR,SP,schlen
 use com,         ONLY:of_open_close,msg,code_version
 use BS,          ONLY:BSS_description,BSS_n_descs
 use SPP,         ONLY:nr,nr_tot,exc_wf,p_dir,exc_wf,lambda,ncell,wf_2d
 use D_lattice,   ONLY:n_atom_species,n_atoms,a,atom_pos,znucl
 use stderr,      ONLY:intc
 !
 implicit none
 integer  :: plot_dim
 logical  :: use_2d(3)
 !
 ! Work Space...
 !
 integer  :: i1,i2,i3,ir,is,ia,n_atoms_tot,dir2plot(2),nr2plot(2)
 real(SP) :: a_angs(3,3)    !lattice vectors in Angstrom
 real(SP) :: rv(3),wf_max
 character(schlen) :: ch
 ! 
 ! Open the output file
 !
 call of_open_close('exc_wf_'//trim(intc(plot_dim))//'d_'//trim(intc(lambda))//'.xsf','ot')
 do i1=1,BSS_n_descs
   call msg('o exc',"#",trim(BSS_description(i1)),INDENT=0)
 enddo
 call msg('o exc',"#")
 !
 n_atoms_tot=0
 do is=1,n_atom_species
   n_atoms_tot=n_atoms_tot+n_atoms(is)
 enddo
 !
 !the atomic cell is given in a.u. and I want
 !to write the output of xcrysden in \AA
 !
 a_angs(:,:) = a(:,:)*BOHR
 !
 !This is the output file common at both grids
 !
 call msg('o exc','CRYSTAL')
 call msg('o exc','PRIMVEC')
 call msg('o exc','',ncell(1)*a_angs(1,:))
 call msg('o exc','',ncell(2)*a_angs(2,:))
 call msg('o exc','',ncell(3)*a_angs(3,:))
 call msg('o exc','PRIMCOORD')
 call msg('o exc','',(/n_atoms_tot*ncell(1)*ncell(2)*ncell(3),1/))
 !
 ! write the translated atoms of the cell
 ! 
 do is=1,n_atom_species
  do ia=1,n_atoms(is)
    do i1=0,ncell(1)-1
      do i2=0,ncell(2)-1
        do i3=0,ncell(3)-1
           rv(1)=atom_pos(1,ia,is)*BOHR+i1*a_angs(1,1)+i2*a_angs(2,1)+i3*a_angs(3,1)
           rv(2)=atom_pos(2,ia,is)*BOHR+i1*a_angs(1,2)+i2*a_angs(2,2)+i3*a_angs(3,2)
           rv(3)=atom_pos(3,ia,is)*BOHR+i1*a_angs(1,3)+i2*a_angs(2,3)+i3*a_angs(3,3)   
           write(ch,'(i,f,f,f)') znucl(is),rv(1),rv(2),rv(3)
           call msg('o exc','',ch,INDENT=0,USE_TABS=.FALSE.)
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
     call msg('o exc','BEGIN_BLOCK_DATAGRID_2D')
     call msg('o exc','Generated with SPP',code_version)
     call msg('o exc','BEGIN_DATAGRID_2D')
     !
     ! here it depends on the plane
     ! number of data-points in each direction 
     !
     call msg('o exc','',nr2plot) 
     !
     ! Origin of the datagrid
     !
     if (use_2d(1)) call msg('o exc','',ncell(3)*a_angs(3,:)/2)
     if (use_2d(2)) call msg('o exc','',ncell(2)*a_angs(2,:)/2) 
     if (use_2d(3)) call msg('o exc','',ncell(1)*a_angs(1,:)/2)
     !
     ! First spanning vector of the datagrid
     !
     if (use_2d(1).or.use_2d(2)) call msg('o exc','',ncell(1)*a_angs(1,:))
     if (use_2d(3)) call msg('o exc','',ncell(2)*a_angs(2,:))
     !
     !second spanning vector of the datagrid
     !
     if (use_2d(1))call msg('o exc','',ncell(2)*a_angs(2,:))
     if (use_2d(2).or.use_2d(3)) call msg('o exc','',ncell(3)*a_angs(3,:))
     !
     do i2=1,nr2plot(2) 
       do i1=1,nr2plot(1)
         call msg('o exc','',wf_2d(i1,i2))
       enddo
     enddo  
     !
     call msg('o exc','','END_DATAGRID_2D')
     call msg('o exc','','END_BLOCK_DATAGRID_2D')
     !
     deallocate(wf_2d)
     !
   case(3)
     !
     call msg('o exc','BEGIN_BLOCK_DATAGRID_3D')
     call msg('o exc','Generated with SPP',code_version)
     call msg('o exc','BEGIN_DATAGRID_3D')
     !
     call msg('o exc','',nr)
     call msg('o exc','',(/0._SP,0._SP,0._SP/))
     call msg('o exc','',ncell(1)*a_angs(1,:))
     call msg('o exc','',ncell(2)*a_angs(2,:))
     call msg('o exc','',ncell(3)*a_angs(3,:))
     !
     ir = 0
     forall(ir=1:nr_tot) exc_wf(ir)=exc_wf(ir)*conjg(exc_wf(ir))
     wf_max=maxval(real(exc_wf))
     do i3 = 0, nr(3)-1
       do i2 = 0, nr(2)-1
         do i1 = 0, nr(1)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           call msg('o exc','',real(exc_wf(ir))/wf_max)
         enddo
       enddo
     enddo
     !
     call msg('o exc','','END_DATAGRID_3D')
     call msg('o exc','','END_BLOCK_DATAGRID_3D')
     !
 end select
 !
 call of_open_close('exc_wf_'//trim(intc(plot_dim))//'d_'//trim(intc(lambda))//'.xsf')
 !
end subroutine
