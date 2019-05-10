!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine plot_xcrysden()
 !
 use pars,        ONLY:SP,schlen
 use units,       ONLY:BO2ANG
 use C_driver,    ONLY:code_version
 use com,         ONLY:msg
 use YPPm,        ONLY:nr,v2plot,ncell,v2plot2D,r_hole,plot_dim,l_free_hole,&
&                      l_norm_to_one,WF_multiplier,l_exc_wf,plot_title,output_string,plot_is_2D
 use D_lattice,   ONLY:n_atomic_species,n_atoms_species,a,atom_pos,Z_species,n_atoms
 use LIVE_t,      ONLY:live_timing
 !
#include<memory.h>
 !
 ! Work Space...
 !
 integer  :: i1,i2,i3,i4,i5,ir,is,ia,dir2plot(2),nr2plot(2)
 real(SP) :: a_angs(3,3)    !lattice vectors in Angstrom
 real(SP) :: rv(3),max_
 character(schlen) :: ch
 real(SP), allocatable :: v2plot_xc(:,:,:)
 ! 
 !the atomic cell is given in a.u. and I want
 !to write the output of xcrysden in \AA
 !
 a_angs(:,:) = a(:,:)*BO2ANG
 !
 !This is the output file common at both grids
 !
 call msg(output_string,'CRYSTAL')
 call msg(output_string,'PRIMVEC')
 call msg(output_string,'',ncell(1)*a_angs(1,:))
 call msg(output_string,'',ncell(2)*a_angs(2,:))
 call msg(output_string,'',ncell(3)*a_angs(3,:))
 call msg(output_string,'PRIMCOORD')
 if (.not.l_free_hole.and.l_exc_wf) then
   call msg('o exc','',(/n_atoms*ncell(1)*ncell(2)*ncell(3)+1,1/))
   !
   ! write Hole position
   ! 
   write(ch,'(i2,f10.5,f10.5,f10.5)') -1,r_hole*BO2ANG
   call msg(output_string,'',ch,INDENT=0,USE_TABS=.FALSE.)
 else
   call msg(output_string,'',(/n_atoms*ncell(1)*ncell(2)*ncell(3),1/))
 endif
 !
 ! write the translated atoms of the cell
 ! 
 do is=1,n_atomic_species
   do ia=1,n_atoms_species(is)
     do i1=0,ncell(1)-1
       do i2=0,ncell(2)-1
         do i3=0,ncell(3)-1
           rv(1)=atom_pos(1,ia,is)*BO2ANG+i1*a_angs(1,1)+i2*a_angs(2,1)+i3*a_angs(3,1)
           rv(2)=atom_pos(2,ia,is)*BO2ANG+i1*a_angs(1,2)+i2*a_angs(2,2)+i3*a_angs(3,2)
           rv(3)=atom_pos(3,ia,is)*BO2ANG+i1*a_angs(1,3)+i2*a_angs(2,3)+i3*a_angs(3,3)   
           write(ch,'(i5,3f10.5)') Z_species(is),rv(:)
           call msg(output_string,'',ch,INDENT=0,USE_TABS=.FALSE.)
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
     if (plot_is_2D(1)) nr2plot=(/nr(1)+1,nr(2)+1/)
     if (plot_is_2D(2)) nr2plot=(/nr(1)+1,nr(3)+1/)
     if (plot_is_2D(3)) nr2plot=(/nr(2)+1,nr(3)+1/)
     if (plot_is_2D(1)) dir2plot=(/1,2/)
     if (plot_is_2D(2)) dir2plot=(/1,3/)
     if (plot_is_2D(3)) dir2plot=(/2,3/)
     !
     call msg(output_string,'BEGIN_BLOCK_DATAGRID_2D')
     call msg(output_string,'Generated with YPP',code_version)
     call msg(output_string,'BEGIN_DATAGRID_2D')
     !
     ! here it depends on the plane
     ! number of data-points in each direction 
     !
     call msg(output_string,'',nr2plot) 
     !
     ! Origin of the datagrid
     !
     if (plot_is_2D(1)) call msg(output_string,'',ncell(3)*a_angs(3,:)/2)
     if (plot_is_2D(2)) call msg(output_string,'',ncell(2)*a_angs(2,:)/2) 
     if (plot_is_2D(3)) call msg(output_string,'',ncell(1)*a_angs(1,:)/2)
     !
     ! First spanning vector of the datagrid
     !
     if (plot_is_2D(1).or.plot_is_2D(2)) call msg(output_string,'',ncell(1)*a_angs(1,:))
     if (plot_is_2D(3)) call msg(output_string,'',ncell(2)*a_angs(2,:))
     !
     !second spanning vector of the datagrid
     !
     if (plot_is_2D(1))call msg(output_string,'',ncell(2)*a_angs(2,:))
     if (plot_is_2D(2).or.plot_is_2D(3)) call msg(output_string,'',ncell(3)*a_angs(3,:))
     !
     do i2=1,nr2plot(2) 
       do i1=1,nr2plot(1)
         call msg(output_string,'',v2plot2D(i1,i2))
       enddo
     enddo
     !
     call msg(output_string,'','END_DATAGRID_2D')
     call msg(output_string,'','END_BLOCK_DATAGRID_2D')
     !
     YAMBO_FREE(v2plot2D)
     !
   case(3)
     !
     call msg(output_string,'BEGIN_BLOCK_DATAGRID_3D')
     call msg(output_string,'Generated with YPP',code_version)
     call msg(output_string,'BEGIN_DATAGRID_3D')
     !
     call msg(output_string,'',(/nr(1)+1,nr(2)+1,nr(3)+1/))
     call msg(output_string,'',(/0._SP,0._SP,0._SP/))
     call msg(output_string,'',ncell(1)*a_angs(1,:))
     call msg(output_string,'',ncell(2)*a_angs(2,:))
     call msg(output_string,'',ncell(3)*a_angs(3,:))
     !
     ir = 0
     max_=maxval(v2plot)
     if (.not.l_norm_to_one) max_=1._SP
     !
     if (len_trim(plot_title)==0) then
       call live_timing('3D Plot',nr(3))
     else
       call live_timing('3D Plot of '//trim(plot_title),nr(3))
     endif
     !
     YAMBO_ALLOC(v2plot_xc,(nr(1)+1,nr(2)+1,nr(3)+1))
     !
     do i3 = 0, nr(3)-1
       do i2 = 0, nr(2)-1
         do i1 = 0, nr(1)-1
           ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
           v2plot_xc(i1+1,i2+1,i3+1)=v2plot(ir)/max_*WF_multiplier
           call msg(output_string,'',v2plot_xc(i1+1,i2+1,i3+1))
           if(i1==nr(1)-1) call msg(output_string,'',v2plot_xc(1,i2+1,i3+1))
           if(i1==nr(1)-1.and.i2==nr(2)-1) then
             do i4=1,nr(1)
               call msg(output_string,'',v2plot_xc(i4,1,i3+1))
             enddo
             call msg(output_string,'',v2plot_xc(1,1,i3+1))
           endif
           if(i1==nr(1)-1.and.i2==nr(2)-1.and.i3==nr(3)-1) then 
             do i5=1,nr(2)
               do i4=1,nr(1)
                 call msg(output_string,'',v2plot_xc(i4,i5,1))
               enddo
               call msg(output_string,'',v2plot_xc(1,i5,1))
             enddo
             do i4=1,nr(1)
               call msg(output_string,'',v2plot_xc(i4,1,1))
             enddo
             call msg(output_string,'',v2plot_xc(1,1,1))
           endif
         enddo
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
     call live_timing()
     YAMBO_FREE(v2plot_xc)
     !
     call msg(output_string,'','END_DATAGRID_3D')
     call msg(output_string,'','END_BLOCK_DATAGRID_3D')
     !
 end select
 !
end subroutine
