!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine plot_cube()
 !
 use pars,        ONLY:SP,schlen
 use FFT_m,       ONLY:fft_dim 
 use LOGO,        ONLY:code_version
 use com,         ONLY:msg
 use YPP,         ONLY:nr,v2plot,ncell,r_hole,l_free_hole,&
&                      l_norm_to_one,WF_multiplier,l_exc_wf,plot_title,output_string
 use D_lattice,   ONLY:n_atomic_species,n_atoms_species,a,atom_pos,Z_species,n_atoms
 use LIVE_t,      ONLY:live_timing
 !
 implicit none
 !
 ! Work Space...
 !
 integer  :: i1,i2,i3,ir,is,ia
 real(SP) :: rv(3),max_
 character(schlen) :: ch
 !
 call msg(output_string,'CUBE FILE')
 call msg(output_string,'Generated with YPP',code_version)
 if (.not.l_free_hole.and.l_exc_wf) then
   write(ch,'(i5,3f10.5)') n_atoms*ncell(1)*ncell(2)*ncell(3)+1,0._SP,0._SP,0._SP
 else 
  write(ch,'(i5,3f10.5)') n_atoms*ncell(1)*ncell(2)*ncell(3),0._SP,0._SP,0._SP
 endif
 call msg(output_string,'',ch) 
 do i1=1,3
   write(ch,'(i5,3f10.5)') nr(i1),a(i1,:)/fft_dim(i1)
   call msg(output_string,'',ch) 
 enddo
 !
 ! write the atoms position 
 !
 if (.not.l_free_hole.and.l_exc_wf) then
   write(ch,'(2i5,3f10.5)') -1,-1,r_hole
   call msg(output_string,'',ch,INDENT=0,USE_TABS=.FALSE.)
 endif 

 do is=1,n_atomic_species
   do ia=1,n_atoms_species(is)
     do i1=0,ncell(1)-1
       do i2=0,ncell(2)-1
         do i3=0,ncell(3)-1
           rv(1)=atom_pos(1,ia,is)+i1*a(1,1)+i2*a(2,1)+i3*a(3,1)
           rv(2)=atom_pos(2,ia,is)+i1*a(1,2)+i2*a(2,2)+i3*a(3,2)
           rv(3)=atom_pos(3,ia,is)+i1*a(1,3)+i2*a(2,3)+i3*a(3,3)   
           write(ch,'(2i5,3f10.5)') Z_species(is), Z_species(is),rv(:)
           call msg(output_string,'',ch,INDENT=0,USE_TABS=.FALSE.)
         enddo
       enddo
     enddo
   enddo
 enddo
 !
 ir = 0
 max_=maxval(v2plot)
 if (.not.l_norm_to_one) max_=1.
 !
 if (len_trim(plot_title)==0) then
   call live_timing('3D Plot',nr(1))
 else
   call live_timing('3D Plot of '//trim(plot_title),nr(1))
 endif
 !
 do i1 = 0, nr(1)-1
   do i2 = 0, nr(2)-1
     do i3 = 0, nr(3)-1
       ir = 1 + i1 + i2*nr(1) + i3*nr(1)*nr(2)
         call msg(output_string,'',v2plot(ir)/max_*WF_multiplier)
     enddo
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
   !
 call live_timing()
 !
end subroutine
