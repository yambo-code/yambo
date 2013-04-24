!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine grid_generator(USER_k,grid_dim)
 !
 ! Generate an uniform k-point grid k[grid_dim(:)] in the BZ
 !
 use pars,           ONLY:SP,schlen
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:error,msg
 use vec_operate,    ONLY:c2a
 use memory_m,       ONLY:mem_est
 implicit none
 !
 integer, intent(in)        :: grid_dim(3)
 type(bz_samp), intent(out) :: USER_k
 !
 ! Work Space
 !
 integer  :: i1,i2,i3,ic
 real(SP) :: shift(3)
 character(schlen) :: ch
 !
 if(any(grid_dim(:)<1)) call error("Wrong grid dimension")
 !
 write (ch,'(2a)') trim(USER_k%description),"-ptbz"
 !
 ! Clean
 !
 if (associated(USER_k%ptbz)) then
   deallocate(USER_k%ptbz)
   call mem_est(trim(ch))
 endif
 !
 USER_k%nbz=product(grid_dim)
 allocate(USER_k%ptbz(USER_k%nbz,3))
 call mem_est(trim(ch),(/USER_k%nbz*3/),(/SP/))
 !
 shift=0._SP
 do i1=1,3
  if(mod(grid_dim(i1),2)==0) shift(i1)=0.5_SP
 enddo
 !
 ic=1
 do i1=1,grid_dim(1)
    do i2=1,grid_dim(2)
      do i3=1,grid_dim(3)
        USER_k%ptbz(ic,1)=(i1-1._SP+shift(1))/grid_dim(1)
        USER_k%ptbz(ic,2)=(i2-1._SP+shift(2))/grid_dim(2)
        USER_k%ptbz(ic,3)=(i3-1._SP+shift(3))/grid_dim(3)
        USER_k%ptbz(ic,1:3)=USER_k%ptbz(ic,1:3)-nint(USER_k%ptbz(ic,1:3))     
        ic=ic+1
      enddo
    enddo
  enddo
  !
  do ic=1,USER_k%nbz
    call c2a(v_in=USER_k%ptbz(ic,:),mode="ka2i")
  enddo
  !
  USER_k%units='i'
  !
  Call k_reduce(USER_k)
  call msg('s',' Interpolation nibz : ',USER_k%nibz)
  call k_expand(USER_k)
  call msg('s',' Interpolation nbz  : ',USER_k%nbz)
  call k_ibz2bz(USER_k,'d',.TRUE.)
  call k_ibz2bz(USER_k,'i',.TRUE.)
  !
end subroutine grid_generator
