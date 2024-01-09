!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
subroutine INTERPOLATION_create_the_grid(GRID)
 !
 use interpolate,          ONLY:GRID_k
 use R_lattice,            ONLY:bz_samp_reset
 !
 implicit none
 !
 integer, intent(in) :: GRID(3)
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="INTERP"
 call INTERPOLATION_grid(GRID_k,'i',GRID)
 !
end subroutine INTERPOLATION_create_the_grid
!
subroutine INTERPOLATION_grid(K,units,grid_dim)
 !
 ! Generate an uniform k-point grid k[grid_dim(:)] in the BZ
 !
 use pars,           ONLY:SP,schlen
 use R_lattice,      ONLY:bz_samp
 use com,            ONLY:msg
 use vec_operate,    ONLY:c2a
 implicit none
 !
 integer,       intent(in)    :: grid_dim(3)
 type(bz_samp), intent(inout) :: K
 character(1),  intent(in)    :: units
 !
 ! Work Space
 !
 integer  :: i1,i2,i3,ic
 character(schlen) :: ch
 !
 if(any(grid_dim(:)<1)) call error("Wrong grid dimension")
 !
 write (ch,'(2a)') trim(K%description),"-ptbz"
 !
 ! Clean
 !
 if (allocated(K%ptbz)) then
   deallocate(K%ptbz)
 endif
 !
 K%nbz=product(grid_dim)
 allocate(K%ptbz(K%nbz,3))
 !
 ic=1
 do i1=1,grid_dim(1)
    do i2=1,grid_dim(2)
      do i3=1,grid_dim(3)
        K%ptbz(ic,1)=(i1-1._SP)/grid_dim(1)
        K%ptbz(ic,2)=(i2-1._SP)/grid_dim(2)
        K%ptbz(ic,3)=(i3-1._SP)/grid_dim(3)
        K%ptbz(ic,1:3)=K%ptbz(ic,1:3)-nint(K%ptbz(ic,1:3))     
        ic=ic+1
      enddo
    enddo
  enddo
  !
  do ic=1,K%nbz
    call c2a(v_in=K%ptbz(ic,:),mode="ka2i")
  enddo
  !
  K%units='i'
  !
  call k_reduce(K,.TRUE.)
  call msg('s','[GRID_INTERP] Points (IBZ)  ',K%nibz)
  call k_expand(K)
  call msg('s','                      (BZ)  ',K%nbz)
  call k_ibz2bz(K,'d',.TRUE.)
  call k_ibz2bz(K,'a',.TRUE.)
  !
  do ic=1,K%nbz
    call c2a(v_in=K%ptbz(ic,:),mode="ka2"//units)
  enddo
  !
end subroutine INTERPOLATION_grid
