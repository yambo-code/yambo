!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!                  
subroutine DIPOLE_symmetrize(k)
 !
 use pars,           ONLY:SP,cI
 use nl_optics,      ONLY:NL_bands
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:n_sp_pol
 use D_lattice,      ONLY:kpoints_map,old_dl_sop
 use DIPOLES,        ONLY:DIP_iR
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:REP,OP_RD_CL,DUMP
 use com,            ONLY:msg
 !
#include<y_memory.h>
 !     
 type(bz_samp), intent(in)  :: k
 !
 ! Work space
 !
 integer, external     :: io_full_SYMMs
 integer               :: io_old_SYMMs,ID
 integer               :: ik,ib1,ib2,i_sp,ik_old,is
 !     
 ! Load old full symmetries if present
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID)
 io_old_SYMMs=io_full_SYMMs(k,ID)
 !
 if(io_old_SYMMs/=0) return
 !
 ! Loop on k-points and application of symmetries to the dipole
 ! For the moment this subroutine does not work with k-parallelization!!!!
 !
 call msg('rs','Dipoles symmetrized with full symmetries of the system') 
 !
 do ik=1,k%nibz
   ! 
   ik_old=kpoints_map(1,ik)
   is=kpoints_map(2,ik)
   ! 
   if(is==1) continue
   !
   do i_sp=1,n_sp_pol
   do ib1=1,NL_bands(2)
   do ib2=1,NL_bands(2)
     DIP_iR(:,ib1,ib2,ik,i_sp)=matmul(old_dl_sop(:,:,is),DIP_iR(:,ib1,ib2,ik_old,i_sp)) 
   enddo
   enddo
   enddo
   !     
 enddo 
 !
 YAMBO_FREE(kpoints_map)
 YAMBO_FREE(old_dl_sop)
 !  
end subroutine DIPOLE_symmetrize
