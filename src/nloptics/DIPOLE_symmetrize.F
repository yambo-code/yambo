!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!                  
subroutine DIPOLE_symmetrize(k)
 !
 use pars,           ONLY:SP,cI,cZERO
 use nl_optics,      ONLY:NL_bands
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:n_sp_pol
 use D_lattice,      ONLY:kpoints_map,old_dl_sop,old_S_contains_TR
 use DIPOLES,        ONLY:DIP_iR,DIP_S
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:REP,OP_RD_CL,DUMP
 use com,            ONLY:msg
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_Xk_ibz_index,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 !
#include<y_memory.h>
 !     
 type(bz_samp), intent(in)  :: k
 !
 ! Work space
 !
 integer, external :: io_full_SYMMs
 integer           :: io_old_SYMMs,ID
 integer           :: ik,ib1,ib2,i_sp,ik_old,is,ik_mem
 complex(SP),allocatable :: DIP_tmp(:,:,:,:,:)
 !     
 ! Load old full symmetries if present
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID)
 io_old_SYMMs=io_full_SYMMs(k,ID)
 !
 if(io_old_SYMMs/=0) return
 !
 YAMBO_ALLOC(DIP_tmp,(3,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),k%nibz,n_sp_pol))
 !
 ! I need dipoles in all cores
 !
 DIP_tmp=cZERO
 do ik=1,k%nibz
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   DIP_tmp(:,:,:,ik,:)=DIP_iR(:,:,:,ik_mem,:) 
 enddo
 call PP_redux_wait(DIP_tmp,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 ! Loop on k-points and application of symmetries to the dipole
 !
 call msg('rs','Dipoles symmetrized with full symmetries of the system') 
 !
 do ik=1,k%nibz
   ! 
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   ik_old=kpoints_map(1,ik)
   is=kpoints_map(2,ik)
   ! 
   if(is==1) continue
   !
   do i_sp=1,n_sp_pol
   do ib2=NL_bands(1),NL_bands(2)
   do ib1=NL_bands(1),NL_bands(2)
     DIP_iR(:,ib1,ib2,ik_mem,i_sp)=matmul(old_dl_sop(:,:,is),DIP_tmp(:,ib1,ib2,ik_old,i_sp)) 
   enddo
   enddo
   if(old_S_contains_TR(is)==1) DIP_iR(:,:,:,ik_mem,i_sp)=conjg(DIP_iR(:,:,:,ik_mem,i_sp))
   enddo
   !     
 enddo 
 !
 ! Overlaps symmetrization
 !
! do ik=1,k%nibz
!
!   ik_old=kpoints_map(1,ik)
!   is=kpoints_map(2,ik)
!   ! 
!   do i_sp=1,n_sp_pol
!   do ib2=1,NL_bands(2)
!   do ib1=1,NL_bands(2)
!!     DIP_S(ib1,ib2,1:3,ik,i_sp)=matmul(old_dl_sop(:,:,is),DIP_S(ib1,ib2,1:3,ik_old,i_sp))
!!     DIP_S(ib1,ib2,4:6,ik,i_sp)=matmul(old_dl_sop(:,:,is),DIP_S(ib1,ib2,4:6,ik_old,i_sp))
!   enddo
!   enddo
!!   if(old_S_contains_TR(is)==1) DIP_S(:,:,:,ik,i_sp)=conjg(DIP_S(:,:,:,ik,i_sp))
!   enddo
! enddo
 !
 YAMBO_FREE(kpoints_map)
 YAMBO_FREE(old_dl_sop)
 YAMBO_FREE(DIP_tmp)
 !  
end subroutine DIPOLE_symmetrize
