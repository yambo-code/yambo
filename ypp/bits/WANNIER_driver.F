! 
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AR
!
subroutine WANNIER_driver(k,E)
 !
 ! Latest update: September 14th, 2016 by Antimo Marrazzo (antimo.marrazzo@epfl.ch)
 !
 ! NB: QP corrections (and not QP eigenvalues) are written in the Wannier90 format.
 !
 use pars,           ONLY:lchlen,SP
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use YPPm,           ONLY:seed_name
 use com,            ONLY:msg
 use vec_operate,    ONLY:rlu_v_is_zero
 use zeros,          ONLY:k_rlu_zero
 use QP_m,           ONLY:QP_t,QP_reset
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,LOG,DUMP
#include<memory.h>
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer              :: i1,nk_w,ik,io_QP_and_GF_err,ID,i_qp
 integer              :: a,b
 integer, allocatable :: k_table(:)
 integer,    external :: io_QP_and_GF
 real(SP)             :: k_w(3)
 type(QP_t)           :: qp
 character(lchlen)    :: nnkp_file
 logical              :: found
 !
 call section("*","== Wannier 90 Interface ==")
 !
 call k_ibz2bz(k,'a',.true.)
 !
 nnkp_file=trim(seed_name)//".nnkp"
 open(unit=10,file=trim(nnkp_file),form='formatted')
 call scan_file_to('kpoints',found,10)
 read(10,*) nk_w
 YAMBO_ALLOC(k_table,(nk_w))
 k_table=0
 !if (nk_w/=nkbz) call error('Inconsitent number of k-points')
 do i1=1,nk_w
   read (10,*) k_w
   do ik=1,k%nbz
     if (rlu_v_is_zero(k_w(:)-k%ptbz(ik,:),zero_=k_rlu_zero)) k_table(i1)=ik
   enddo
 enddo
 !
 close(10)
 !
 if (.not.any(k_table==0)) call msg('s','K-table built correctly')
 !
 call QP_reset(qp)
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=LOG,MODE=DUMP,ID=ID)
 io_QP_and_GF_err=io_QP_and_GF("QP",qp,ID)

 call msg('s','QP db read, writing QP corrections in the .eig Wannier90 format')
 open(unit=10,file=trim(seed_name)//'.gw.unsorted'//'.eig',form='formatted')
 do ik=1,nk_w
   do i_qp=1,qp%n_states
      if(qp%table(i_qp,3)==k%sstar(k_table(ik),1)) then
      write(10,*) (qp%table(i_qp,1)-qp%table(1,1)+1),ik,real(qp%E(i_qp)-qp%E_bare(i_qp))*HA2EV
      end if
   enddo
 enddo
 close(10)
 !
 call msg('s','gw.unsorted.eig written')
 !
 contains
 !
 subroutine scan_file_to(keyword,found,iun_nnkp)
   !
   !-----------------------------------------------------------------------
   !---Subroutine from Wannier90, GPL license------------------------------
   !-----------------------------------------------------------------------
   !
#include<memory.h>
   !
   integer:: iun_nnkp
   character(len=*), intent(in)  :: keyword
   logical,          intent(out) :: found
   !
   ! Workspace
   !
   character(len=80) :: line1, line2
   !
   ! by uncommenting the following line the file scan restarts every time
   ! from the beginning thus making the reading independent on the order
   ! of data-blocks
   !
   !rewind (iun_nnkp)
   !
10 continue
   read(iun_nnkp,*,end=20) line1, line2
   if(line1/='begin') goto 10
   if(line2/=keyword) goto 10
   found=.true.
   return
20 found=.false.
   rewind (iun_nnkp)
   !
 end subroutine scan_file_to
 !
end subroutine WANNIER_driver
