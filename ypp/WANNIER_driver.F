! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine WANNIER_driver(k,E)
 !
 use pars,           ONLY:lchlen,SP
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,nkbz
 use electrons,      ONLY:levels
 use YPP,            ONLY:seed_name
 use com,            ONLY:error,msg
 use vec_operate,    ONLY:rlu_v_is_zero
 use zeros,          ONLY:k_rlu_zero
 use QP_m,           ONLY:QP_t,QP_reset
 use IO_m,           ONLY:io_control,OP_RD_CL,LOG,DUMP
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer              :: i1,nk_w,k_table(nkbz),ik,io_QP_and_GF_err,ID,i_qp
 integer,    external :: io_QP_and_GF
 real(SP)             :: k_w(3)
 type(QP_t)           :: qp
 character(lchlen)    :: nnkp_file,eig_file
 !
 call section("*","== Wannier 90 Interface ==")
 !
 call k_ibz2bz(k,'a',.true.)
 !
 nnkp_file=trim(seed_name)//".nnkp"
 open(unit=10,file=trim(nnkp_file),form='formatted')
 do i1=1,17
   read(10,*)
 enddo
 read(10,*) nk_w
 k_table=0
 if (nk_w/=nkbz) call error('Inconsitent number of k-points')
 do i1=1,nkbz
   read (10,*) k_w
   do ik=1,k%nbz
     if (rlu_v_is_zero(k_w(:)-k%ptbz(ik,:),zero_=k_rlu_zero)) k_table(i1)=ik
   enddo
 enddo
 close(10)
 !
 if (.not.any(k_table==0)) call msg('s','K-table built correctly')
 !
 call QP_reset(qp)
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2,3/),COM=LOG,MODE=DUMP,ID=ID)
 io_QP_and_GF_err=io_QP_and_GF("QP",qp,ID)
 !
 eig_file=trim(seed_name)//".eig"
 open(unit=10,file=trim(eig_file),form='formatted')
 do ik=1,k%nbz
   do i_qp=1,qp%n_states
     if  (qp%table(i_qp,3)==k%sstar(k_table(ik),1)) then
       write (10,*) qp%table(i_qp,1),ik,real(qp%E(i_qp))*HA2EV
     endif
   enddo
 enddo
 close(10)
 !
end subroutine
