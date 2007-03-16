!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
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
 integer function elph_pw_dbs(k,E,q)
 !
 use pars,                ONLY:DP
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use ELPH,                ONLY:ph_freqs_sq,ph_modes,elph_nb,elph_use_q_grid,&
&                              elph_gkkp_free,max_ph_freq,elph_gkkp_alloc,&
&                              elph_nq,ph_qpt
 use com,                 ONLY:msg,warning
 use R_lattice,           ONLY:bz_samp
 use D_lattice,           ONLY:nsym
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,&
&                              OP_RD_CL,VERIFY,dbs_fragment
 use timing,              ONLY:live_timing
 implicit none
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 !Work Space
 !
 integer,allocatable::ikbz_is_table(:,:)
 integer            ::iq,idb,io_err,io_elph_err,ID,elph_nk,iq_db(q%nbz)
 integer, external  ::ioELPH 
 logical            ::dbs_fragment_save
 complex(DP), allocatable :: grad_at_gamma(:,:,:,:,:)
 !
 call section("*","== Electron-Phonon Interface: Databases ==")
 !
 call k_ibz2bz(k,'i',.false.)
 !
 call msg('s','ELPH databases ...')
 !
 io_err=0
 max_ph_freq=0.
 elph_use_q_grid=.true.
 elph_nq=0
 iq_db=0
 elph_pw_dbs=0
 !
 do while (.TRUE.)
   elph_nq=elph_nq+1
   io_err=elph_pw_dbs_io(-elph_nq)
   if (io_err==-1) exit
   if (io_err==-2) then
     call msg("l","incorrect K-point correspondance")
     return
   endif
   if (io_err>0) iq_db(elph_nq)=io_err
 enddo
 elph_nq=elph_nq-1
 if (elph_nq==0) then
   call msg("l","not found")
   return
 endif
 if (elph_use_q_grid) then
   call msg("l",'found '//trim(intc(elph_nq))//' Q-grid compatible')
 else
   call msg("l",'found '//trim(intc(elph_nq))//' NOT Q-grid compatible')
 endif
 !
 ! KBZ + SYM table
 !
 allocate(ikbz_is_table(k%nbz,nsym))
 call k_syms_tab(ikbz_is_table)
 !
 ! Check if DB is already done
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_elph_err=ioELPH(ID)
 if (io_elph_err==0) return
 !
 ! Allocation
 !
 call elph_gkkp_alloc()
 !
 ! Force DB fragmentation
 !
 dbs_fragment_save=dbs_fragment
 dbs_fragment=.TRUE.
 !
 call live_timing('ELPH databases (WRITE)',elph_nq)
 !
 do idb=1,elph_nq
   !
   iq=idb
   if (elph_use_q_grid) iq=iq_db(idb)
   !
   io_err=elph_pw_dbs_io(idb)
   !
   if (io_err==-1) then
     call live_timing(steps=1)
     cycle
   endif
   !
   if (io_err==-4) call warning("K-grid is not fully q-connected")
   !
   if (idb==1) then
     call io_control(ACTION=OP_APP_WR_CL,SEC=(/1,iq+1/),ID=ID)
   else
     call io_control(ACTION=OP_APP_WR_CL,SEC=(/iq+1/),ID=ID)
   endif
   io_elph_err=ioELPH(ID)
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 call msg("s",':: Modes           :',ph_modes)
 call msg("s",':: Bands range     :',elph_nb)
 !
 call elph_gkkp_free()
 dbs_fragment=dbs_fragment_save
 !
 contains
   !
   integer function elph_pw_dbs_io(idb_)
   !------------------------------------
   !
   ! s.dbph_ structure
   !
   !   read (99) ph_modes,elph_nk,elph_nb
   !   read (99) pw_alat,pw_q,pw_k
   !   read (99) ph_freqs_sq
   !   DO ik=1,nksq
   !     read (99) elph_gkkp_disk
   !     read (99) polarization_vec
   !     read (99) grad_at_gamma (only at Gamma)
   !     read (99) Ek
   !     read (99) Ek_plus_q
   !   ENDDO
   !
   use pars,           ONLY:SP,DP,schlen
   use D_lattice,      ONLY:nsym,sop_inv,alat,n_atoms
   use R_lattice,      ONLY:nqibz
   use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,&
&                           io_netcdf_support,io_disconnect,io_file
   use ELPH,           ONLY:ph_modes,elph_nb,E_k_plus_q,&
&                           ph_freqs_sq,elph_gkkp,elph_DW
   use SPP,            ONLY:elph_dbs_path
   use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
   use zeros,          ONLY:k_iku_zero,k_rlu_zero
   use electrons,      ONLY:n_bands
   implicit none
   integer           :: idb_
   !
   !Work Space
   !
   integer           :: ib1,ib2,ig,il,nat,ia,i,j
   real(SP)          :: r_v(3),ph_freq
   complex(DP)       :: F1(3)
   !
   !I/O
   !
   integer           :: ID,io_err
   character(schlen) :: db_name
   real(SP)          :: dVRY2Ha
   real(DP)          :: pw_k(3,k%nbz),pw_alat,pw_q(3),Ek(n_bands),&
&                       Ek_p_q(n_bands),pw_E_k_shift
   real(DP),    allocatable :: ph_freqs_sq_disk(:)
   complex(DP), allocatable :: gkkp_disk(:,:,:),pol_vec(:,:,:)
   !
   ! K Table
   !
   integer           :: i1,ik,k_found(k%nbz),k2k_tab(k%nbz),ikbz_rot,iq_check
   integer           :: nsmall,small(nsym)
   !
   elph_pw_dbs_io=-1
   !
   call io_control(ACTION=OP_RD,ID=ID)
   !
   io_netcdf_support(ID)=.FALSE.
   !
   write (db_name,'(a,i6.6)') 's.dbph_',iabs(idb_)
   io_err=io_connect(desc=trim(db_name),subfolder=trim(elph_dbs_path),type=-1,ID=ID)
   !
   elph_pw_dbs_io=io_err
   if (io_err/=0) return  
   !
   ! HEADER 
   !
   read (io_unit(ID)) ph_modes,elph_nk,elph_nb
   read (io_unit(ID)) pw_alat,pw_q,pw_k(:,:elph_nk)
   !
   if (any(alat/=pw_alat)) then
     forall (i1=1:3) pw_q(i1)=pw_q(i1)*alat(i1)/pw_alat
     forall (i1=1:3) pw_k(i1,:)=pw_k(i1,:)*alat(i1)/pw_alat
   endif
   !
   if (idb_<0) then
     iq_check=0
     do i1=1,nqibz
       if (v_is_zero(real(pw_q(:))+q%pt(i1,:),zero_=k_iku_zero)) iq_check=i1
     enddo
     if (iq_check==0) elph_use_q_grid=.false.
     elph_pw_dbs_io=iq_check
   else
     ph_qpt(iq,:)=pw_q
   endif
   !
   ! Espresso <-> SELF k table correspondance
   !
   k2k_tab=0
   k_found=0
   do i1=1,elph_nk
     !
     ! For non zero phonon q PW grid is composed of (k_1,k_1+q,k_2,k_2+q ...).
     ! I table the  k1,k2 ...
     !
     do ik=1,k%nbz
       if (k_found(ik)==1) cycle
       call c2a(v_in=k%ptbz(ik,:)-real(pw_k(:,i1),SP),v_out=r_v,mode="ki2a")
       if (rlu_v_is_zero(r_v,zero_=k_rlu_zero)) then
         k2k_tab(i1)=ik
         k_found(ik)=1
         exit
       endif
     enddo
     if (k2k_tab(i1)==0) elph_pw_dbs_io=-2
   enddo
   !
   ! Phonon Frequencies
   !
   allocate(ph_freqs_sq_disk(ph_modes))
   !
   ! PW energies are in Rydbergs. Here we have the phonon frequencies square
   !
   read (io_unit(ID)) ph_freqs_sq_disk
   !
   do il=1,ph_modes
     !
     ph_freq=sqrt( max( real(ph_freqs_sq_disk(il)),0._SP))/2._SP
     max_ph_freq=max(max_ph_freq,ph_freq)
     !
   enddo
   if (idb_<0) then
     deallocate(ph_freqs_sq_disk)
     goto 1
   endif
   ph_freqs_sq(iq,:)=abs(ph_freqs_sq_disk)/4._DP
   !
   elph_gkkp=(0._SP,0._SP)
   elph_DW  =(0._SP,0._SP)
   ! 
   ! ELPH_gkkp 
   !
   nat=sum(n_atoms)
   !
   allocate(gkkp_disk(elph_nb,elph_nb,ph_modes),pol_vec(ph_modes,nat,3))
   if (idb_==1) allocate(grad_at_gamma(k%nbz,elph_nb,elph_nb,nat,3))
   !
   ! Reading
   !
   do ik=1,elph_nk
     !
     ! Let's remember it again:
     !                                ib1                             ib2
     !                                |                               | 
     ! el_ph_mat(i,j,k,I)= <\psi(k+q) n_i|dV_{SCF}/du^q_{i a}|\psi(k) n_j>
     !                           |                          
     !                           ik[PW]/k2k_tab(ik)[SELF]  
     !
     ! I = (i,a)
     !
     ! In PW we define
     !
     !  gkkp_disk(i,j,k,l) = el_ph_mat(i,j,k,I) u(I,l)^* eps_I(q l)/sqrt(M_a)
     !
     ! However SELF table describe the k->k-q transitions and not k+q. So we 
     ! define
     !
     ! g_jik^{qI}|_SELF= g_jik^{-qI}|_PW = <k-q n_i|dV_{SCF}/du^-q_{i a}|k n_j>
     !
     ! where k = k2k_tab(ik). Note that this procedure implies that SELF {q}'s
     ! are -{q}'s in PW (note the MinusQ flag).
     !
     read (io_unit(ID)) gkkp_disk
     read (io_unit(ID)) pol_vec
     if (idb==1) read (io_unit(ID)) grad_at_gamma(k2k_tab(ik),:,:,:,:)
     read (io_unit(ID)) Ek(:elph_nb)
     read (io_unit(ID)) Ek_p_q(:elph_nb)
     !
     ! To allign correctly the E(k+q) energies I use the shift between
     ! the ABSOLUTE SELF energy levels at k and the PW ones.
     !
     pw_E_k_shift=real(Ek(1)/2._DP)-(E%E(1,k%sstar(k2k_tab(ik),1),1)+E%Efermi(1))
     !
     E_k_plus_q(:,k2k_tab(ik),1)=real(Ek_p_q(:elph_nb)/2._DP)-pw_E_k_shift
     !
     do ib1=1,elph_nb
       do ib2=1,elph_nb
         !
         ! PW energies are in Rydbergs.
         !
         !   Here we have <dV/dr>. Now [<V>]=[E] but [<r>]=[E^{-1/2}] so
         !
         !   [<dV/dr>] = [E^{3/2}]   
         !
         dVRY2Ha=2._DP**(-3._DP/2._DP)
         !
         elph_gkkp(k2k_tab(ik),:,ib1,ib2)=gkkp_disk(ib1,ib2,:)*dVRY2Ha
         !
         ! Debye Waller Term
         !
         F1=(0._DP,0._DP)
         do ia=1,nat
           F1(:)=F1(:)+grad_at_gamma(k2k_tab(ik),ib1,ib2,ia,:)*dVRY2Ha
         enddo
         !
         do i=1,3
           do j=1,3
             do ia=1,nat
               do il=1,ph_modes
                 elph_DW(k2k_tab(ik),il,ib1,ib2)=elph_DW(k2k_tab(ik),il,ib1,ib2)+&
&                2.*real(&
&                  F1(i)*grad_at_gamma(k2k_tab(ik),ib2,ib1,ia,j)*pol_vec(il,ia,i)*&
&                  conjg(pol_vec(il,ia,j))*dVRY2Ha&
&                )
               enddo
             enddo
           enddo
         enddo
         !
       enddo 
       !
     enddo
   enddo
   !
   ! The small group of q
   ! 
   call q_small(real(pw_q,SP),small,nsmall)
   !
   ! The pw_k K-grid is reduced using the small group of q. 
   ! I need to expand the K-grid to define the missing elements of elph_dV
   !
   do ik=1,k%nbz
     if (k_found(ik)==0) cycle
     do i1=1,nsmall
       ikbz_rot=ikbz_is_table(ik,sop_inv(small(i1)))
       if (k_found(ikbz_rot)/=0) cycle
       elph_gkkp(ikbz_rot,:,:,:)=elph_gkkp(ik,:,:,:)
       elph_DW(ikbz_rot,:,:,:)=elph_DW(ik,:,:,:)
       E_k_plus_q(:,ikbz_rot,1)=E_k_plus_q(:,ik,1)
       k_found(ikbz_rot)=-1
     enddo
   enddo
   !
   if (any((/k_found==0/))) elph_pw_dbs_io=-4
   !
   ! CLEAN
   !
   deallocate(ph_freqs_sq_disk,gkkp_disk,pol_vec)
   if (idb==elph_nq) deallocate(grad_at_gamma)
 
1  call io_control(ACTION=RD_CL,ID=ID)
   call io_disconnect(ID)
 
   end function
   !
   subroutine q_small(pw_q,small,nsmall)
   !------------------------------------
   !
   use pars,           ONLY:SP
   use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
   use D_lattice,      ONLY:nsym
   use R_lattice,      ONLY:rl_sop,nkbz
   use zeros,          ONLY:k_rlu_zero
   !
   integer  :: nsmall,small(nsym)
   real(SP) :: pw_q(3),r_v(3)
   integer  :: is
   small=0
   nsmall=0
   do is=1,nsym
     r_v=matmul(rl_sop(:,:,is),pw_q) -pw_q
     call k2bz(r_v)
     call c2a (v_in=r_v,mode='ki2a')
     if (rlu_v_is_zero(r_v,zero_=k_rlu_zero)) then
       nsmall=nsmall+1
       small(nsmall)=is
     endif
   enddo
   end subroutine
   !
   subroutine k_syms_tab(ikbz_is_table)
   !-----------------------------------
   !
   use pars,           ONLY:SP
   use vec_operate,    ONLY:c2a
   use D_lattice,      ONLY:nsym,sop_tab
   use memory_m,       ONLY:mem_est
   use R_lattice,      ONLY:rl_sop,nkbz,ik_is_table
   implicit none
   integer :: ikbz_is_table(nkbz,nsym)
   integer :: i1,is,ir,ikibz,i2
   real(SP):: r_v(3)
   ikbz_is_table=0
   !
   ! First I find the action of all the syms on the IBZ kpts
   !
   ! R_is k_ibz = k_{ik_is_table(k_ibz,is)}
   !
   ! where ik_is_table(k_ibz,is) is a BZ index
   !
   call k_sym2sym(k,'k')
   call k_ibz2bz(k,'i',.false.) ! in k_sym2sym there is the k_ibz2bz(k,'d',...) call
   !
   ! To extend ik_is_table to the BZ I simply have that
   !
   ! R_is k_bz = R_is S k_ibz = ik_is_table(k_ibz,sop_tab(R_is,S))
   !
   do i1=1,k%nbz
     ikibz=k%sstar(i1,1)
     ir   =k%sstar(i1,2)
     do is=1,nsym
       ikbz_is_table(i1,is)=ik_is_table(ikibz,sop_tab(is,ir))
     enddo
   enddo
   deallocate(ik_is_table)
   call mem_est("ik_is_table")
 
   end subroutine
   !
   ! GPL_START
   !
 end function
