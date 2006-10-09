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
 subroutine elph_pw_dbs(k,q)
!===========================
 use ELPH,                ONLY:ph_freqs_sq,ph_modes,elph_bands,elph_free
 use com,                 ONLY:msg
 use R_lattice,           ONLY:bz_samp,nqibz
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,&
&                              OP_RD_CL,VERIFY
 use timing,              ONLY:live_timing
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,       ONLY:par_indexes
 implicit none
 type(bz_samp) ::k,q
!
!ws
 type(pp_indexes)  ::px
 integer           ::iq,io_pw_err(nqibz),io_elph_err,ID
 integer, external ::ioELPH
!
 call section("*","== Electron-Phonon Interface: Databases ==")
!--------------------------------------------------------------
 call pp_indexes_reset(px)
!

 call k_ibz2bz(k,'i',.false.)
 call par_indexes(px,(/nqibz/),(/1/))
 call live_timing('ELPH databases (CHECK)',px%stps(myid+1))
 io_pw_err=0
 do iq=1,q%nibz
   if (.not.px%i1p(iq)) cycle
   io_pw_err(iq)=elph_pw_dbs_io(-iq)
   call live_timing(steps=1)
 enddo
 call live_timing()
 call pp_redux_wait(io_pw_err)
 if (any((/io_pw_err==-1/))) then
   call msg("s",":: Databases incomplete")
   return
 endif
 if (any((/io_pw_err==-2/))) then
   call msg("s",":: Database incorrect Q-point")
   return
 endif
!
! Check if DB is already done
!
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_elph_err=ioELPH(ID)
 if (io_elph_err==0) return
!
 call io_control(ACTION=OP_APP_WR_CL,SEC=(/1/),ID=ID)
 io_elph_err=ioELPH(ID)
 call live_timing('ELPH databases (WRITE)',nqibz)
 do iq=1,q%nibz
   io_elph_err=elph_pw_dbs_io(iq)
   if (io_elph_err==-3) then
     call msg("l"," incorrect K-point correspondance")
     return
   endif
   call io_control(ACTION=OP_APP_WR_CL,SEC=(/iq+1/),ID=ID)
   io_elph_err=ioELPH(ID)
   call live_timing(steps=1)
 enddo
 call live_timing()
 call msg("s",':: Modes           :',ph_modes)
 call msg("s",':: Bands range     :',elph_bands)
!
 call elph_free()
!
 contains
!
  integer function elph_pw_dbs_io(iq)
!=====================================
!
! s.dbph_ structure
!
!   write (99) nmodes,nk_2_write,self_lowb,self_highb,.false.
!   write (99) alat,xq,xk(:,:nk_2_write)
!   write (99) w2(:)
!   DO ik=1,nksq
!     write (99) ikq,ikk
!     write(99) self_gkkp(:,:,:)
!   ENDDO
!
  use pars,           ONLY:SP,DP,schlen
  use D_lattice,      ONLY:nsym,sop_inv,alat
  use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,&
&                          io_netcdf_support,io_disconnect,io_file
  use ELPH,           ONLY:ph_modes,elph_bands,elph_alloc,elph_dV,ph_freqs_sq
  use SPP,            ONLY:elph_dbs_path
  use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
  implicit none
  integer           :: iq
!ws
  integer           :: pw_nk,ib1,ib2
  real(SP)          :: r_v(3)
!I/O
  integer           :: ID,io_err
  character(schlen) :: db_name
  real(SP)          :: dVRY2Ha
  real(DP)          :: pw_k(3,2*k%nbz),pw_alat,pw_q(3)
  real(DP),    allocatable :: ph_freqs_sq_disk(:)
  complex(DP), allocatable :: elph_dV_disk(:,:,:)
! K Table
  integer           :: i1,ik,k_found(k%nbz),k2k_tab(2*k%nbz),nk_dV,ikkp(2),&
&                      ikkp_s,ikbz_rot
  integer           :: kst(k%nbz,nsym),nsmall,small(nsym)

  elph_pw_dbs_io=-1

  call io_control(ACTION=OP_RD,ID=ID)

  io_netcdf_support(ID)=.FALSE.

  write (db_name,'(a,i6.6)') 's.dbph_',iabs(iq)
  io_err=io_connect(desc=trim(db_name),subfolder=trim(elph_dbs_path),type=-1,ID=ID)

  elph_pw_dbs_io=io_err
  if (io_err/=0) return  

! HEADER 
!--------
  read (io_unit(ID)) ph_modes,pw_nk,elph_bands
  read (io_unit(ID)) pw_alat,pw_q,pw_k(:,:pw_nk)
!
  if (any(alat/=pw_alat)) then
    forall (i1=1:3) pw_q(i1)=pw_q(i1)*alat(i1)/pw_alat
    forall (i1=1:3) pw_k(i1,:)=pw_k(i1,:)*alat(i1)/pw_alat
  endif
!
  if (iq<0) then
    call c2a(v_in= real(pw_q(:))-q%pt(iabs(iq),:) ,v_out=r_v,mode="ki2a")
    if (.not.rlu_v_is_zero(r_v)) elph_pw_dbs_io=-2
    goto 1
  endif
!
! Allocation
!----
  if (iq==1) call elph_alloc()
  elph_dV=(0.,0.)
  allocate(ph_freqs_sq_disk(ph_modes))
!
! PW energies are in Rydbergs. Here we have the phonon frequencies square
!-------------------------------------------------------------------------
  read (io_unit(ID)) ph_freqs_sq_disk
  ph_freqs_sq(iq,:)=abs(ph_freqs_sq_disk)/4._DP
!
! Espresso <-> SELF k table correspondance
!------------------------------------------
  k2k_tab=0
  k_found=0
  do i1=1,pw_nk
!
! For non zero phonon q PW grid is composed of (k_1,k_1+q,k_2,k_2+q ...).
! I table the  k1+q,k2+q ...
!
    if (.not.v_is_zero(real(pw_q(:))).and.mod(i1,2)/=0) cycle
    do ik=1,k%nbz
      if (k_found(ik)==1) cycle
      call c2a(v_in=k%ptbz(ik,:)-real(pw_k(:,i1)),v_out=r_v,mode="ki2a")
      if (rlu_v_is_zero(r_v)) then
        k2k_tab(i1)=ik
        k_found(ik)=1
      endif
    enddo
    if (k2k_tab(i1)==0) then
      elph_pw_dbs_io=-3
      goto 1
    endif
  enddo
! 
! ELPH_dV
!=========
!
  allocate(elph_dV_disk(elph_bands(2),elph_bands(2),ph_modes))
  nk_dV=pw_nk
  k_found=0
  if (.not.v_is_zero(real(pw_q(:)))) nk_dV=pw_nk/2
  do i1=1,nk_dV
    read (io_unit(ID)) ikkp     ! k k-q  
    ikkp_s=k2k_tab(ikkp(1))
    k_found(ikkp_s)=1
!
! Let's remember it again:
!                                ib1                             ib2
!                                |                               | 
! el_ph_mat(i,j,k,I)= <\psi(k+q) n_i|dV_{SCF}/du^q_{i a}|\psi(k) n_j>
!                           |                                 |
!                           ikkp(1)                           ikkp(2)
!
    read (io_unit(ID)) elph_dV_disk
    do ib1=elph_bands(1),elph_bands(2)
      do ib2=elph_bands(1),elph_bands(2)
!
! PW energies are in Rydbergs.
!
!   Here we have <dV/dr>. Now [<V>]=[E] but [<r>]=[E^{-1/2}] so
!
!   [<dV/dr>] = [E^{3/2}]   
!
        dVRY2Ha=2._DP**(-3._DP/2._DP)
!
! If I am reading in the Q mode I need to store all <k1|g|k1-q> ME.
! So k1 is the index.
!
! dV(i,nu,n,n')= <\psi(k) n|dV_{SCF}/du^{nu}|\psi(k-q) n'>
!
! NOTE THE BAND ORDER (imposed by PW)
!
        elph_dV(ikkp_s,:,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)=&
&              elph_dV_disk(ib1,ib2,:)*dVRY2Ha
      enddo
    enddo
  enddo
!
!  The pw_k K-grid is reduced using the small group of q. 
!  I need to expand the K-grid to define the missing elements of elph_dV
!-----------------------------------------------------------------------
  call k_syms_tab(kst)
  call q_small(real(pw_q),small,nsmall)
  do ik=1,k%nbz
    if (k_found(ik)==0) cycle
    do i1=1,nsmall
      ikbz_rot=kst(ik,sop_inv(small(i1)))
      if (k_found(ikbz_rot)/=0) cycle
      elph_dV(ikbz_rot,:,:,:)=elph_dV(ik,:,:,:)
      k_found(ikbz_rot)=-1
    enddo
  enddo
  if (any((/k_found==0/))) then
    elph_pw_dbs_io=-3
    goto 1
  endif
!
! CLEAN
!
 deallocate(ph_freqs_sq_disk,elph_dV_disk)

1 call io_control(ACTION=RD_CL,ID=ID)
  call io_disconnect(ID)

  end function
!
  subroutine q_small(pw_q,small,nsmall)
!======================================
  use pars,           ONLY:SP
  use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
  use D_lattice,      ONLY:nsym
  use R_lattice,      ONLY:rl_sop,nkbz
  integer  :: nsmall,small(nsym)
  real(SP) :: pw_q(3),r_v(3)
  integer  :: is
  small=0
  nsmall=0
  do is=1,nsym
    r_v=matmul(rl_sop(:,:,is),pw_q) -pw_q
    call k2bz(r_v)
    call c2a (v_in=r_v,mode='ki2a')
    if (rlu_v_is_zero(r_v)) then
      nsmall=nsmall+1
      small(nsmall)=is
    endif
  enddo
  end subroutine
!
  subroutine k_syms_tab(kst)
!============================
  use pars,           ONLY:SP
  use vec_operate,    ONLY:rlu_v_is_zero,c2a
  use D_lattice,      ONLY:nsym
  use R_lattice,      ONLY:rl_sop,nkbz
  implicit none
  integer :: kst(nkbz,nsym)
  integer :: i1,is,i2
  real(SP):: r_v(3)
  kst=0
  do i1=1,k%nbz
    do is=1,nsym
      do i2=1,k%nbz
        call c2a(v_in=matmul(rl_sop(:,:,is),k%ptbz(i1,:))-k%ptbz(i2,:),&
&                v_out=r_v,mode='ki2a')
        if (rlu_v_is_zero(r_v)) then
          kst(i1,is)=i2
          exit
        endif
      enddo
    enddo
  enddo
  end subroutine
!
 end subroutine

