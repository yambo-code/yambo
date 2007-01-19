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
 use ELPH,                ONLY:ph_freqs_sq,ph_modes,elph_nb,elph_dV_free,&
&                              elph_gkkp_free
 use com,                 ONLY:msg
 use R_lattice,           ONLY:bz_samp,nqibz
 use D_lattice,           ONLY:nsym
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,&
&                              OP_RD_CL,VERIFY
 use timing,              ONLY:live_timing
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,       ONLY:par_indexes
 !DEBUG
 use parser_m,            ONLY:parser
 !END DEBUG
 implicit none
 type(bz_samp) ::k,q
!
!ws GPL_END
 type(pp_indexes)   ::px
 integer,allocatable::ikbz_is_table(:,:)
 integer            ::iq,io_pw_err(nqibz),io_elph_err,ID,X_ng
 integer, external  ::ioELPH 
 !
 !DEBUG
 logical :: l_use_dV
 call parser('UseDV',l_use_dV)
 !END DEBUG 
 !
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
 call msg("s",':: Bands range     :',elph_nb)
!
 call elph_gkkp_free()
 call elph_dV_free()
!
 contains
!
  integer function elph_pw_dbs_io(iq)
!=====================================
!
! s.dbph_ structure
!
!   read (99) ph_modes,dV_nk,pw_ngx,elph_nb
!   read (99) pw_alat,pw_q,pw_kpq
!   read (99) ph_freqs_sq
!   DO ik=1,nksq
!     read (99) dV_bare_disk(:,:,:)  (pw_ngx,elph_nb,ph_modes)
!     read (99) dV_bare_psi(:,:,:)   (pw_ngx,elph_nb,dV_nk)
!     read (99) elph_dV_disk(:,:,:)  (elph_nb,elph_nb,ph_modes)
!   ENDDO
!
  use pars,           ONLY:SP,DP,schlen
  use D_lattice,      ONLY:nsym,sop_inv,alat
  use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,&
&                          io_netcdf_support,io_disconnect,io_file
  use ELPH,           ONLY:ph_modes,elph_nb,elph_dV_alloc,elph_dV,elph_dV_psi,&
&                          ph_freqs_sq,elph_gkkp,elph_gkkp_alloc,dV_ngx,dV_nk
  use SPP,            ONLY:elph_dbs_path
  use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
  implicit none
  integer           :: iq
!
!Work Space
!
  integer           :: ib1,ib2,ig
  real(SP)          :: r_v(3)
!I/O
  integer           :: ID,io_err
  character(schlen) :: db_name
  real(SP)          :: dVRY2Ha
  real(DP)          :: pw_kpq(3,k%nbz),pw_alat,pw_q(3)
  real(DP),    allocatable :: ph_freqs_sq_disk(:)
  complex(DP), allocatable :: gkkp_disk(:,:,:),dV_disk(:,:,:),dV_psi_disk(:,:)
! K Table
  integer           :: i1,ik,k_found(k%nbz),k2k_tab(k%nbz),ikbz_rot
  integer           :: nsmall,small(nsym)

  elph_pw_dbs_io=-1

  call io_control(ACTION=OP_RD,ID=ID)

  io_netcdf_support(ID)=.FALSE.

  write (db_name,'(a,i6.6)') 's.dbph_',iabs(iq)
  io_err=io_connect(desc=trim(db_name),subfolder=trim(elph_dbs_path),type=-1,ID=ID)

  elph_pw_dbs_io=io_err
  if (io_err/=0) return  

! HEADER 
!--------
  read (io_unit(ID)) ph_modes,dV_nk,dV_ngx,elph_nb
  read (io_unit(ID)) pw_alat,pw_q,pw_kpq(:,:dV_nk)
!
  if (any(alat/=pw_alat)) then
    forall (i1=1:3) pw_q(i1)=pw_q(i1)*alat(i1)/pw_alat
    forall (i1=1:3) pw_kpq(i1,:)=pw_kpq(i1,:)*alat(i1)/pw_alat
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
  if (iq==1) call elph_gkkp_alloc()
  if (iq> 1) call elph_dV_free()
  call elph_dV_alloc()
  !
  elph_dV=(0._DP,0._DP)
  elph_gkkp=(0._DP,0._DP)
  elph_dV_psi=(0._DP,0._DP)
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
  do i1=1,dV_nk
!
! For non zero phonon q PW grid is composed of (k_1,k_1+q,k_2,k_2+q ...).
! I table the  k1+q,k2+q ...
!
    do ik=1,k%nbz
      if (k_found(ik)==1) cycle
      call c2a(v_in=k%ptbz(ik,:)-real(pw_kpq(:,i1),SP),v_out=r_v,mode="ki2a")
      if (rlu_v_is_zero(r_v)) then
        k2k_tab(i1)=ik
        k_found(ik)=1
        exit
      endif
    enddo
    if (k2k_tab(i1)==0) then
      elph_pw_dbs_io=-3
      goto 1
    endif
  enddo
! 
! ELPH_gkkp & dV
!===================
!
  allocate(gkkp_disk(elph_nb,elph_nb,ph_modes))
  allocate(dV_disk(dV_ngx,elph_nb,ph_modes))
  allocate(dV_psi_disk(dV_ngx,elph_nb))
!
! Reading
!=========
  do ik=1,dV_nk
!
! Let's remember it again:
!                                ib1                             ib2
!                                |                               | 
! el_ph_mat(i,j,k,I)= <\psi(k+q) n_i|dV_{SCF}/du^q_{i a}|\psi(k) n_j>
!                           |                          
!                           ik[PW]/k2k_tab(ik)[SELF]  
!
    read (io_unit(ID)) dV_disk
    read (io_unit(ID)) dV_psi_disk
    read (io_unit(ID)) gkkp_disk
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
! If I am reading in the Q mode I need to store all <k1|g|k1-q> ME.
! So k1 is the index.
!
! dV(i,nu,n,n')= <\psi(k) n|dV_{SCF}/du^{nu}|\psi(k-q) n'>
!
! NOTE THE BAND ORDER (imposed by PW)
!
        elph_gkkp(k2k_tab(ik),:,ib1,ib2)=gkkp_disk(ib1,ib2,:)*dVRY2Ha
        !
      enddo !ib2
!
      do ig=1,dV_ngx
        elph_dV(ig,ib1,ik,:)=dV_disk(ig,ib1,:)*dVRY2Ha
        elph_dV_psi(ig,ib1,ik)=dV_psi_disk(ig,ib1)
      enddo
      !
    enddo
  enddo
!
!  The pw_kpq K-grid is reduced using the small group of q. 
!  I need to expand the K-grid to define the missing elements of elph_dV
!-----------------------------------------------------------------------
  !
  ! The small group of q
  ! 
  call q_small(real(pw_q,SP),small,nsmall)
  !
  if (l_use_dV) call elph_dV_2_gkkp(iq,k2k_tab,X_ng)
  !
  do ik=1,k%nbz
    if (k_found(ik)==0) cycle
    do i1=1,nsmall
      ikbz_rot=ikbz_is_table(ik,sop_inv(small(i1)))
      if (k_found(ikbz_rot)/=0) cycle
      elph_gkkp(ikbz_rot,:,:,:)=elph_gkkp(ik,:,:,:)
      k_found(ikbz_rot)=-1
    enddo
  enddo
!
  if (any((/k_found==0/))) then
    elph_pw_dbs_io=-3
    goto 1
  endif
!
! CLEAN
!
 deallocate(ph_freqs_sq_disk,dV_disk,gkkp_disk,dV_psi_disk)

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
  subroutine k_syms_tab(ikbz_is_table)
!=====================================
  use pars,           ONLY:SP
  use vec_operate,    ONLY:rlu_v_is_zero,c2a
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
 end subroutine

