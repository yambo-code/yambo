!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
integer function elph_databases(k,E,q)
 !
 use pars,                ONLY:DP,SP
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use ELPH,                ONLY:ph_freqs_sq,ph_modes,elph_nb,elph_use_q_grid,&
&                              elph_global_free,max_ph_freq,elph_global_alloc,&
&                              elph_nDBs,ph_qpt
 use com,                 ONLY:msg,warning,file_exists,error
 use R_lattice,           ONLY:bz_samp
 use D_lattice,           ONLY:nsym
 use YPP,                 ONLY:ph_freqs_file
 use IO_m,                ONLY:io_control,OP_APP_WR_CL,OP_RD_CL,VERIFY
 use timing,              ONLY:live_timing
 implicit none
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 !Work Space...
 !
 !... shadows ...
 !
 integer, parameter ::max_nq_in_db=10000
 real(SP)           ::ph_qpt_(max_nq_in_db,3)
 !
 !... I/O 
 !
 integer            ::idb,io_err,io_elph_err,ID
 integer, external  ::ioELPH 
 logical            ::use_ext_ph_freqs,abinit_DB
 !
 !... dummies 
 !
 integer,allocatable::ikbz_is_table(:,:)
 integer            ::iq,elph_nk,iq_db(q%nbz)
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
 elph_nDBs=0
 iq_db=0
 elph_databases=0
 !
 do while (.TRUE.)
   elph_nDBs=elph_nDBs+1
   io_err=elph_databases_io(-elph_nDBs)
   if (io_err==-1) exit
   if (io_err==-2) then
     call msg("l","incorrect K-point correspondance")
     return
   endif
   if (io_err>0) iq_db(elph_nDBs)=io_err
 enddo
 elph_nDBs=elph_nDBs-1
 if (elph_nDBs==0) then
   call msg("l","not found")
   return
 endif
 if (elph_use_q_grid) then
   call msg("l",'found '//trim(intc(elph_nDBs))//' Q-grid compatible')
 else
   call msg("l",'found '//trim(intc(elph_nDBs))//' NOT Q-grid compatible')
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
 io_elph_err=ioELPH(ID,'gkkp')
 if (io_elph_err==0) return
 !
 ! Allocation...
 !
 call elph_global_alloc('gkkp')
 !
 ! ... rebuilding of ph q-points table
 !
 do idb=1,elph_nDBs
   iq=idb
   if (elph_use_q_grid) iq=iq_db(idb)
   ph_qpt(iq,:)=ph_qpt_(iq,:)
 enddo
 !
 ! Force DB fragmentation
 !
 call IO_and_Messaging_switch("DB_is_fragmented")
 !
 ! With the external ph_freqs_file I can overwrite the 
 ! phonon frequencies read from the s.dbph* files. In this way I can, for example, 
 ! use ph freqs obtained via interpolation (q2r, matdyn ...).
 ! 
 use_ext_ph_freqs=.false.
 if (file_exists(trim(ph_freqs_file))) then
   call msg('s',"Phonon frequencies (re)read from: "//trim(ph_freqs_file))
   use_ext_ph_freqs=.true.
   open(unit=99,file=trim(ph_freqs_file))
   read(99,*)
 endif
 !
 call live_timing('ELPH databases (WRITE)',elph_nDBs,SERIAL=.true.)
 !
 do idb=1,elph_nDBs
   !
   iq=idb
   if (elph_use_q_grid) iq=iq_db(idb)
   !
   io_err=elph_databases_io(idb)
   !
   if (io_err==-1) then
     call live_timing(steps=1)
     cycle
   endif
   !
   if (io_err==-4) call warning("K-grid is not fully q-connected @ Q"//trim(intc(iq)))
   !
   if (idb==1) then
     call io_control(ACTION=OP_APP_WR_CL,SEC=(/1,iq+1/),ID=ID)
   else
     call io_control(ACTION=OP_APP_WR_CL,SEC=(/iq+1/),ID=ID)
   endif
   io_elph_err=ioELPH(ID,'gkkp')
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 if (use_ext_ph_freqs) close(99)
 !
 call msg("s",':: Modes           :',ph_modes)
 call msg("s",':: Bands range     :',elph_nb)
 !
 call elph_global_free()
 !
 call IO_and_Messaging_switch(" ")
 !
 contains
   !
   integer function elph_databases_io(idb_)
   !------------------------------------
   !
   ! s.dbph_ structure
   !
   !   read (99) ph_modes,elph_nk,elph_nb
   !   read (99) alat_DP,Q_DP,K_DP
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
   use units,          ONLY:HA2CM1
   use D_lattice,      ONLY:nsym,sop_inv,alat,n_atoms_species
   use R_lattice,      ONLY:nqibz
   use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,&
&                           io_netcdf_support,io_disconnect,io_file
   use ELPH,           ONLY:ph_modes,elph_nb,E_k_plus_q,&
&                           ph_freqs_sq,elph_gkkp,elph_DW,pol_vector
   use YPP,            ONLY:elph_dbs_path
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
   integer           :: ID,io_err,type_
   character(schlen) :: db_name
   real(SP)          :: dVRY2Ha,ph_q_disk(3),ph_q(3),K_(3,k%nbz)
   real(DP)          :: K_DP(3,k%nbz),alat_DP,Q_DP(3),Ek(n_bands),&
&                       Ek_p_q(n_bands),GS_E_k_shift
   real(DP),    allocatable :: ph_freqs_sq_disk(:)
   complex(DP), allocatable :: gkkp_disk(:,:,:),pol_vec(:,:,:)
   !
   ! K Table
   !
   integer           :: i1,ik,k_found(k%nbz),k2k_tab(k%nbz),ikbz_rot,iq_check
   integer           :: nsmall,small(nsym)
   !
   elph_databases_io=-1
   !
   call io_control(ACTION=OP_RD,ID=ID)
   !
   io_netcdf_support(ID)=.FALSE.
   !
   write (db_name,'(a,i6.6)') 's.dbph_',iabs(idb_)
   io_err=io_connect(desc=trim(elph_dbs_path)//"/"//trim(db_name),type=-2,ID=ID)
   !
   elph_databases_io=io_err
   if (io_err/=0) return  
   !
   ! HEADER 
   !
   read (io_unit(ID)) ph_modes,elph_nk,elph_nb
   read (io_unit(ID)) alat_DP,Q_DP,K_DP(:,:elph_nk) 
   !
   abinit_DB=alat_DP<0._DP
   !
   if (elph_nb>n_bands) elph_databases_io=-1
   if (elph_nb>n_bands) return
   !
   if (abinit_DB) then
     if (idb_==-1) call msg("l","[ABINIT] ...")
     ph_q_disk(:)=Q_DP(:)
     call c2a(v_in=ph_q_disk,mode="ka2i")
     K_=K_DP
     do i1=1,elph_nk
       call c2a(v_in=K_(:,i1),mode="ka2i")
     enddo
   else
     if (idb_==-1) call msg("l","[PHONON] ...")
     !
     ! In PW Q/K points are in cartesian coordinates (units of 2 PI /alat_DP)  
     !
     !  K_pw=K_cc*alat_DP/2/pi
     !  K_yambo=K_cc*alat(:)/2/pi=K_pw*alat(:)/alat_DP
     !
     ph_q_disk(:)=Q_DP(:)/alat_DP*alat(:)
     forall (i1=1:elph_nk) K_(:,i1)=K_DP(:,i1)/alat_DP*alat(:)
     !
   endif
   !
   if (idb_<0) then
     iq_check=0
     do i1=1,nqibz
       if (v_is_zero(ph_q_disk(:)+q%pt(i1,:),zero_=k_iku_zero)) iq_check=i1
     enddo
     if (iq_check==0) elph_use_q_grid=.false.
     elph_databases_io=iq_check
     !
     ! When elph_use_q_grid = .false. idb_ is the q-pt counter
     !
     if (iq_check==0) iq_check=iabs(idb_)
     !
     ph_qpt_(iq_check,:)=ph_q_disk
     !
   endif
   !
   ! Espresso <-> YAMBO k table correspondance
   !
   k2k_tab=0
   k_found=0
   do i1=1,elph_nk
     !
     ! For non zero phonon q GS grid is composed of (k_1,k_1+q,k_2,k_2+q ...).
     ! I table the  k1,k2 ...
     !
     do ik=1,k%nbz
       if (k_found(ik)==1) cycle
       call c2a(v_in=k%ptbz(ik,:)-K_(:,i1),v_out=r_v,mode="ki2a")
       if (rlu_v_is_zero(r_v,zero_=k_rlu_zero)) then
         k2k_tab(i1)=ik
         k_found(ik)=1
         exit
       endif
     enddo
     if (k2k_tab(i1)==0) elph_databases_io=-2
   enddo
   !
   ! Phonon Frequencies
   !
   allocate(ph_freqs_sq_disk(ph_modes))
   !
   ! GS energies are in Rydbergs(PW)/Hartree(Abinit). Here we have the phonon frequencies square
   !
   read (io_unit(ID)) ph_freqs_sq_disk
   !
   ph_freqs_sq_disk=abs(ph_freqs_sq_disk)
   !
   do il=1,ph_modes
     !
     ph_freq=sqrt( max( real(ph_freqs_sq_disk(il)),0._SP))
     !
     ! PW energies are in Rydbergs
     !
     if (.not.abinit_DB) ph_freq=ph_freq/2._SP
     !
     max_ph_freq=max(max_ph_freq,ph_freq)
     !
   enddo
   if (idb_<0) then
     deallocate(ph_freqs_sq_disk)
     goto 1
   endif
   if (use_ext_ph_freqs) then
     read(99,*) ph_q 
     if (.not.v_is_zero(ph_q(:)-real(Q_DP(:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_freqs_file))
     do i1=1,ph_modes,6
       read(99,*) ph_freqs_sq(iq,i1:min(i1+5,ph_modes))
     enddo
     ph_freqs_sq(iq,:)=(abs(ph_freqs_sq(iq,:))/HA2CM1)**2.
   else
     do il=1,ph_modes
       ph_freqs_sq(iq,il)=max( real(ph_freqs_sq_disk(il)),0._SP)
       if (.not.abinit_DB)  ph_freqs_sq(iq,il)=ph_freqs_sq(iq,il)/4._SP
     enddo
   endif
   !
   elph_gkkp=(0._SP,0._SP)
   elph_DW  =(0._SP,0._SP)
   ! 
   ! ELPH_gkkp 
   !
   nat=sum(n_atoms_species)
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
     !                           ik[GS]/k2k_tab(ik)[YAMBO]  
     !
     ! I = (i,a)
     !
     ! In GS we define
     !
     !  gkkp_disk(i,j,k,l) = el_ph_mat(i,j,k,I) u(I,l)^* eps_I(q l)/sqrt(M_a)
     !
     ! However YAMBO table describe the k->k-q transitions and not k+q. So we 
     ! define
     !
     ! g_jik^{qI}|_YAMBO= g_jik^{-qI}|_GS = <k-q n_i|dV_{SCF}/du^-q_{i a}|k n_j>
     !
     ! where k = k2k_tab(ik). Note that this procedure implies that YAMBO {q}'s
     ! are -{q}'s in GS (note the MinusQ flag).
     !
     read (io_unit(ID)) gkkp_disk
     read (io_unit(ID)) pol_vec
     !
     if (ik==1) pol_vector=pol_vec
     !
     if (idb==1) read (io_unit(ID)) grad_at_gamma(k2k_tab(ik),:,:,:,:)
     !
     if (.not.abinit_DB) then
       read (io_unit(ID)) Ek(:elph_nb)
       read (io_unit(ID)) Ek_p_q(:elph_nb)
       !
       ! To allign correctly the E(k+q) energies I use the shift between
       ! the ABSOLUTE YAMBO energy levels at k and the GS ones.
       !
       GS_E_k_shift=real(Ek(1)/2._DP)-(E%E(1,k%sstar(k2k_tab(ik),1),1)+E%Efermi(1))
       !
       E_k_plus_q(:,k2k_tab(ik),1)=real(Ek_p_q(:elph_nb)/2._DP)-GS_E_k_shift
       !
     else
       E_k_plus_q(:,k2k_tab(ik),1)=0.
     endif
     !
     do ib1=1,elph_nb
       do ib2=1,elph_nb
         !
         ! PW     energies are in Rydbergs.
         ! Abinit energies are in Hartrees.
         !
         ! YAMBO is in HARTREE
         !
         ! Thus, in the case of PW I need to rescale the gkkp
         !
         !   Here we have <dV/dr>. Now [<V>]=[E] but [<r>]=[E^{-1/2}] so
         !
         !   [<dV/dr>] = [E^{3/2}]   
         !
         dVRY2Ha=2._DP**(-3._DP/2._DP)
         if (abinit_DB) dVRY2Ha=1._DP
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
   call q_small(ph_q_disk,small,nsmall)
   !
   ! The GS K-grid is reduced using the small group of q. 
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
   if (any((/k_found==0/))) elph_databases_io=-4
   !
   ! CLEAN
   !
   deallocate(ph_freqs_sq_disk,gkkp_disk,pol_vec)
   if (idb==elph_nDBs) deallocate(grad_at_gamma)
   !
1  call io_control(ACTION=RD_CL,ID=ID)
   call io_disconnect(ID)
   !
   end function
   !
   subroutine q_small(q_,small,nsmall)
   !----------------------------------
   !
   use pars,           ONLY:SP
   use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
   use D_lattice,      ONLY:nsym
   use R_lattice,      ONLY:rl_sop,nkbz
   use zeros,          ONLY:k_rlu_zero
   !
   integer  :: nsmall,small(nsym)
   real(SP) :: q_(3),r_v(3)
   integer  :: is
   small=0
   nsmall=0
   do is=1,nsym
     r_v=matmul(rl_sop(:,:,is),q_) -q_
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
end function
