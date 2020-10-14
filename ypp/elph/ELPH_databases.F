!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, FP
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
integer function ELPH_databases(k,E,q)
 !
 use pars,                ONLY:DP,SP,schlen
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use ELPH,                ONLY:ph_freqs_sq,ph_modes,elph_nb,elph_use_q_grid,&
&                              elph_global_free,W_debye,elph_global_alloc,&
&                              elph_nDBs,ph_qpt,ph_kpt_bz,elph_nk_bz
 use com,                 ONLY:msg
 use R_lattice,           ONLY:bz_samp,qindx_B
 use D_lattice,           ONLY:nsym,n_atoms
 use parser_m,            ONLY:parser
 use YPP_ELPH,            ONLY:ph_freqs_file,ph_modes_file
 use IO_m,                ONLY:io_control,OP_APP_CL,OP_RD_CL,VERIFY,IO_and_Messaging_switch
 use LIVE_t,              ONLY:live_timing 
 use YPPm,                ONLY:k_grid_printer,alat_used_for_output
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 !Work Space...
 !
 !... shadows ...
 !
 integer, parameter ::max_nq_in_db=10000
 real(SP)           ::ph_qpt_(max_nq_in_db,3),ph_q_disk(max_nq_in_db,3)
 !
 !... I/O 
 !
 integer            ::idb,io_err,io_elph_err,ID,nDBs_disk
 integer, external  ::io_ELPH 
 logical            ::use_ext_ph_freqs,use_ext_ph_modes,abinit_DB
 character(schlen)  ::db_name
 !
 !... dummies 
 !
 logical            ::l_gkkp_expand
 logical            ::l_gkkp_bare=.FALSE.
 logical            ::l_are_bare_there=.FALSE.
 integer,allocatable::ikbz_is_table(:,:)
 integer            ::iq,elph_nk,iq_db(q%nbz),iq_bz,iq_s,i1
 complex(DP), allocatable :: grad_at_gamma(:,:,:,:,:)
 !
 logical, external  ::file_exists
 !
 call section("*","== Electron-Phonon Interface: PW->Yambo Databases ==")
 !
 call k_ibz2bz(k,'i',.false.)
 !
 call parser('GkkpExpand',l_gkkp_expand)
 !
 if (l_gkkp_expand.and..not.allocated(qindx_B)) then
   call msg('s',"Missing k/q scattering database")
   call error("Please run a setup with the BSEscatt flag activated")
 endif
 !
 call msg('s','PW(ELPH) databases ...')
 !
 io_err=0
 W_debye=0.
 elph_use_q_grid=.true.
 elph_nDBs=0
 nDBs_disk=0
 iq_db=0
 ELPH_databases=0
 !
 do while (.TRUE.)
   elph_nDBs=elph_nDBs+1
   io_err=ELPH_databases_io(-elph_nDBs)
   if (io_err==-1) exit
   if (io_err==-2) then
     call msg("l","incorrect K-point correspondance")
     return
   endif
   if (io_err>0) iq_db(elph_nDBs)=io_err
 enddo
 elph_nDBs=elph_nDBs-1
 if (elph_nDBs==0) then
   ELPH_databases=-2
   call msg("l","not found")
   return
 endif
 !
 !NOTE: The bare databases are copies of the screened ones in all variables
 !      except elph_gkkp. This depends on how elphondb.f90 in QE/PHonon was
 !      modified.
 !      Therefore such redundant variables are overwritten when l_gkkp_bare is
 !      on,
 !      including io_err at the function call of ELPH_databases_io.
 !
 call parser('GkkpReadBare',l_gkkp_bare)
 if (l_gkkp_bare)    call msg('s','Reading also bare couplings')
 !
 db_name='gkkp'
 if (l_gkkp_bare) db_name='gkkp_plus_bare'
 nDBs_disk=elph_nDBs
 !
 if (elph_use_q_grid) then
   !
   call msg("l",'found '//trim(intc(elph_nDBs))//' Q-grid compatible')
   !
   if (l_gkkp_expand) then
     !
     call k_ibz2bz(q,'i',.false.)
     !
     elph_nDBs=q%nbz
     db_name='gkkp_expanded'
     if (l_gkkp_bare) db_name='gkkp_expanded_plus_bare'
     !
     call msg("s",'Database expanded to the whole '//trim(intc(elph_nDBs))//' BZ Q-grid')
     !
   endif
   !
 else
   !
   call msg("l",'found '//trim(intc(elph_nDBs))//' NOT Q-grid compatible')
   !
   ! In this case there is no sense in expanding the gkkp
   !
   l_gkkp_expand=.FALSE.
   !
 endif
 !
 ! Define elph_nk_bz before the ioELPH call to avoid a  io_elph_err<0 here and 
 ! >0 after elph_nk_bz is defined
 !
 elph_nk_bz=k%nbz
 !
 ! KBZ + SYM table
 !
 YAMBO_ALLOC(ikbz_is_table,(k%nbz,nsym))
 call k_syms_tab(ikbz_is_table)
 !
 ! Check if DB is already done
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_elph_err=io_ELPH(ID,trim(db_name))
 !
 if (io_elph_err==0) return
 !
 ! External freqs & modes ?
 !
 use_ext_ph_freqs=.false.
 if (file_exists(trim(ph_freqs_file))) then
   call msg('s',"Phonon frequencies (re)read from: "//trim(ph_freqs_file))
   use_ext_ph_freqs=.true.
 endif
 !
 use_ext_ph_modes=.false.
 if (file_exists(trim(ph_modes_file))) then
   call msg('s',"Phonon modes (re)read from: "//trim(ph_modes_file))
   use_ext_ph_modes=.true.
 endif
 !
 ! Allocation...
 !
 if (.not.l_gkkp_bare) then
   call elph_global_alloc('gkkp')
 else
   call elph_global_alloc('gkkp_plus_bare')
 endif
 !
 ! Keep a copy of k-points to be used by _RT to rotate the unperturbed gkkp m.e.
 !
 ph_kpt_bz=k%ptbz
 !
 ! ... rebuilding of ph q-points table
 !
 if (l_gkkp_expand) then
   ph_qpt=q%ptbz
 else
   do idb=1,elph_nDBs
     iq=idb
     if (elph_use_q_grid) iq=iq_db(idb)
     ph_qpt(iq,:)=ph_qpt_(iq,:)
   enddo
 endif
 !
 ! Force DB fragmentation
 !
 call IO_and_Messaging_switch("+DB_is_fragmented")
 !
 ! With the external ph_freqs_file I can overwrite the 
 ! phonon frequencies read from the s.dbph* files. In this way I can, for example, 
 ! use ph freqs obtained via interpolation (q2r, matdyn ...).
 ! 
 if (use_ext_ph_modes) then
   open(unit=98,file=trim(ph_modes_file))
   read(98,*)
   read(98,*)
 endif
 !
 call live_timing('ELPH databases (WRITE)',nDBs_disk)
 !
 iq_bz=0
 !
 YAMBO_ALLOC(grad_at_gamma,(k%nbz,elph_nb,elph_nb,n_atoms,3))
 !
 do idb=1,nDBs_disk
   !
   iq=idb
   if (elph_use_q_grid) iq=iq_db(idb)
   !
   if (l_gkkp_expand) then
     !
     ! Note that ph_qpt is correclty pointing to the q%ptbz. But ph_freqs_sq, instead, is not
     ! and must be re-pointed
     !
     do i1=1,q%nstar(iq)
       !
       iq_bz=iq_bz+1
       !
       iq_s=q%star(iq,i1)
       !
       if (.not.l_gkkp_bare) then
         io_err=ELPH_databases_io(idb)
       else
         io_err=ELPH_databases_io(idb) ! Obtain elph_gkkp
         io_err=ELPH_databases_io(idb) ! Obtain elph_gkkp_bare
       endif
       !
       ph_freqs_sq(iq_bz,:)=ph_freqs_sq(iq,:)
       !
       if (iq_bz==1) then
         call io_control(ACTION=OP_APP_CL,SEC=(/1/),ID=ID)
         io_elph_err=io_ELPH(ID,trim(db_name))
       endif
       !
       call ELPH_gkkp_expand(iq_bz,iq_s,k,.FALSE.) ! Expand elph_gkkp
       if (l_gkkp_bare) call ELPH_gkkp_expand(iq_bz,iq_s,k,.TRUE.) ! Expand elph_gkkp_bare
       !
       call io_control(ACTION=OP_APP_CL,SEC=(/iq_bz+1/),ID=ID)
       io_elph_err=io_ELPH(ID,trim(db_name))
       !
     enddo
     !
   else
     !
     if (.not.l_gkkp_bare) then
       io_err=ELPH_databases_io(idb)
     else
       io_err=ELPH_databases_io(idb) ! Obtain elph_gkkp
       io_err=ELPH_databases_io(idb) ! Obtain elph_gkkp_bare
     endif
     !
     if (io_err==-1) then
       call live_timing(steps=1)
       cycle
     endif
     !
     if (io_err==-4) call warning("K-grid is not fully q-connected @ Q"//trim(intc(iq)))
     !
     if (idb==1) then
       call io_control(ACTION=OP_APP_CL,SEC=(/1/),ID=ID)
       io_elph_err=io_ELPH(ID,trim(db_name))
     endif
     !
     call io_control(ACTION=OP_APP_CL,SEC=(/iq+1/),ID=ID)
     io_elph_err=io_ELPH(ID,trim(db_name))
     !
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 if (use_ext_ph_freqs) close(99)
 if (use_ext_ph_modes) close(98)
 !
 call msg("s",' Modes           ',ph_modes)
 call msg("s",' Bands range     ',elph_nb)
 !
 ! Print the list of PW q-points (to be used with matdyn, for example)
 !
 call msg("s",'Databases Q-points PW-formatted')
 call k_grid_printer(nDBs_disk,ph_q_disk(:nDBs_disk,:),'iku',"PW",coo_out="alat")
 !
 call elph_global_free()
 !
 YAMBO_FREE(grad_at_gamma)
 !
 call IO_and_Messaging_switch("-DB_is_fragmented")
 !
 contains
   !
   integer function ELPH_databases_io(idb_)
   !---------------------------------------
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
   use units,          ONLY:HA2CMm1
   use D_lattice,      ONLY:nsym,sop_inv,alat,n_atoms_species
   use R_lattice,      ONLY:nqibz
   use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,&
&                           io_disconnect,io_file
   use ELPH,           ONLY:ph_modes,elph_nb,E_k_plus_q,&
&                           ph_freqs_sq,elph_gkkp,elph_gkkp_bare,elph_DW,pol_vector
   use YPP_ELPH,       ONLY:elph_dbs_path
   use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
   use zeros,          ONLY:k_iku_zero,k_rlu_zero
   use electrons,      ONLY:n_bands
#include<memory.h>
   integer           :: idb_
   !
   !Work Space
   !
   integer           :: ib1,ib2,ig,il,ia,i,j
   real(SP)          :: r_v(3),ph_freq
   complex(DP)       :: F1(3)
   !
   !I/O
   !
   integer           :: ID,io_err,type_
   character(schlen) :: dbph_name
   character(1)      :: ws_ch(2)
   real(SP)          :: dVRY2Ha,h_q_disk(max_nq_in_db,3),ph_q(3),K_(3,k%nbz),w_vec_pot(6),pol_vec_norm
   real(DP)          :: K_DP(3,k%nbz),alat_DP,Q_DP(3),Ek(n_bands),&
&                       Ek_p_q(n_bands),GS_E_k_shift
   real(DP),    allocatable :: ph_freqs_sq_disk(:)
   complex(DP), allocatable :: gkkp_disk(:,:,:),pol_vec(:,:,:)
   !
   ! K Table
   !
   integer           :: i1,ic,ik,k_found(k%nbz),k2k_tab(k%nbz),ikbz_rot,iq_check
   integer           :: nsmall,small(nsym)
   !
   ELPH_databases_io=-1
   !
   call io_control(ACTION=OP_RD,ID=ID)
   !
   if (.not.l_are_bare_there) then
     write (dbph_name,'(a,i6.6)') 's.dbph_',iabs(idb_)
   else
     write (dbph_name,'(a,i6.6)') 's.dbph_bare_',iabs(idb_)
   endif
   !
   io_err=io_connect(desc=trim(elph_dbs_path)//"/"//trim(dbph_name),type=-2,ID=ID)
   !
   ELPH_databases_io=io_err
   if (io_err/=0) return
   !
   ! HEADER 
   !
   K_DP=0._DP
   read (io_unit(ID)) ph_modes,elph_nk,elph_nb
   read (io_unit(ID)) alat_DP,Q_DP,K_DP(:,:elph_nk) 
   !
   abinit_DB=alat_DP<0._DP
   !
   if (elph_nb>n_bands) ELPH_databases_io=-1
   if (elph_nb>n_bands) return
   !
   if (abinit_DB) then
     if (idb_==-1) call msg("l","[ABINIT] ...")
     ph_q_disk(iabs(idb_),:)=Q_DP(:)
     call c2a(v_in=ph_q_disk(iabs(idb_),:),mode="ka2i")
     K_=K_DP
     do i1=1,elph_nk
       call c2a(v_in=K_(:,i1),mode="ka2i")
     enddo
   else
     if (idb_==-1) call msg("l","[PHONON] ...")
     !
     ! In PW Q/K points are in units of 2 PI/alat_DP
     !
     !  K_pw=K_cc*alat_DP/2/pi
     !  K_yambo=K_cc*alat(:)/2/pi=K_pw*alat(:)/alat_DP
     !
     ph_q_disk(iabs(idb_),:)=Q_DP(:)/alat_DP*alat(:)
     !
     alat_used_for_output=alat_DP
     !
     forall (i1=1:elph_nk) K_(:,i1)=K_DP(:,i1)/alat_DP*alat(:)
     !
   endif
   !
   if (idb_<0) then
     !
     iq_check=0
     !
     do i1=1,nqibz
       if (v_is_zero(ph_q_disk(iabs(idb_),:)+q%pt(i1,:),zero_=k_iku_zero)) iq_check=i1
     enddo
     !
     if (iq_check==0) elph_use_q_grid=.false.
     !
     ELPH_databases_io=iq_check
     !
     ! When elph_use_q_grid = .false. idb_ is the q-pt counter
     !
     if (iq_check==0) iq_check=iabs(idb_)
     !
     ph_qpt_(iq_check,:)=ph_q_disk(iabs(idb_),:)
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
     if (k2k_tab(i1)==0) ELPH_databases_io=-2
   enddo
   !
   ! Phonon Frequencies & Modes
   !
   YAMBO_ALLOC(ph_freqs_sq_disk,(ph_modes))
   YAMBO_ALLOC(pol_vec,(ph_modes,n_atoms,3))
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
     W_debye=max(W_debye,ph_freq)
     !
   enddo
   if (idb_<0) then
     YAMBO_FREE(ph_freqs_sq_disk)
     goto 1
   endif
   if (use_ext_ph_freqs) then
     open(unit=99,file=trim(ph_freqs_file))
     read(99,*)
     do i1=1,(idb-1)*(1+ceiling(real(ph_modes)/6.))
       read(99,*) ph_q 
     enddo
     read(99,*) ph_q 
     if (.not.v_is_zero(ph_q(:)-real(Q_DP(:)),zero_=k_iku_zero).and.&
&        .not.v_is_zero(ph_q(:)+real(Q_DP(:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_freqs_file))
     do i1=1,ph_modes,6
       read(99,*) ph_freqs_sq(iq,i1:min(i1+5,ph_modes))
     enddo
     ph_freqs_sq(iq,:)=(abs(ph_freqs_sq(iq,:))/HA2CMm1)**2.
     close(99)
   else
     do il=1,ph_modes
       ph_freqs_sq(iq,il)=max( real(ph_freqs_sq_disk(il)),0._SP)
       if (.not.abinit_DB)  ph_freqs_sq(iq,il)=ph_freqs_sq(iq,il)/4._SP
     enddo
   endif
   if (use_ext_ph_modes) then
     read(98, *) ws_ch(1),ws_ch(2),ph_q 
     if (.not.v_is_zero(ph_q(:)-real(Q_DP(:)),zero_=k_iku_zero).and.&
&        .not.v_is_zero(ph_q(:)+real(Q_DP(:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_modes_file))
     read(98,*)
     do i1=1,ph_modes
       read(98,*) 
       do ia=1,n_atoms
         read(98,*) ws_ch(1),w_vec_pot
         forall(ic=1:3) pol_vector(i1,ia,ic)=cmplx(w_vec_pot(2*(ic-1)+1),w_vec_pot(2*ic),SP)
       enddo
     enddo
     read(98,*)
     read(98,*)
     read(98,*)
   endif
   !
   if (.not.l_are_bare_there) then
     elph_gkkp=(0._SP,0._SP)
   else
     elph_gkkp_bare=(0._SP,0._SP)
   endif
   elph_DW  =(0._SP,0._SP)
   ! 
   ! ELPH_gkkp 
   !
   YAMBO_ALLOC(gkkp_disk,(elph_nb,elph_nb,ph_modes))
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
     ! g_ijk^{qI}|_YAMBO= g_ijk^{-qI}|_GS = <k-q n_i|dV_{SCF}/du^-q_{i a}|k n_j>
     !
     ! where k = k2k_tab(ik). Note that this procedure implies that YAMBO {q}'s
     ! are -{q}'s in GS (note the MinusQ flag).
     !
     read (io_unit(ID)) gkkp_disk
     read (io_unit(ID)) pol_vec
     !
     if (ik==1.and..not.use_ext_ph_modes) pol_vector=pol_vec
     !
     !DEBUG>
     !if (ik==1) then
     !  write (*,'(a,i3)') 'q=',iabs(idb_)
     !  do il=1,ph_modes
     !    pol_vec_norm=0._SP
     !    do ia=1,n_atoms
     !      do ic=1,3
     !        pol_vec_norm=pol_vec_norm+conjg(pol_vec(il,ia,ic))*pol_vec(il,ia,ic)
     !      enddo
     !      write (*,'(a,i2,6f10.6)') 'Matd file ia',ia,pol_vector(il,ia,:)/159.25556471558120362737
     !      write (*,'(a,i2,6f10.6)') 'DBPH file ia',ia,pol_vec(il,ia,:)
     !    enddo
     !    write (*,'(a,i3,a,f9.6)') 'mode=',il,' norm=',pol_vec_norm
     !  enddo
     !endif
     !DEBUG<
     !
     if (idb==1) read (io_unit(ID)) grad_at_gamma(k2k_tab(ik),:,:,:,:)
     !
     if (.not.abinit_DB) then
       !
       read (io_unit(ID)) Ek(:elph_nb)
       read (io_unit(ID)) Ek_p_q(:elph_nb)
       !
       ! To allign correctly the E(k+q) energies I use the shift between
       ! the ABSOLUTE YAMBO energy levels at k and the GS ones.
       !
       GS_E_k_shift=real(Ek(1)/2._DP)-(E%E(1,k%sstar(k2k_tab(ik),1),1)+E%E_Fermi)
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
         if (.not.l_are_bare_there) then
           elph_gkkp(:,ib1,ib2,k2k_tab(ik))=gkkp_disk(ib1,ib2,:)*dVRY2Ha
         else
           elph_gkkp_bare(:,ib1,ib2,k2k_tab(ik))=gkkp_disk(ib1,ib2,:)*dVRY2Ha
         endif
         !
         ! Debye Waller Term
         !
         F1=(0._DP,0._DP)
         do ia=1,n_atoms
           F1(:)=F1(:)+grad_at_gamma(k2k_tab(ik),ib1,ib2,ia,:)*dVRY2Ha
         enddo
         !
         do i=1,3
           do j=1,3
             do ia=1,n_atoms
               do il=1,ph_modes
                 elph_DW(il,ib1,ib2,k2k_tab(ik))=elph_DW(il,ib1,ib2,k2k_tab(ik))+&
&                2._SP*real(&
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
   call q_small(ph_q_disk(iabs(idb_),:),small,nsmall)
   !
   ! The GS K-grid is reduced using the small group of q. 
   ! I need to expand the K-grid to define the missing elements of elph_dV
   !
   do ik=1,k%nbz
     if (k_found(ik)==0) cycle
     do i1=1,nsmall
       ikbz_rot=ikbz_is_table(ik,sop_inv(small(i1)))
       if (k_found(ikbz_rot)/=0) cycle
       if (.not.l_are_bare_there) then
         elph_gkkp(:,:,:,ikbz_rot)=elph_gkkp(:,:,:,ik)
       else
         elph_gkkp_bare(:,:,:,ikbz_rot)=elph_gkkp_bare(:,:,:,ik)
       endif
       elph_DW(:,:,:,ikbz_rot)=elph_DW(:,:,:,ik)
       E_k_plus_q(:,ikbz_rot,1)=E_k_plus_q(:,ik,1)
       k_found(ikbz_rot)=-1
     enddo
   enddo
   !
   if (any((/k_found==0/))) ELPH_databases_io=-4
   !
   ! CLEAN
   !
   YAMBO_FREE(ph_freqs_sq_disk)
   YAMBO_FREE(gkkp_disk)
   YAMBO_FREE(pol_vec)
   !
1  call io_control(ACTION=RD_CL,ID=ID)
   call io_disconnect(ID,FORCE=.TRUE.)
   !
   ! Set to read other elph_gkkp type at the next function call
   !
   if (l_gkkp_bare) l_are_bare_there = (.not.l_are_bare_there)
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
   use R_lattice,      ONLY:rl_sop,nkbz
   implicit none
   integer :: ikbz_is_table(nkbz,nsym)
   integer :: i1,is,ir,ikibz,i2
   real(SP):: r_v(3)
   ikbz_is_table=0
   !
   ! First I find the action of all the syms on the IBZ kpts
   !
   ! R_is k_ibz = k_{s_table(k_ibz,is)}
   !
   ! where s_table(k_ibz,is) is a BZ index
   !
   call k_build_up_BZ_tables(k)
   call k_ibz2bz(k,'i',.false.) ! in k_sym2sym there is the k_ibz2bz(k,'d',...) call
   !
   ! To extend s_table to the BZ I simply have that
   !
   ! R_is k_bz = R_is S k_ibz = s_table(k_ibz,sop_tab(R_is,S))
   !
   do i1=1,k%nbz
     ikibz=k%sstar(i1,1)
     ir   =k%sstar(i1,2)
     do is=1,nsym
       ikbz_is_table(i1,is)=k%k_table(ikibz,sop_tab(is,ir))
     enddo
   enddo
   !
   end subroutine
   !
end function
