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
integer function ELPH_databases_IO(what,GKKP,k,E,q,IDB)
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
 use D_lattice,      ONLY:nsym,alat,n_atoms,sop_inv
 use R_lattice,      ONLY:nqibz,bz_samp
 use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,io_disconnect
 use ELPH,           ONLY:ph_modes,elph_nb,GKKP_me,ph_modes,elph_nb,pol_vector,elph_use_q_grid
 use YPP_ELPH,       ONLY:elph_dbs_path,ph_freqs_file,ph_modes_file,ikbz_is_table,DB_ph_qpt,&
&                         use_ext_ph_freqs,use_ext_ph_modes,DB_ph_freq_sq,grad_at_gamma
 use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
 use zeros,          ONLY:k_iku_zero,k_rlu_zero
 use com,            ONLY:msg
 use electrons,      ONLY:n_bands,levels
 use YPPm,           ONLY:alat_used_for_output
 !
#include<memory.h>
 !
 character(*)      ::what
 integer           ::IDB
 type(GKKP_me)     ::GKKP
 type(bz_samp)     ::k,q
 type(levels)      ::E
 !
 ! Work Space
 !
 integer           :: ib1,ib2,il,ia,i,j,ID,io_err,elph_nk
 real(SP)          :: r_v(3),ph_freq
 complex(DP)       :: F1(3)
 character(schlen) :: dbph_name
 character(1)      :: ws_ch(2)
 real(SP)          :: dVRY2Ha,K_(3,k%nbz),ph_q(3),w_vec_pot(6)
 real(DP)          :: K_DP(3,k%nbz),alat_DP,Q_DP(3),Ek(n_bands),Ek_p_q(n_bands),GS_E_k_shift
 logical           ::abinit_DB
 real(DP),    allocatable :: ph_freqs_sq_disk(:)
 complex(DP), allocatable :: gkkp_disk(:,:,:),pol_vec(:,:,:)
 !
 ! K Table ... 
 integer           :: i1,ic,ik,k_found(k%nbz),k2k_tab(k%nbz),ikbz_rot,IQ
 integer           :: nsmall,small(nsym)
 !
 ELPH_databases_io=-1
 !
 call io_control(ACTION=OP_RD,ID=ID)
 !
 if (what=="dV")      write (dbph_name,'(a,i6.6)') 's.dbph_',iabs(IDB)
 if (what=="dV_bare") write (dbph_name,'(a,i6.6)') 's.dbph_bare_',iabs(IDB)
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
   if (IDB==-1) call msg("l","["//trim(what)//"] ...")
   DB_ph_qpt(iabs(IDB),:)=Q_DP(:)
   call c2a(v_in=DB_ph_qpt(iabs(IDB),:),mode="ka2i")
   K_=K_DP
   do i1=1,elph_nk
     call c2a(v_in=K_(:,i1),mode="ka2i")
   enddo
 else
   if (IDB==-1) call msg("l","["//trim(what)//"] ...")
   !
   ! In PW Q/K points are in units of 2 PI/alat_DP
   !
   !  K_pw=K_cc*alat_DP/2/pi
   !  K_yambo=K_cc*alat(:)/2/pi=K_pw*alat(:)/alat_DP
   !
   DB_ph_qpt(iabs(IDB),:)=Q_DP(:)/alat_DP*alat(:)
   !
   alat_used_for_output=alat_DP
   !
   forall (i1=1:elph_nk) K_(:,i1)=K_DP(:,i1)/alat_DP*alat(:)
   !
 endif
 !
 if (IDB<0) then
   !
   IQ=0
   !
   do i1=1,nqibz
     if (v_is_zero(DB_ph_qpt(iabs(IDB),:)+q%pt(i1,:),zero_=k_iku_zero)) IQ=i1
   enddo
   !
   if (IQ==0) elph_use_q_grid=.false.
   !
   ELPH_databases_io=IQ
   !
   ! When elph_use_q_grid = .false. IDB is the q-pt counter
   !
   if (IQ==0) IQ=iabs(IDB)
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
   !
   if (k2k_tab(i1)==0) ELPH_databases_io=-2
   !
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
 enddo
 if (IDB<0) then
   YAMBO_FREE(ph_freqs_sq_disk)
   call io_control(ACTION=RD_CL,ID=ID)
   call io_disconnect(ID,FORCE=.TRUE.)
   return
 endif
 !
 !======================
 ! END OF IDB<0 SECTION
 !======================
 if (use_ext_ph_freqs) then
   open(unit=99,file=trim(ph_freqs_file))
   read(99,*)
   do i1=1,(IDB-1)*(1+ceiling(real(ph_modes)/6.))
     read(99,*) ph_q 
   enddo
   read(99,*) ph_q 
   if (.not.v_is_zero(ph_q(:)-real(Q_DP(:)),zero_=k_iku_zero).and.&
&      .not.v_is_zero(ph_q(:)+real(Q_DP(:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_freqs_file))
   do i1=1,ph_modes,6
     read(99,*) DB_ph_freq_sq(IDB,i1:min(i1+5,ph_modes))
   enddo
   DB_ph_freq_sq(IDB,:)=(abs(DB_ph_freq_sq(IDB,:))/HA2CMm1)**2.
   close(99)
 else
   do il=1,ph_modes
     DB_ph_freq_sq(IDB,il)=max( real(ph_freqs_sq_disk(il)),0._SP)
     if (.not.abinit_DB)  DB_ph_freq_sq(IDB,il)=DB_ph_freq_sq(IDB,il)/4._SP
   enddo
 endif
 if (use_ext_ph_modes) then
   read(98, *) ws_ch(1),ws_ch(2),ph_q 
   if (.not.v_is_zero(ph_q(:)-real(Q_DP(:)),zero_=k_iku_zero).and.&
&      .not.v_is_zero(ph_q(:)+real(Q_DP(:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_modes_file))
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
 if (what=="dV")      write (dbph_name,'(a,i6.6)') 's.dbph_',iabs(IDB)
 if (what=="dV_bare") write (dbph_name,'(a,i6.6)') 's.dbph_bare_',iabs(IDB)
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
   !  write (*,'(a,i3)') 'q=',iabs(IDB)
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
     GKKP%E_kpq(1)%E(:,k2k_tab(ik),1)=real(Ek_p_q(:elph_nb)/2._DP)-GS_E_k_shift
     !
   else
     GKKP%E_kpq(1)%E(:,k2k_tab(ik),1)=0.
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
       GKKP%dVc(ib1,ib2,:,k2k_tab(ik),1)=gkkp_disk(ib1,ib2,:)*dVRY2Ha
       !
       if (what=="dV_bare") cycle
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
               GKKP%dVr(ib1,ib2,:,k2k_tab(ik),1)=GKKP%dVr(ib1,ib2,:,k2k_tab(ik),1)+&
&                2._SP*real(F1(i)*grad_at_gamma(k2k_tab(ik),ib2,ib1,ia,j)*pol_vec(il,ia,i)*conjg(pol_vec(il,ia,j))*dVRY2Ha)
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
 call q_small(DB_ph_qpt(iabs(IDB),:),small,nsmall)
 !
 ! The GS K-grid is reduced using the small group of q. 
 ! I need to expand the K-grid to define the missing elements of elph_dV
 !
 do ik=1,k%nbz
   if (k_found(ik)==0) cycle
   do i1=1,nsmall
     ikbz_rot=ikbz_is_table(ik,sop_inv(small(i1)))
     !
     if (k_found(ikbz_rot)/=0) cycle
     !
     GKKP%dVc(:,:,:,ikbz_rot,1)= GKKP%dVc(:,:,:,ik,1)
     GKKP%E_kpq(1)%E(:,ikbz_rot,1)=GKKP%E_kpq(1)%E(:,ik,1)
     !
     if (what=="dV_bare") cycle
     !
     GKKP%dVr(:,:,:,ikbz_rot,1)= GKKP%dVc(:,:,:,ik,1)
     !
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
 call io_control(ACTION=RD_CL,ID=ID)
 call io_disconnect(ID,FORCE=.TRUE.)
 !
 contains
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
end function
