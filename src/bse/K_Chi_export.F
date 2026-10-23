!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
subroutine K_Chi_export(iq,Ken,Xk,W)
 !
 ! BSEChiOut: the xc kernel that reproduces this BSE, written as an ndb.Chi.
 !
 ! The coupled BSE matrix M (its transition energies, the exchange vbar and, with
 ! BSKmod= "SEX", the screened e-h attraction W) gives the G-resolved response
 !
 !   chib(G,G';z) = c B_G^T [z - M]^-1 A_G'              (TDDFT_Chi_pair_response)
 !
 ! on the Chi vertices that K_kernel collected. With P^-1 = chib^-1 + vbar the kernel
 !
 !   "exc" : fxc = Pqp^-1 - P^-1,  Pqp = bubble of the BSE transition energies
 !   "full": fxc = chi0^-1 - P^-1, chi0 = bubble of the KS transition energies
 !   "mdr" : as "exc", with P from the exchange-free BSE matrix to first order in its
 !           off-diagonal part (Marini-Del Sole-Rubio, TDDFT_Chi_mdr_response): an
 !           approximate BSE-derived kernel, not the one that reproduces this BSE
 !   "nq"  : as "exc", fxc = Pqp^-1 T2 Pqp^-1 with the whole W part of the exchange-free
 !           matrix, diagonal included, to first order: the "mdr" route with Ed = E, whose
 !           shifted bubble is then Pqp itself. Defined in doc/greens_function_fxc.md and
 !           attributed to Sottile-Olevano-Reining (not checked against the paper)
 !
 ! makes the Casida response with the same reference energies and vbar equal to chib,
 ! and so reproduces this BSE, exciton included (tests/chi_dynamic). ndb.Chi stores
 ! chi0/Pqp as "chi0", P and fxc. Its first frequency is z = 0 exactly (static Casida),
 ! followed by the BSE frequencies (BSEChiDyn with the same BSE grid).
 !
 ! Any BSE momentum q (BSEQptR): at finite q the transitions are v(k-q) -> c(k) and
 ! the vertices rho(q+G) come from the wavefunctions (no dipoles). The anti-resonant
 ! transitions are then explicit (K_driver_init), each with its own vertex, energy and
 ! occupation, and the response runs over all rows of BS_mat (TDDFT_Chi_full_*); at
 ! q=0 they are derived from the resonant ones, a special case of the same formulas.
 ! The exchange drops G=0 (Lkind="bar", the default: vbar) or keeps it (Lkind="full",
 ! or "default" with a Coulomb cutoff at q>1), and P^-1 = chib^-1 + vbar or chib^-1 + v
 ! accordingly. The header of ndb.Chi is written once per run; each q adds its fragment.
 !
 use pars,           ONLY:SP,DP,pi,schlen,cZERO
 use units,          ONLY:HA2EV
 use frequency,      ONLY:w_samp,W_reset,W_duplicate
 use electrons,      ONLY:levels,n_spinor,n_sp_pol
 use D_lattice,      ONLY:Tel
 use R_lattice,      ONLY:bz_samp,bare_qpg,qindx_X,g_vec
 use com,            ONLY:msg,isec
 use stderr,         ONLY:intc,real2ch,STRING_match
 use parallel_m,     ONLY:ncpu
 use X_m,            ONLY:X_t,X_reset,global_gauge
 use BS,             ONLY:BS_K_coupling,BS_K_dim,BS_H_dim,BS_res_ares_n_mat,BS_n_g_fxc,BS_bands,BS_K_Chi_out,&
&                         BSE_L_kind,l_BS_ares_from_res,BS_G_inplane
 use BS_solvers,     ONLY:BS_mat,BSS_eh_E,BSS_eh_W,BSS_eh_Z,BSS_eh_table,BSS_q0,BSS_dipoles_opt,BSS_eh_f
 use Chi_m,          ONLY:Chi_ng,Chi_n_freqs,Chi_alloc,Chi_free,Chi_freqs,Chi_0,Chi_P,Chi_fxc,Chi_rcond,&
&                         Chi_QP_applied,Chi_QP_action,Chi_convention,Chi_G_mode,Chi_G_axis,Chi_G_db,&
&                         Chi_optical_q0,Chi_optical_q0_known,Chi_G0_error,Chi_rcond_floor,&
&                         Chi_dyn_V,Chi_dyn_filled,Chi_dyn_c,Chi_dyn_free
 use Chi_m,          ONLY:Chi_occupations,Chi_occ_bands,Chi_occ_nk,Chi_occ_nsp,Chi_occ_nbf,Chi_occ_nbm,&
&                         Chi_occ_Tel,Chi_occ_alloc,Chi_occ_free
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_WR_CL,OP_APP_CL,REP
 use TDDFT_Chi_dynamic_m, ONLY:TDDFT_Chi_full_response,TDDFT_Chi_full_bubble,TDDFT_Chi_kernel_from_response,&
&                              TDDFT_Chi_full_dynamic_response,TDDFT_Chi_mdr_response,&
&                              TDDFT_Chi_exchange_free_split,TDDFT_Chi_first_order_kernel
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 integer       :: iq
 type(levels)  :: Ken
 type(bz_samp) :: Xk
 type(w_samp)  :: W
 !
 ! Work Space
 !
 character(1)             :: sec
 logical                  :: l_exc,l_qp,l_full_L,l_mdr,l_nq
 character(3)             :: lab
 logical, save            :: header_done=.FALSE.
 integer                  :: n,nh,ng,nw,iw,it,ig,jg,ik,ikv,ierr,n_bad,ID,io_err
 integer,     external    :: io_Chi
 integer,     allocatable :: err_w(:)
 real(DP)                 :: herm,scale_max,rc(2)
 real(DP),    allocatable :: E_ref(:)
 complex(DP), allocatable :: d(:),H0(:,:),zz(:),chib(:,:),chi0(:,:),f(:,:),Pm(:,:)
 complex(DP), allocatable :: head(:),F0(:,:),chs(:,:),c0s(:,:),fs(:,:),Ps(:,:)
 complex(DP), allocatable :: Vf(:,:),sf(:),df(:)
 complex(DP), allocatable :: Hw(:,:),Ed(:),Wd(:),Kd(:),p0(:,:),dpm(:,:),p0s(:,:)
 logical,     allocatable :: xg(:)
 real(DP)                 :: mdr_err,mdr_nrm
 integer,     allocatable :: S(:)
 integer                  :: ns
 complex(DP)              :: rr(9),lam
 real(DP)                 :: head_err
 integer                  :: is,iws(9)
 type(X_t)                :: X_io
 type(w_samp)             :: W_io
 !
 call timing('ChiExport',OPR='start')
 !
 if (isec(2)/=0) sec="="
 if (isec(2)==0) sec="+"
 call section(sec,'Chi kernel from the BSE @q'//trim(intc(iq)))
 !
 l_mdr=trim(BS_K_Chi_out)=='mdr'
 l_nq =trim(BS_K_Chi_out)=='nq'
 l_exc=trim(BS_K_Chi_out)=='exc'.or.l_mdr.or.l_nq
 lab='MDR'
 if (l_nq) lab='NQ'
 !
 if (.not.l_BS_ares_from_res.and.iq==1) &
&  call error('[Chi/BSK] BSEChiOut at the optical q: anti-resonant blocks from the resonant ones only (no ImposeAsym)')
 if (ncpu/=1) call error('[Chi/BSK] BSEChiOut is serial (MPI); use OpenMP threads')
 if (.not.BS_K_coupling) call error('[Chi/BSK] BSEChiOut needs BSEmod= "coupling"')
 if (BS_res_ares_n_mat/=1) call error('[Chi/BSK] BSEChiOut: one BSE matrix only (found '//&
&  trim(intc(BS_res_ares_n_mat))//'. With BSEmod= "coupling" it is 1: an object file older than the modules '//&
&  'reads another variable; rebuild after make clean what=dep and make clean)')
 if (trim(global_gauge)/='length') call error('[Chi/BSK] BSEChiOut: length gauge only')
 if (.not.allocated(Chi_dyn_V)) call error('[Chi/BSK] BSEChiOut: no Chi vertices (the kernel must be built in this run)')
 if (.not.all(Chi_dyn_filled)) call error('[Chi/BSK] BSEChiOut: some transitions have no Chi vertex')
 !
 call K_components_folded_in_serial_arrays(iq)
 if (allocated(BSS_eh_W).or.allocated(BSS_eh_Z)) &
&  call error('[Chi/BSK] BSEChiOut: transition widths/Z factors are not supported')
 call K_stored_in_a_big_matrix(1,iq,"DIAGO")
 !
 n =BS_K_dim(1)
 nh=BS_H_dim
 ng=BS_n_g_fxc
 if (size(BS_mat,1)/=nh.or.nh/=2*n.or.size(Chi_dyn_V,1)/=ng) call error('[Chi/BSK] BSEChiOut: inconsistent dimensions')
 !
 ! Reference transition energies of the bubble the kernel is measured from
 !
 ! Every row A of BS_mat: vertex, s_A = sqrt(f_A) (i sqrt|f_A| for the anti-resonant
 ! rows, as in K_kernel), oscillator and reference energy
 allocate(E_ref(nh),d(ng),Vf(ng,nh),sf(nh),df(nh))
 Vf(:,:n)=Chi_dyn_V(:,:,1)
 Vf(:,n+1:)=Chi_dyn_V(:,:,2)
 do it=1,nh
   sf(it)=sqrt(cmplx(BSS_eh_f(it),kind=DP))
   df(it)=cmplx(BSS_dipoles_opt(1,it),kind=DP)
 enddo
 l_qp=.FALSE.
 if (allocated(Ken%Eo)) l_qp=maxval(abs(Ken%Eo-Ken%E))>1.E-6_SP
 do it=1,nh
   E_ref(it)=real(BSS_eh_E(it),DP)
   if (.not.l_exc.and.l_qp) then
     ! c at k, v at k-q (K_Transition_check): ikv = ik at q=0. Explicit anti-resonant
     ! rows carry their swapped bands in the table; derived ones mirror row it-n.
     if (l_BS_ares_from_res.and.it>n) then
       E_ref(it)=-E_ref(it-n)
       cycle
     endif
     ik =Xk%sstar(BSS_eh_table(it,1),1)
     ikv=Xk%sstar(qindx_X(iq,BSS_eh_table(it,1),1),1)
     E_ref(it)=real(Ken%Eo(BSS_eh_table(it,3),ik ,BSS_eh_table(it,4))-&
&                   Ken%Eo(BSS_eh_table(it,2),ikv,BSS_eh_table(it,5)),DP)
   endif
 enddo
 do ig=1,ng
   ! as the BSE exchange (1/bare_qpg**2): complex and signed with a Coulomb cutoff
   d(ig)=sqrt(4._DP*pi)/cmplx(bare_qpg(iq,ig),kind=DP)
 enddo
 !
 ! G set of the kernel: all, or the in-plane ones (BSEGinplane: the exchange acts on them only,
 ! so chib restricted to them obeys its own Dyson equation and the kernel is exact there)
 allocate(S(ng))
 ns=0
 do ig=1,ng
   if (BS_G_inplane.and.abs(g_vec(ig,3))>1.E-5_SP) cycle
   ns=ns+1
   S(ns)=ig
 enddo
 if (S(1)/=1) call error('[Chi/BSK] BSEChiOut: G=0 must be in the kernel G set')
 if (BS_G_inplane) call msg('r','[Chi/BSK] BSEGinplane: kernel on the in-plane G of the basis',(/ns,ng/))
 !
 l_full_L=STRING_match(BSE_L_kind,"full")
 call msg('r','[Chi/BSK] BSEChiOut kernel reference',trim(BS_K_Chi_out))
 call msg('r','[Chi/BSK] BSE momentum index',iq)
 if (l_BS_ares_from_res)      call msg('r','[Chi/BSK] anti-resonant rows derived from the resonant ones')
 if (.not.l_BS_ares_from_res) call msg('r','[Chi/BSK] anti-resonant rows: explicit transitions')
 if (l_full_L)      call msg('r','[Chi/BSK] exchange with G=0 (Lkind full): P^-1 = chib^-1 + v')
 if (.not.l_full_L) call msg('r','[Chi/BSK] exchange without G=0 (Lkind bar): P^-1 = chib^-1 + vbar')
 call msg('r','[Chi/BSK] QP-corrected BSE transitions',l_qp)
 call msg('r','[Chi/BSK] spinor states (two components, spin_occ = 1)',n_spinor==2)
 call msg('r','[Chi/BSK] lowest BSE transition energy (gap of the window) [eV]',real(minval(BSS_eh_E(1:n))*HA2EV,SP))
 if (l_exc.and..not.l_qp) call warning('[Chi/BSK] BSEChiOut= "'//trim(BS_K_Chi_out)//&
&  '" on KS transitions: the reference bubble is the KS one')
 !
 ! Frequencies: z=0 first, then the BSE grid
 !
 nw=W%n_freqs+1
 allocate(zz(nw),err_w(nw))
 zz(1)=(0._DP,0._DP)
 zz(2:)=cmplx(W%p(:),kind=DP)
 !
 call Chi_alloc(ng,nw)
 Chi_freqs=cmplx(zz,kind=SP)
 allocate(H0(nh,nh),head(nw))
 H0=cmplx(BS_mat,kind=DP)
 !
 ! MDR and NQ: the exchange-free matrix M_W = M - X over the G of the exchange (S, without G=0
 ! unless Lkind full), split by TDDFT_Chi_exchange_free_split into Ed and Hw (Koff): MDR moves
 ! the W-part diagonal Wd into the energies, NQ keeps Ed = E and the whole W part in Hw.
 ! Kd keeps the diagonal of Hw (MDR: zero; NQ: Wd), so that M_W = Hw + diag(Ed) can be rebuilt
 if (l_mdr.or.l_nq) then
   if (l_mdr) call msg('r','[Chi/BSK] MDR kernel: diagonal of W in the energies, off-diagonal W to first order')
   if (l_nq)  call msg('r','[Chi/BSK] NQ kernel: chi0^-1 T2 chi0^-1, all of W (diagonal included) to first order')
   allocate(Hw(nh,nh),Ed(nh),Wd(nh),Kd(nh),xg(ng))
   xg=.FALSE.
   xg(S(:ns))=.TRUE.
   if (.not.l_full_L) xg(1)=.FALSE.
   call TDDFT_Chi_exchange_free_split(trim(BS_K_Chi_out),H0,Vf,sf,d,xg,Chi_dyn_c,E_ref,Hw,Ed,Wd,ierr)
   if (ierr/=0) call error('[Chi/BSK] BSEChiOut: exchange-free split failed with code '//trim(intc(ierr)))
   call msg('r','[Chi/BSK] diagonal of the W part (resonant rows) [eV]: min, mean, max',&
&           real((/minval(real(Wd(:n))),sum(real(Wd(:n)))/real(n,DP),maxval(real(Wd(:n)))/)*HA2EV,SP))
   do it=1,nh
     Kd(it)=Hw(it,it)
   enddo
   deallocate(xg)
 endif
 err_w=0
 head=(0._DP,0._DP)
 !
 !$omp parallel default(shared) private(iw,chib,chi0,f,Pm,rc,ierr,chs,c0s,fs,Ps)
 allocate(chib(ng,ng),chi0(ng,ng),f(ng,ng),Pm(ng,ng))
 allocate(chs(ns,ns),c0s(ns,ns),fs(ns,ns),Ps(ns,ns))
 !$omp do schedule(dynamic)
 do iw=1,nw
   call TDDFT_Chi_full_response(zz(iw),H0,Vf,sf,Chi_dyn_c,chib,ierr)
   if (ierr==0) then
     head(iw)=chib(1,1)
     call TDDFT_Chi_full_bubble(zz(iw),E_ref,Vf,sf,Chi_dyn_c,chi0)
     ! kernel on the G set S (all G unless BSEGinplane), zero outside it
     chs=chib(S(:ns),S(:ns))
     c0s=chi0(S(:ns),S(:ns))
     if (l_mdr.or.l_nq) then
       ! NQ: with Ed = E the shifted bubble is chi0 (checked below), dP1 is T2, and chi0 is
       ! passed in both slots of TDDFT_Chi_kernel_mdr: fxc = chi0^-1 T2 chi0^-1
       call TDDFT_Chi_first_order_kernel(trim(BS_K_Chi_out),zz(iw),Ed,Hw,Vf,sf,Chi_dyn_c,S(:ns),c0s,&
&                                        d(S(:ns)),fs,Ps,rc,ierr)
     else
       call TDDFT_Chi_kernel_from_response(c0s,chs,d(S(:ns)),.TRUE.,fs,Ps,rc,ierr,exch_head=l_full_L)
     endif
     f=(0._DP,0._DP)
     Pm=(0._DP,0._DP)
     f(S(:ns),S(:ns))=fs
     Pm(S(:ns),S(:ns))=Ps
   endif
   err_w(iw)=ierr
   if (ierr==0) then
     Chi_0(:,:,iw)=cmplx(chi0,kind=SP)
     Chi_P(:,:,iw)=cmplx(Pm,kind=SP)
     Chi_fxc(:,:,iw)=cmplx(f,kind=SP)
     Chi_rcond(:,iw)=real(rc,SP)
   endif
 enddo
 !$omp end do
 deallocate(chib,chi0,f,Pm,chs,c0s,fs,Ps)
 !$omp end parallel
 !
 n_bad=count(err_w/=0)
 if (n_bad>0) then
   do iw=1,nw
     if (err_w(iw)/=0) exit
   enddo
   call error('[Chi/BSK] BSEChiOut failed at '//trim(intc(n_bad))//' frequencies; first at '//&
&    trim(real2ch(real(real(zz(iw))*HA2EV,SP)))//' eV with code '//trim(intc(err_w(iw)))//' (31/32: reduce BSENGfxc)')
 endif
 call msg('r','[Chi/BSK] minimum rcond (reference bubble, BSE response)',(/minval(Chi_rcond(1,:)),minval(Chi_rcond(2,:))/))
 !
 ! Head check, valid at any q: the G=0 element of chib (our vertices) must be Yambo's own
 ! coupled response, built from its oscillators BSS_dipoles_opt on every row of BS_mat:
 ! the fxc=0 limit of the Casida solver, scaled by c.
 allocate(F0(ng,ng))
 F0=(0._DP,0._DP)
 do is=1,9
   iws(is)=max(2,min(nw,2+((nw-2)*(is-1))/8))
 enddo
 rr=(0._DP,0._DP)
 do is=1,9
   call TDDFT_Chi_full_dynamic_response(zz(iws(is)),H0,F0,Vf,sf,Chi_dyn_c,df,rr(is),ierr)
   if (ierr/=0) rr(is)=(0._DP,0._DP)
 enddo
 ! best complex scale lam (exactly c when the conventions agree), then the residual
 lam=sum(conjg(rr)*head(iws))/max(sum(abs(rr)**2),tiny(1._DP))
 head_err=sqrt(sum(abs(head(iws)-lam*rr)**2)/max(sum(abs(head(iws))**2),tiny(1._DP)))
 call msg('r','[Chi/BSK] head of the exported response vs the BSE response: residual, |scale/c|, arg(scale)',&
&         (/real(head_err,SP),real(abs(lam)/Chi_dyn_c,SP),real(atan2(aimag(lam),real(lam)),SP)/))
 if (head_err>1.E-4_DP.or.abs(abs(lam)/Chi_dyn_c-1._DP)>1.E-4_DP) &
&  call warning('[Chi/BSK] the exported G=0 response differs from the BSE one: '//&
&    'vertex or anti-resonant convention mismatch at this q, the kernel is not the BSE one')
 deallocate(F0)
 !
 ! NQ: the bubble of the MDR route with Ed = E must be the reference bubble
 if (l_nq) then
   allocate(chi0(ng,ng),p0(ng,ng),dpm(ng,ng))
   call TDDFT_Chi_full_bubble(zz(iws(5)),E_ref,Vf,sf,Chi_dyn_c,chi0)
   call TDDFT_Chi_mdr_response(zz(iws(5)),Ed,Hw,Vf,sf,Chi_dyn_c,p0,dpm)
   call msg('r','[Chi/BSK] NQ: bubble with Ed = E vs the reference bubble (max relative difference)',&
&           real(maxval(abs(p0-chi0))/max(maxval(abs(chi0)),tiny(1._DP)),SP))
   deallocate(chi0,p0,dpm)
 endif
 !
 ! MDR and NQ: the exchange-free matrix must give the exact P of this BSE (P^-1 = chib^-1 + vbar)
 if (l_mdr.or.l_nq) then
   allocate(chib(ng,ng),chi0(ng,ng),chs(ns,ns),c0s(ns,ns),fs(ns,ns),Ps(ns,ns),p0s(ns,ns))
   do it=1,nh
     Hw(it,it)=Kd(it)+Ed(it)
   enddo
   mdr_err=0._DP
   mdr_nrm=0._DP
   do is=1,9,4
     call TDDFT_Chi_full_response(zz(iws(is)),H0,Vf,sf,Chi_dyn_c,chib,ierr)
     if (ierr/=0) cycle
     call TDDFT_Chi_full_bubble(zz(iws(is)),E_ref,Vf,sf,Chi_dyn_c,chi0)
     chs=chib(S(:ns),S(:ns))
     c0s=chi0(S(:ns),S(:ns))
     call TDDFT_Chi_kernel_from_response(c0s,chs,d(S(:ns)),.TRUE.,fs,Ps,rc,ierr,exch_head=l_full_L)
     if (ierr/=0) cycle
     call TDDFT_Chi_full_response(zz(iws(is)),Hw,Vf,sf,Chi_dyn_c,chib,ierr)
     if (ierr/=0) cycle
     p0s=chib(S(:ns),S(:ns))
     do jg=1,ns
       do ig=1,ns
         mdr_err=max(mdr_err,abs((p0s(ig,jg)-Ps(ig,jg))*d(S(ig))*d(S(jg))))
         mdr_nrm=max(mdr_nrm,abs(Ps(ig,jg)*d(S(ig))*d(S(jg))))
       enddo
     enddo
   enddo
   do it=1,nh
     Hw(it,it)=Kd(it)
   enddo
   call msg('r','[Chi/BSK] '//trim(lab)//': P of the exchange-free matrix vs the exact P (max relative difference)',&
&           real(mdr_err/max(mdr_nrm,tiny(1._DP)),SP))
   if (mdr_err>1.E-4_DP*mdr_nrm) call warning('[Chi/BSK] '//trim(lab)//&
&    ': the exchange removed from the BSE matrix is not its exchange')
   deallocate(chib,chi0,chs,c0s,fs,Ps,p0s,Hw,Ed,Wd,Kd)
 endif
 if (minval(Chi_rcond)<Chi_rcond_floor) &
&  call error('[Chi/BSK] BSEChiOut: ill-conditioned response; reduce BSENGexx/BSENGfxc for this band window')
 !
 ! The static kernel must be Hermitian (on the scale |q+G| fxc |q+G'| where all blocks are O(1))
 !
 herm=0._DP
 scale_max=0._DP
 do jg=1,ng
   do ig=1,ng
     herm=max(herm,abs(Chi_fxc(ig,jg,1)-conjg(Chi_fxc(jg,ig,1)))/abs(d(ig)*d(jg)))
     scale_max=max(scale_max,abs(Chi_fxc(ig,jg,1))/abs(d(ig)*d(jg)))
   enddo
 enddo
 call msg('r','[Chi/BSK] static fxc head [Ha] and relative anti-Hermitian part',&
&         (/real(Chi_fxc(1,1,1)),real(herm/max(scale_max,tiny(1._DP)),SP)/))
 !
 ! ndb.Chi in this job's directory
 !
 Chi_ng=ng
 Chi_n_freqs=nw
 Chi_QP_applied=l_qp
 Chi_G_db=''
 if (BS_G_inplane) Chi_G_db='BSEGinplane: exchange and fxc on the in-plane G (G_z=0) only'
 Chi_G_axis='REAL'
 Chi_G0_error=-1._SP
 Chi_optical_q0=0._SP
 Chi_optical_q0_known=iq==1
 if (iq==1) Chi_optical_q0=BSS_q0
 if (l_nq) then
   Chi_G_mode='BSE-EXC'
   Chi_QP_action='NQ: fxc = chi0^-1 T2 chi0^-1, all of W to first order; chi0 = bubble of the (QP) BSE energies'
 else if (l_mdr) then
   Chi_G_mode='BSE-EXC'
   Chi_QP_action='MDR: P to first order in the off-diagonal W; chi0 = bubble of the (QP) BSE energies'
 else if (l_exc) then
   Chi_G_mode='BSE-EXC'
   Chi_QP_action='P from the coupled BSE; chi0 = bubble of its (QP) transition energies'
 else
   Chi_G_mode='BSE-KS'
   Chi_QP_action='P from the coupled BSE; chi0 = KS bubble'
 endif
 Chi_convention='Bare vertices; P^-1 = chib^-1 + vbar from the BSE; fxc=chi0^-1-P^-1, unsymmetrized [Ha]; w1 = 0'
 call X_reset(X_io)
 X_io%ib=BS_bands
 X_io%ng=ng
 call W_reset(W_io)
 call W_duplicate(W,W_io)
 ! One header per run (it holds |q+G| of every q); each q appends its fragment. Run q=1
 ! first in a multi-q job (BSEQptR 1 | n) to keep the optical q0 of the header.
 if (.not.header_done) then
   ! Occupations of these BSE levels, checked by the Casida run (BSKmod="CHI")
   Chi_occ_bands=BS_bands
   Chi_occ_nk=Ken%nk
   Chi_occ_nsp=n_sp_pol
   Chi_occ_nbf=minval(Ken%nbf(:n_sp_pol))
   Chi_occ_nbm=maxval(Ken%nbm(:n_sp_pol))
   Chi_occ_Tel=Tel
   call Chi_occ_alloc()
   Chi_occupations=Ken%f(BS_bands(1):BS_bands(2),:,:)
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
   io_err=io_Chi(X_io,W_io,ID)
   if (io_err<0) call error('[Chi/BSK] cannot write the ndb.Chi header')
   call Chi_occ_free(.TRUE.)
   header_done=.TRUE.
 endif
 call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/2*iq,2*iq+1/),ID=ID)
 io_err=io_Chi(X_io,W_io,ID)
 if (io_err<0) call error('[Chi/BSK] cannot write the ndb.Chi q fragment')
 call msg('r','[Chi/BSK] ndb.Chi written: G vectors, frequencies (z=0 first)',(/ng,nw/))
 !
 call W_reset(W_io)
 call Chi_free()
 call Chi_dyn_free()
 deallocate(E_ref,d,Vf,sf,df,zz,err_w,H0,head,S)
 !
 call timing('ChiExport',OPR='stop')
 !
end subroutine K_Chi_export
