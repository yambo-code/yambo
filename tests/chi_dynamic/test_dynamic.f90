program test_chi_dynamic
  ! Checks TDDFT_Chi_dynamic_m against two independent references:
  !  1. the pair-space Dyson equation solved in G space, chi=[chi0^-1-fxc]^-1,
  !     for an arbitrary complex, non-Hermitian fxc(z) (coupling and TDA);
  !  2. Yambo's static machinery: the matrix K_stored_in_a_big_matrix assembles
  !     and the Lorentzian sums of K_diago_response_functions, for a static
  !     Hermitian fxc that is real in real space.
  use pars, only: DP
  use TDDFT_Chi_dynamic_m, only: TDDFT_Chi_dynamic_kernel,TDDFT_Chi_dynamic_response,&
&   TDDFT_Chi_pair_response,TDDFT_Chi_pair_bubble,TDDFT_Chi_kernel_from_response,&
&   TDDFT_Chi_full_response,TDDFT_Chi_full_bubble,TDDFT_Chi_full_dynamic_response,&
&   TDDFT_Chi_mdr_response,TDDFT_Chi_kernel_mdr
  implicit none
  integer, parameter :: m=3,ng=2*m+1,nt=5
  complex(DP), parameter :: ci=(0._DP,1._DP),zero=(0._DP,0._DP)
  integer :: minus(ng),ig,jg,it,jt,ierr,iz,failures
  real(DP) :: delta(nt),s(nt),worst
  complex(DP) :: rho_r(ng,nt),rho_c(ng,nt),fxc(ng,ng),z(3),resp,ref
  complex(DP) :: a(nt),b(nt),H0(nt,nt),H0c(2*nt,2*nt)
  complex(DP) :: chi0(ng,ng)

  failures=0
  z=[cmplx(0.3_DP,0.05_DP,kind=DP),cmplx(1.7_DP,0.1_DP,kind=DP),cmplx(-0.9_DP,0.02_DP,kind=DP)]
  call random_seed(put=[(17*ig+3,ig=1,64)])
  ! G list: 0, then pairs (+g,-g) so that minus(ig) is the index of -G.
  minus(1)=1
  do ig=1,m
    minus(2*ig)=2*ig+1
    minus(2*ig+1)=2*ig
  enddo
  do it=1,nt
    delta(it)=0.8_DP+0.35_DP*it
    s(it)=1._DP
  enddo

  ! ---------------------------------------------------------------------
  ! 1. Exact pair-space identity for a generic complex fxc(z)
  ! ---------------------------------------------------------------------
  call random_vertices(rho_r)
  call random_vertices(rho_c)
  do iz=1,size(z)
    call random_matrix(fxc)
    ! Yambo form: b = rho_r(0), a = conj(b); the de-excitation head is conj(b).
    b=rho_r(1,:)
    a=conjg(b)
    rho_c(1,:)=a
    ! Head of the full response, coupling: [chi0^-1 - fxc]^-1 (0,0)
    chi0=build_chi0(z(iz))
    ref=dyson_head(chi0,fxc)
    H0c=zero
    do it=1,nt
      H0c(it,it)=delta(it)
      H0c(nt+it,nt+it)=-delta(it)
    enddo
    call TDDFT_Chi_dynamic_response(z(iz),H0c,fxc,rho_r,rho_c,s,1._DP,a,b,.TRUE.,resp,ierr)
    call check('coupling = G-space Dyson (generic complex fxc)',ierr,resp,ref)
    ! TDA: the same identity without the resonant/anti-resonant mixing
    ref=tda_head(z(iz),fxc)
    H0=zero
    do it=1,nt
      H0(it,it)=delta(it)
    enddo
    call TDDFT_Chi_dynamic_response(z(iz),H0,fxc,rho_r,rho_c,s,1._DP,a,b,.FALSE.,resp,ierr)
    call check('TDA = block-diagonal pair Dyson (generic complex fxc)',ierr,resp,ref)
  enddo

  ! Exchange-like static H0 blocks and s/=1, c/=1 still follow the pair Dyson form
  call static_structure_checks()

  ! ---------------------------------------------------------------------
  ! 3. Error reporting
  ! ---------------------------------------------------------------------
  call TDDFT_Chi_dynamic_response(z(1),H0c,fxc,rho_r,rho_c,s,1._DP,a,b,.FALSE.,resp,ierr)
  if (ierr/=20) then
    print *,'FAIL: wrong H0 size not reported, ierr=',ierr
    failures=failures+1
  else
    print *,'PASS: wrong H0 size reported'
  endif
  call TDDFT_Chi_dynamic_response(z(1),H0,fxc(:ng-1,:ng-1),rho_r,rho_c,s,1._DP,a,b,.FALSE.,resp,ierr)
  if (ierr/=12) then
    print *,'FAIL: vertex/fxc size mismatch not reported, ierr=',ierr
    failures=failures+1
  else
    print *,'PASS: vertex/fxc size mismatch reported'
  endif

  call threaded_check()

  call export_chain_checks()

  call full_space_checks()

  call mdr_checks()

  if (failures>0) then
    print *,failures,' check(s) failed'
    error stop 1
  endif
  print *,'all dynamic Casida checks passed'

contains

  subroutine random_vertices(v)
    complex(DP), intent(out) :: v(:,:)
    real(DP) :: re(size(v,1),size(v,2)),im(size(v,1),size(v,2))
    call random_number(re)
    call random_number(im)
    v=cmplx(re-0.5_DP,im-0.5_DP,kind=DP)
  end subroutine

  subroutine random_matrix(f)
    complex(DP), intent(out) :: f(:,:)
    real(DP) :: re(size(f,1),size(f,2)),im(size(f,1),size(f,2))
    call random_number(re)
    call random_number(im)
    f=cmplx(re-0.5_DP,im-0.5_DP,kind=DP)
  end subroutine

  function build_chi0(zz) result(c0)
    ! chi0 = sum_t rho_r rho_r^H/(z-D) - rho_c rho_c^H/(z+D)
    complex(DP), intent(in) :: zz
    complex(DP) :: c0(ng,ng)
    integer :: i,j,t
    c0=zero
    do t=1,nt
      do j=1,ng
        do i=1,ng
          c0(i,j)=c0(i,j)+rho_r(i,t)*conjg(rho_r(j,t))/(zz-delta(t))&
&                        -rho_c(i,t)*conjg(rho_c(j,t))/(zz+delta(t))
        enddo
      enddo
    enddo
  end function

  function dyson_head(c0,f) result(h)
    complex(DP), intent(in) :: c0(:,:),f(:,:)
    complex(DP) :: h,work(ng,ng),lw(4*ng)
    integer :: ipiv(ng),info
    work=c0
    call zgetrf(ng,ng,work,ng,ipiv,info)
    call zgetri(ng,work,ng,ipiv,lw,size(lw),info)
    work=work-f
    call zgetrf(ng,ng,work,ng,ipiv,info)
    call zgetri(ng,work,ng,ipiv,lw,size(lw),info)
    h=work(1,1)
  end function

  function tda_head(zz,f) result(h)
    ! r^T (z-D-Krr)^-1 conj(r) - rc^T (z+D+Kcc)^-1 conj(rc), K_xy = rho_x^H f rho_y
    complex(DP), intent(in) :: zz,f(:,:)
    complex(DP) :: h,M(nt,nt),x(nt)
    integer :: ipiv(nt),info,t
    M=-matmul(conjg(transpose(rho_r)),matmul(f,rho_r))
    do t=1,nt
      M(t,t)=M(t,t)+zz-delta(t)
    enddo
    x=conjg(rho_r(1,:))
    call zgesv(nt,1,M,nt,ipiv,x,nt,info)
    h=sum(rho_r(1,:)*x)
    M=matmul(conjg(transpose(rho_c)),matmul(f,rho_c))
    do t=1,nt
      M(t,t)=M(t,t)+zz+delta(t)
    enddo
    x=conjg(rho_c(1,:))
    call zgesv(nt,1,M,nt,ipiv,x,nt,info)
    h=h-sum(rho_c(1,:)*x)
  end function

  subroutine static_structure_checks()
    ! A static fxc that is Hermitian and real in real space, f(-G,-G')=conj f(G,G'),
    ! and de-excitation vertices rho_c(G)=conj(rho_r(-G)): then Kf_cc=conj(Kf_rr)
    ! and Kf_rc is symmetric, which is what K_stored_in_a_big_matrix assumes.
    complex(DP) :: f(ng,ng),fs(ng,ng),Kx(nt,nt),Cx(nt,nt),Krr(nt,nt),Krc(nt,nt),Kcc(nt,nt),Kcr(nt,nt)
    complex(DP) :: R(nt,nt),C(nt,nt),BS_mat(2*nt,2*nt),H(nt,nt),zz,refs,A2(2*nt),B2(2*nt)
    complex(DP) :: VR(2*nt,2*nt),VL(2*nt,2*nt),E(2*nt),work(8*nt),x(2*nt),ov(2*nt,2*nt)
    real(DP) :: sq(nt),Ereal(nt),rwork(4*nt),c_fac
    integer :: i,j,t,info,ipiv(2*nt),lam
    call random_matrix(f)
    ! Hermitian and f(-G,-G')=conj(f(G,G'))
    f=0.5_DP*(f+conjg(transpose(f)))
    do j=1,ng
      do i=1,ng
        fs(i,j)=0.5_DP*(f(i,j)+conjg(f(minus(i),minus(j))))
      enddo
    enddo
    f=fs
    call random_vertices(rho_r)
    do t=1,nt
      do i=1,ng
        rho_c(i,t)=conjg(rho_r(minus(i),t))
      enddo
    enddo
    do t=1,nt
      sq(t)=sqrt(0.5_DP+0.25_DP*t)
    enddo
    c_fac=0.37_DP
    ! Static exchange-like part: Hermitian resonant block, symmetric coupling block
    call random_matrix(Kx)
    Kx=0.5_DP*(Kx+conjg(transpose(Kx)))
    call random_matrix(Cx)
    Cx=0.5_DP*(Cx+transpose(Cx))
    call TDDFT_Chi_dynamic_kernel(f,rho_r,rho_r,sq,c_fac,Krr,info)
    call TDDFT_Chi_dynamic_kernel(f,rho_r,rho_c,sq,c_fac,Krc,info)
    call TDDFT_Chi_dynamic_kernel(f,rho_c,rho_c,sq,c_fac,Kcc,info)
    call TDDFT_Chi_dynamic_kernel(f,rho_c,rho_r,sq,c_fac,Kcr,info)
    worst=max(maxval(abs(Kcc-conjg(Krr))),maxval(abs(Krc-transpose(Krc))),maxval(abs(Kcr-conjg(transpose(Krc)))))
    if (worst>1.E-12_DP) then
      print *,'FAIL: static fxc symmetries of the projected blocks',worst
      failures=failures+1
    else
      print *,'PASS: static fxc gives Kcc=conj(Krr), symmetric Krc, Kcr=Krc^H'
    endif
    ! Yambo residual vectors: a=d sqrt(f), b=conj(d) sqrt(f)
    b=cmplx([(0.3_DP*t,t=1,nt)],[(0.2_DP-0.1_DP*t,t=1,nt)],kind=DP)
    a=b
    b=conjg(a)
    ! f=sq**2; residual vectors carry sqrt(f)
    a=a*sq
    b=b*sq
    zz=cmplx(1.9_DP,0.07_DP,kind=DP)
    ! --- TDA: K_diago_response_functions on the eigenpairs of R=H0+Krr
    R=Kx
    do t=1,nt
      R(t,t)=R(t,t)+delta(t)
    enddo
    H=R+Krr
    call zheev('V','U',nt,H,nt,Ereal,work,size(work),rwork,info)
    refs=zero
    do lam=1,nt
      ! BS_R_right = sum_j conjg(d_j) sqrt(f_j) V(j,lam) = b^T V ; R_left = conjg(R_right)
      refs=refs+abs(sum(b*H(:,lam)))**2/(zz-Ereal(lam))-abs(sum(b*H(:,lam)))**2/(zz+Ereal(lam))
    enddo
    call TDDFT_Chi_dynamic_response(zz,R,f,rho_r,rho_c,sq,c_fac,a,b,.FALSE.,resp,ierr)
    call check('TDA static limit = K_diago_response_functions (with mirror)',ierr,resp,refs)
    ! --- Coupling: BS_mat as K_stored_in_a_big_matrix fills it from R and C blocks
    R=R+Krr
    C=ci*(Cx+Krc)
    BS_mat=zero
    do i=1,nt
      do j=i,nt
        BS_mat(i,j)=R(i,j)
        BS_mat(j,i)=conjg(R(i,j))
        BS_mat(i+nt,j+nt)=-conjg(R(i,j))
        BS_mat(j+nt,i+nt)=-R(i,j)
        BS_mat(i,j+nt)=C(i,j)
        BS_mat(j+nt,i)=-conjg(C(i,j))
        BS_mat(j,i+nt)=C(i,j)
        BS_mat(i+nt,j)=-conjg(C(i,j))
      enddo
    enddo
    ! Lorentzian sum over the non-Hermitian eigenpairs, as K_diago_left/right_residuals
    VR=BS_mat
    call zgeev('V','V',2*nt,VR,2*nt,E,VL,2*nt,H0c,2*nt,work,size(work),rwork,info)
    VR=H0c
    ov=matmul(conjg(transpose(VL)),VR)
    call inv(ov)
    A2(:nt)=a
    A2(nt+1:)=ci*b
    B2(:nt)=b
    B2(nt+1:)=ci*a
    x=matmul(conjg(transpose(VL)),A2)
    x=matmul(ov,x)
    refs=zero
    do lam=1,2*nt
      refs=refs+sum(B2*VR(:,lam))*x(lam)/(zz-E(lam))
    enddo
    ! Dynamic solver with the static part (energies, exchange) in H0 and fxc apart
    H0c=zero
    R=Kx
    do t=1,nt
      R(t,t)=R(t,t)+delta(t)
    enddo
    C=ci*Cx
    do i=1,nt
      do j=i,nt
        H0c(i,j)=R(i,j)
        H0c(j,i)=conjg(R(i,j))
        H0c(i+nt,j+nt)=-conjg(R(i,j))
        H0c(j+nt,i+nt)=-R(i,j)
        H0c(i,j+nt)=C(i,j)
        H0c(j+nt,i)=-conjg(C(i,j))
        H0c(j,i+nt)=C(i,j)
        H0c(i+nt,j)=-conjg(C(i,j))
      enddo
    enddo
    call TDDFT_Chi_dynamic_response(zz,H0c,f,rho_r,rho_c,sq,c_fac,a,b,.TRUE.,resp,ierr)
    call check('coupling static limit = K_stored matrix + non-Hermitian Lorentzian sum',ierr,resp,refs)
    ! and directly against the resolvent of the static BS_mat
    VR=-BS_mat
    do i=1,2*nt
      VR(i,i)=VR(i,i)+zz
    enddo
    x=A2
    call zgesv(2*nt,1,VR,2*nt,ipiv,x,2*nt,info)
    call check('coupling static limit = B^T (z-BS_mat)^-1 A',ierr,resp,sum(B2*x))
  end subroutine

  subroutine threaded_check()
    ! Same frequencies solved serially and from OpenMP threads (K_Chi_dynamic loop)
    integer, parameter :: nz=16
    complex(DP) :: zs(nz),r_serial(nz),r_thread(nz),Hc(2*nt,2*nt),f(ng,ng,nz)
    integer :: i,e(nz),t
    Hc=zero
    do t=1,nt
      Hc(t,t)=delta(t)
      Hc(nt+t,nt+t)=-delta(t)
    enddo
    do i=1,nz
      zs(i)=cmplx(0.25_DP*i,0.05_DP,kind=DP)
      call random_matrix(f(:,:,i))
      call TDDFT_Chi_dynamic_response(zs(i),Hc,f(:,:,i),rho_r,rho_c,s,1._DP,a,b,.TRUE.,r_serial(i),e(i))
    enddo
    !$omp parallel do schedule(dynamic)
    do i=1,nz
      call TDDFT_Chi_dynamic_response(zs(i),Hc,f(:,:,i),rho_r,rho_c,s,1._DP,a,b,.TRUE.,r_thread(i),e(i))
    enddo
    !$omp end parallel do
    if (any(e/=0).or.maxval(abs(r_thread-r_serial))>1.E-12_DP*maxval(abs(r_serial))) then
      print *,'FAIL: threaded frequency loop differs from the serial one'
      failures=failures+1
    else
      print *,'PASS: threaded frequency loop reproduces the serial results'
    endif
  end subroutine

  subroutine export_chain_checks()
    ! BSEChiOut chain: a BSE-like matrix M (QP energies, exchange with vbar, a random
    ! "W" part) -> its G-resolved response chib -> fxc = chi_ref^-1 - chib^-1 - vbar ->
    ! Casida with the reference energies, the exchange and fxc must return chib (head),
    ! for the QP ("exc") and the KS ("full") reference.
    complex(DP) :: M(2*nt,2*nt),M0(2*nt,2*nt),H0x(2*nt,2*nt),Kb(nt,nt),Rw(nt,nt),Cw(nt,nt)
    complex(DP) :: chib(ng,ng),chi0(ng,ng),bub(ng,ng),f(ng,ng),Pm(ng,ng),vb(ng,ng),zz,work(ng,ng)
    complex(DP) :: Krr(nt,nt),Krc(nt,nt),Kcr(nt,nt),Kcc(nt,nt)
    real(DP) :: sq(nt),dq(nt),dks(nt),rc(2),c_fac,worst
    complex(DP) :: d(ng)
    integer :: t,i,iz,info,ipv(ng),iref,ns,isub(ng)
    complex(DP), allocatable :: cs(:,:),bs(:,:),fs(:,:),ps(:,:),ws(:,:)
    complex(DP) :: lw(4*ng),zs(3)
    call random_vertices(rho_r)
    call random_vertices(rho_c)
    rho_c(1,:)=conjg(rho_r(1,:))
    c_fac=0.37_DP
    do t=1,nt
      sq(t)=sqrt(0.6_DP+0.2_DP*t)
      dks(t)=0.8_DP+0.35_DP*t
      dq(t)=dks(t)+0.45_DP
    enddo
    call random_matrix(Rw)
    Rw=-0.2_DP*(Rw+conjg(transpose(Rw)))
    call random_matrix(Cw)
    Cw=0.1_DP*(Cw+transpose(Cw))
    zs=[cmplx(0._DP,0._DP,kind=DP),cmplx(1.3_DP,0.05_DP,kind=DP),cmplx(2.9_DP,0.1_DP,kind=DP)]
    do iref=1,5
      ! iref 1: QP reference, 2: KS reference, 3: QP reference with a cut Coulomb whose
      ! v_cut(G) < 0 for some G (imaginary d = sqrt(4 pi)/bare_qpg, as in Yambo),
      ! 4: finite q with the G=0 exchange kept (Lkind="full"): v instead of vbar
      ! 5: BSEGinplane: exchange masked on some G (3 and 5); kernel from the block of the
      !    other G, embedded with zeros (the masked block closes the Dyson equation)
      d(1)=40._DP
      if (iref==4) d(1)=3._DP
      do i=2,ng
        d(i)=1.5_DP/sqrt(real(i,DP))
      enddo
      if (iref==3) then
        d(3)=ci*d(3)
        d(5)=ci*d(5)
      endif
      vb=zero
      do i=2,ng
        vb(i,i)=d(i)**2
      enddo
      if (iref==4) vb(1,1)=d(1)**2
      ns=0
      do i=1,ng
        if (iref==5.and.(i==3.or.i==5)) then
          vb(i,i)=zero
          cycle
        endif
        ns=ns+1
        isub(ns)=i
      enddo
      call TDDFT_Chi_dynamic_kernel(vb,rho_r,rho_r,sq,c_fac,Krr,info)
      call TDDFT_Chi_dynamic_kernel(vb,rho_r,rho_c,sq,c_fac,Krc,info)
      call TDDFT_Chi_dynamic_kernel(vb,rho_c,rho_r,sq,c_fac,Kcr,info)
      call TDDFT_Chi_dynamic_kernel(vb,rho_c,rho_c,sq,c_fac,Kcc,info)
      ! BSE matrix: QP energies, exchange, W part; reference energies for the kernel
      M=zero
      M(:nt,:nt)=Krr+Rw
      M(:nt,nt+1:)=ci*(Krc+Cw)
      M(nt+1:,:nt)=ci*(Kcr+conjg(transpose(Cw)))
      M(nt+1:,nt+1:)=-(Kcc+conjg(Rw))
      do t=1,nt
        M(t,t)=M(t,t)+dq(t)
        M(nt+t,nt+t)=M(nt+t,nt+t)-dq(t)
      enddo
      do iz=1,size(zs)
        zz=zs(iz)
        call TDDFT_Chi_pair_response(zz,M,rho_r,rho_c,sq,c_fac,chib,info)
        if (info/=0) then
          print *,'FAIL: pair response error',info
          failures=failures+1
          cycle
        endif
        if (iref/=2) then
          call TDDFT_Chi_pair_bubble(zz,dq,rho_r,rho_c,sq,c_fac,chi0)
        else
          call TDDFT_Chi_pair_bubble(zz,dks,rho_r,rho_c,sq,c_fac,chi0)
        endif
        if (iref/=5) then
          call TDDFT_Chi_kernel_from_response(chi0,chib,d,.TRUE.,f,Pm,rc,info,exch_head=(iref==4))
        else
          allocate(cs(ns,ns),bs(ns,ns),fs(ns,ns),ps(ns,ns),ws(ns,ns))
          cs=chi0(isub(:ns),isub(:ns))
          bs=chib(isub(:ns),isub(:ns))
          call TDDFT_Chi_kernel_from_response(cs,bs,d(isub(:ns)),.TRUE.,fs,ps,rc,info)
          f=zero
          Pm=zero
          f(isub(:ns),isub(:ns))=fs
          Pm(isub(:ns),isub(:ns))=ps
          ! P^-1 = chib^-1 + vbar on the kept block
          ws=ps
          call inv(ws)
          ws=ws-vb(isub(:ns),isub(:ns))
          call inv(ws)
          worst=maxval(abs(ws-bs))/maxval(abs(bs))
          if (worst>1.E-9_DP) then
            print *,'FAIL: in-plane block: P^-1 /= chib^-1 + vbar',worst
            failures=failures+1
          endif
          deallocate(cs,bs,fs,ps,ws)
        endif
        if (info/=0) then
          print *,'FAIL: kernel_from_response error',info
          failures=failures+1
          cycle
        endif
        ! P^-1 = chib^-1 + vbar (iref 5: checked on the kept block above)
        if (iref==5) goto 10
        work=Pm
        call zgetrf(ng,ng,work,ng,ipv,info)
        call zgetri(ng,work,ng,ipv,lw,size(lw),info)
        work=work-vb
        call zgetrf(ng,ng,work,ng,ipv,info)
        call zgetri(ng,work,ng,ipv,lw,size(lw),info)
        worst=maxval(abs(work-chib))/maxval(abs(chib))
        if (iz==1.and.iref==1) then
          ! the response of M without its kernel is the bubble
          M0=zero
          do t=1,nt
            M0(t,t)=dq(t)
            M0(nt+t,nt+t)=-dq(t)
          enddo
          call TDDFT_Chi_pair_response(cmplx(0.7_DP,0.02_DP,kind=DP),M0,rho_r,rho_c,sq,c_fac,chib,info)
          call TDDFT_Chi_pair_bubble(cmplx(0.7_DP,0.02_DP,kind=DP),dq,rho_r,rho_c,sq,c_fac,bub)
          call check('pair response of diag(E,-E) = pair bubble',info,bub(2,3),chib(2,3))
          call TDDFT_Chi_pair_response(zz,M,rho_r,rho_c,sq,c_fac,chib,info)
        endif
        if (worst>1.E-9_DP) then
          print *,'FAIL: exported P does not satisfy P^-1 = chib^-1 + vbar',worst
          failures=failures+1
        endif
10      continue
        ! Casida: reference energies + exchange, fxc from the export
        H0x=zero
        H0x(:nt,:nt)=Krr
        H0x(:nt,nt+1:)=ci*Krc
        H0x(nt+1:,:nt)=ci*Kcr
        H0x(nt+1:,nt+1:)=-Kcc
        do t=1,nt
          if (iref/=2) then
            H0x(t,t)=H0x(t,t)+dq(t); H0x(nt+t,nt+t)=H0x(nt+t,nt+t)-dq(t)
          else
            H0x(t,t)=H0x(t,t)+dks(t); H0x(nt+t,nt+t)=H0x(nt+t,nt+t)-dks(t)
          endif
        enddo
        b=sq*rho_r(1,:)
        a=sq*conjg(rho_r(1,:))
        call TDDFT_Chi_dynamic_response(zz,H0x,f,rho_r,rho_c,sq,c_fac,a,b,.TRUE.,resp,info)
        if (iref==1) call check('export exc: Casida(QP, v, fxc) = BSE response head',info,c_fac*resp,chib(1,1))
        if (iref==2) call check('export full: Casida(KS, v, fxc) = BSE response head',info,c_fac*resp,chib(1,1))
        if (iref==3) call check('export exc, cut Coulomb (v<0 at some G): Casida = BSE head',info,c_fac*resp,chib(1,1))
        if (iref==4) call check('export exc, finite q, G=0 exchange (Lfull): Casida = BSE head',info,c_fac*resp,chib(1,1))
        if (iref==5) call check('export exc, BSEGinplane (masked G): Casida = BSE head',info,c_fac*resp,chib(1,1))
      enddo
    enddo
  end subroutine

  subroutine full_space_checks()
    ! Whole-transition-space routines (explicit anti-resonant transitions, finite q).
    ! 1. With the anti-resonant rows derived from the resonant ones, v = (v_r, v_c),
    !    s = (s, i s), E = (E, -E), d = (d, conj d), they are the pair routines.
    ! 2. Explicit anti-resonant rows with their own vertices, energies, occupations
    !    (f < 0) and oscillators, a BSE with exchange and an arbitrary W-like part:
    !    Casida with the exported kernel returns the BSE response, G-resolved.
    integer, parameter :: nh=2*nt
    complex(DP) :: M(nh,nh),Hx(nh,nh),Kx(nh,nh),Wp(nh,nh),V(ng,nh),sf(nh),df(nh),zz,vb(ng,ng)
    complex(DP) :: chib(ng,ng),chi_p(ng,ng),chi0(ng,ng),f(ng,ng),Pm(ng,ng),d(ng),r1,r2,worst_c
    real(DP) :: E(nh),fo(nh),sq(nt),rc(2),c_fac,worst
    integer :: t,i,iz,info
    c_fac=0.37_DP
    ! 1. pair (TR-derived) structure
    call random_vertices(rho_r)
    call random_vertices(rho_c)
    do t=1,nt
      sq(t)=sqrt(0.6_DP+0.2_DP*t)
      E(t)=0.8_DP+0.35_DP*t
      E(nt+t)=-E(t)
      sf(t)=sq(t)
      sf(nt+t)=ci*sq(t)
    enddo
    V(:,:nt)=rho_r
    V(:,nt+1:)=rho_c
    call random_matrix(M)
    do t=1,nh
      M(t,t)=M(t,t)+E(t)
    enddo
    zz=cmplx(1.3_DP,0.05_DP,kind=DP)
    call TDDFT_Chi_full_response(zz,M,V,sf,c_fac,chib,info)
    call TDDFT_Chi_pair_response(zz,M,rho_r,rho_c,sq,c_fac,chi_p,info)
    call check('full response = pair response for derived anti-resonant rows',info,chib(2,4),chi_p(2,4))
    call TDDFT_Chi_full_bubble(zz,E,V,sf,c_fac,chib)
    call TDDFT_Chi_pair_bubble(zz,E(:nt),rho_r,rho_c,sq,c_fac,chi_p)
    call check('full bubble = pair bubble for derived anti-resonant rows',0,chib(3,2),chi_p(3,2))
    call random_matrix(f)
    df(:nt)=conjg(rho_r(1,:))
    df(nt+1:)=rho_r(1,:)
    call TDDFT_Chi_full_dynamic_response(zz,M,f,V,sf,c_fac,df,r1,info)
    call TDDFT_Chi_dynamic_response(zz,M,f,rho_r,rho_c,sq,c_fac,sq*df(:nt),sq*conjg(df(:nt)),.TRUE.,r2,info)
    call check('full dynamic response = pair one for derived anti-resonant rows',info,r1,r2)
    ! 2. explicit anti-resonant rows
    call random_vertices(V)
    do t=1,nt
      fo(t)=0.6_DP+0.2_DP*t
      fo(nt+t)=-(0.5_DP+0.3_DP*t)
      E(t)=0.8_DP+0.35_DP*t
      E(nt+t)=-(0.9_DP+0.3_DP*t)
    enddo
    do i=1,nh
      sf(i)=sqrt(cmplx(fo(i),kind=DP))
    enddo
    if (abs(sf(nt+1)-ci*sqrt(-fo(nt+1)))>1.E-14_DP) then
      print *,'FAIL: sqrt of a negative occupation is not i sqrt|f|'
      failures=failures+1
    endif
    d(1)=40._DP
    do i=2,ng
      d(i)=1.5_DP/sqrt(real(i,DP))
    enddo
    vb=zero
    do i=2,ng
      vb(i,i)=d(i)**2
    enddo
    Kx=c_fac*matmul(conjg(transpose(V)),matmul(vb,V))
    do i=1,nh
      Kx(:,i)=Kx(:,i)*sf(i)
      Kx(i,:)=Kx(i,:)*sf(i)
    enddo
    call random_matrix(Wp)
    Wp=0.2_DP*Wp
    Hx=Kx
    do t=1,nh
      Hx(t,t)=Hx(t,t)+E(t)
    enddo
    M=Hx+Wp
    df=conjg(V(1,:))
    do iz=1,3
      zz=cmplx(0.4_DP+1.1_DP*iz,0.03_DP*iz,kind=DP)
      call TDDFT_Chi_full_response(zz,M,V,sf,c_fac,chib,info)
      call TDDFT_Chi_full_dynamic_response(zz,M,zero*f,V,sf,c_fac,df,r1,info)
      call check('explicit rows: BSE head response = G=0 element of chib',info,c_fac*r1,chib(1,1))
      call TDDFT_Chi_full_bubble(zz,E,V,sf,c_fac,chi0)
      call TDDFT_Chi_kernel_from_response(chi0,chib,d,.TRUE.,f,Pm,rc,info)
      if (info/=0) then
        print *,'FAIL: kernel_from_response (explicit rows) error',info
        failures=failures+1
        cycle
      endif
      call TDDFT_Chi_full_dynamic_response(zz,Hx,f,V,sf,c_fac,df,r1,info)
      call check('explicit rows: Casida(E, v, fxc) = BSE response head',info,c_fac*r1,chib(1,1))
      ! G-resolved: the Casida matrix with fxc gives chib for every G, G'
      Kx=c_fac*matmul(conjg(transpose(V)),matmul(f,V))
      do i=1,nh
        Kx(:,i)=Kx(:,i)*sf(i)
        Kx(i,:)=Kx(i,:)*sf(i)
      enddo
      call TDDFT_Chi_full_response(zz,Hx+Kx,V,sf,c_fac,chi_p,info)
      worst=maxval(abs(chi_p-chib))/maxval(abs(chib))
      worst_c=cmplx(worst,0._DP,kind=DP)
      call check('explicit rows: Casida G-resolved response = chib (max rel. dev.)',info,1._DP+worst_c,(1._DP,0._DP))
    enddo
  end subroutine

  subroutine mdr_checks()
    ! Marini-Del Sole-Rubio kernel (BSEChiOut= "mdr"), explicit rows, BSE with exchange Kx
    ! and a W-like part Wp. The exchange-free matrix is M - Kx = diag(E) + Wp.
    ! 1. Wp diagonal: MDR is exact (P and kernel equal the exported ones).
    ! 2. Off-diagonal Wp scaled by eps: P_MDR - P = O(eps^2) (first order in K_off).
    ! 3. Casida with the MDR kernel returns the head of (P_MDR^-1 - vbar)^-1.
    integer, parameter :: nh=2*nt
    complex(DP) :: M(nh,nh),Kx(nh,nh),Wp(nh,nh),Wd(nh,nh),Koff(nh,nh),Ed(nh),V(ng,nh),sf(nh),df(nh),zz
    complex(DP) :: chib(ng,ng),chi0(ng,ng),f(ng,ng),Pm(ng,ng),fm(ng,ng),Pmd(ng,ng),p0(ng,ng),dp1(ng,ng)
    complex(DP) :: vb(ng,ng),d(ng),r1,ch(ng,ng),e_c(2)
    real(DP) :: E(nh),fo(nh),rc(2),c_fac,eps(2),errs(2)
    integer :: t,i,ie,info
    c_fac=0.37_DP
    call random_vertices(V)
    do t=1,nt
      fo(t)=0.6_DP+0.2_DP*t
      fo(nt+t)=-(0.5_DP+0.3_DP*t)
      E(t)=0.8_DP+0.35_DP*t
      E(nt+t)=-(0.9_DP+0.3_DP*t)
    enddo
    do i=1,nh
      sf(i)=sqrt(cmplx(fo(i),kind=DP))
    enddo
    d(1)=40._DP
    do i=2,ng
      d(i)=1.5_DP/sqrt(real(i,DP))
    enddo
    vb=zero
    do i=2,ng
      vb(i,i)=d(i)**2
    enddo
    Kx=c_fac*matmul(conjg(transpose(V)),matmul(vb,V))
    do i=1,nh
      Kx(:,i)=Kx(:,i)*sf(i)
      Kx(i,:)=Kx(i,:)*sf(i)
    enddo
    call random_matrix(Wp)
    Wp=0.2_DP*Wp
    Wd=zero
    do t=1,nh
      Wd(t,t)=Wp(t,t)
    enddo
    df=conjg(V(1,:))
    zz=cmplx(1.6_DP,0.04_DP,kind=DP)
    eps=[0._DP,0._DP]
    do ie=0,2
      if (ie>0) eps(ie)=1.E-3_DP*ie
      Koff=Wp-Wd
      if (ie>0) Koff=eps(ie)*Koff
      if (ie==0) Koff=zero
      M=Kx+Wd+Koff
      do t=1,nh
        M(t,t)=M(t,t)+E(t)
        Ed(t)=E(t)+Wd(t,t)
      enddo
      call TDDFT_Chi_full_response(zz,M,V,sf,c_fac,chib,info)
      call TDDFT_Chi_full_bubble(zz,E,V,sf,c_fac,chi0)
      call TDDFT_Chi_kernel_from_response(chi0,chib,d,.TRUE.,f,Pm,rc,info)
      call TDDFT_Chi_mdr_response(zz,Ed,Koff,V,sf,c_fac,p0,dp1)
      call TDDFT_Chi_kernel_mdr(chi0,p0,dp1,d,fm,Pmd,rc,info)
      if (info/=0) then
        print *,'FAIL: kernel_mdr error',info
        failures=failures+1
        return
      endif
      if (ie==0) then
        call check('MDR with diagonal W = exact P (G=1,2)',info,Pmd(1,2),Pm(1,2))
        call check('MDR with diagonal W = exact fxc (G=2,3)',info,fm(2,3),f(2,3))
        ! 3. Casida with the MDR kernel: head of (P_MDR^-1 - vbar)^-1
        ch=Pmd
        call inv(ch)
        ch=ch-vb
        call inv(ch)
        call TDDFT_Chi_full_dynamic_response(zz,Kx+diag(E),fm,V,sf,c_fac,df,r1,info)
        call check('Casida(E, v, fxc_MDR) = head of (P_MDR^-1 - vbar)^-1',info,c_fac*r1,ch(1,1))
      else
        errs(ie)=maxval(abs(Pmd-Pm))/maxval(abs(Pm))
      endif
    enddo
    ! second order: doubling eps multiplies the error by 4
    e_c(1)=cmplx(errs(2)/errs(1),0._DP,kind=DP)
    if (abs(real(e_c(1))-4._DP)<0.2_DP) then
      print '(a,es9.2,a,f6.3)','PASS: MDR error is second order in K_off  err(eps)=',errs(1),'  err(2eps)/err(eps)=',real(e_c(1))
    else
      print '(a,2es10.2)','FAIL: MDR error not second order in K_off ',errs
      failures=failures+1
    endif
  end subroutine

  function diag(x) result(m)
    real(DP), intent(in) :: x(:)
    complex(DP) :: m(size(x),size(x))
    integer :: i
    m=zero
    do i=1,size(x)
      m(i,i)=x(i)
    enddo
  end function

  subroutine inv(mat)
    complex(DP), intent(inout) :: mat(:,:)
    complex(DP) :: lw(4*size(mat,1))
    integer :: ip(size(mat,1)),info,n
    n=size(mat,1)
    call zgetrf(n,n,mat,n,ip,info)
    call zgetri(n,mat,n,ip,lw,size(lw),info)
  end subroutine

  subroutine check(what,err,got,expected)
    character(*), intent(in) :: what
    integer,      intent(in) :: err
    complex(DP),  intent(in) :: got,expected
    real(DP) :: rel
    rel=abs(got-expected)/max(abs(expected),1.E-30_DP)
    if (err/=0.or.rel>1.E-10_DP) then
      print '(a,a,i4,a,es10.2,a,2es14.6,a,2es14.6)','FAIL: ',what,err,' rel=',rel,' got=',got,' ref=',expected
      failures=failures+1
    else
      print '(a,a,a,es9.2)','PASS: ',what,'  rel=',rel
    endif
  end subroutine

end program test_chi_dynamic
