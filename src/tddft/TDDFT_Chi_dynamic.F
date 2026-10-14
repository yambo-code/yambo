!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
! Casida response with a frequency-dependent kernel fxc(G,G';z), one frequency at
! a time. Only BLAS/LAPACK is used, so tests/chi_dynamic builds this file alone.
! The routines are recursive: K_Chi_dynamic calls them from OpenMP threads.
!
! Conventions are those of K_kernel/K_stored_in_a_big_matrix/K_diago_*:
!
!  * H0 is Yambo's BS_mat without the fxc part: transition energies plus the
!    static kernel (exchange). N x N in TDA, 2N x 2N with coupling, where the
!    coupling blocks carry the factor i of K_kernel ("C" block times cI).
!  * The fxc part of a block is  Kf_xy = c diag(s) V_x^H fxc(z) V_y diag(s),
!    with V_res the excitation vertices (O_tddft_R in mode "R"), V_cpl the
!    de-excitation ones (mode "C"), s = sqrt(f) and c = Co/(4 pi) of K_kernel.
!    For a Hermitian static fxc this is exactly what K_kernel adds to BS_mat.
!  * a = d sqrt(f) and b = conj(d) sqrt(f) are the resonant residual vectors of
!    K_diago_left/right_residuals. The anti-resonant ones are (i b, i a).
!
! Returned is  resp = B^T [z - M(z)]^-1 A  with
!
!    coupling: M = H0 + [[ Kf_rr, i Kf_rc ], [ i Kf_cr, -Kf_cc ]],  A=(a,i b), B=(b,i a)
!    TDA     : the two diagonal blocks alone, the anti-resonant one being
!              -conj(H0) - Kf_cc (A = -R* of K_stored_in_a_big_matrix).
!
! Resp_ii = -Co_factor*resp is what K_diago_response_functions accumulates as
! -Co_factor*sum_l R_L R_R/(z-E_l) (TDA: plus the anti-resonant mirror). With a
! z-independent Hermitian fxc the two agree identically.
!
! Every block uses the same retarded fxc(z), z = w + i eta, as the pair-space
! Dyson equation chi = [chi0^-1 - K(z)]^-1 requires: the anti-resonant pairs see
! fxc at +w through their own vertices, not a mirror of the resonant block.
!
module TDDFT_Chi_dynamic_m
 use pars, only: DP
 implicit none
 private
 public :: TDDFT_Chi_dynamic_kernel,TDDFT_Chi_dynamic_response
 public :: TDDFT_Chi_pair_response,TDDFT_Chi_pair_bubble,TDDFT_Chi_kernel_from_response
 public :: TDDFT_Chi_full_response,TDDFT_Chi_full_bubble,TDDFT_Chi_full_dynamic_response
 public :: TDDFT_Chi_mdr_response,TDDFT_Chi_kernel_mdr
 complex(DP), parameter :: zero=(0._DP,0._DP),one=(1._DP,0._DP),ci=(0._DP,1._DP)
contains
 !
 recursive subroutine TDDFT_Chi_dynamic_kernel(fxc,v_left,v_right,s,c,kernel,ierr)
   ! kernel(t,t') = c s_t [ sum_GG' conj(v_left(G,t)) fxc(G,G') v_right(G',t') ] s_t'
   complex(DP), intent(in)  :: fxc(:,:),v_left(:,:),v_right(:,:)
   real(DP),    intent(in)  :: s(:),c
   complex(DP), intent(out) :: kernel(:,:)
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: fv(:,:)
   integer :: ng,nt,it
   ! ierr: 1 fxc not square, 2 vertex G size, 3 vertex/s/kernel transition size
   ierr=1
   ng=size(fxc,1)
   if (ng<1.or.size(fxc,2)/=ng) return
   ierr=2
   if (size(v_left,1)/=ng.or.size(v_right,1)/=ng) return
   ierr=3
   nt=size(v_right,2)
   if (nt<1.or.size(v_left,2)/=nt.or.size(s)/=nt) return
   if (size(kernel,1)/=nt.or.size(kernel,2)/=nt) return
   allocate(fv(ng,nt))
   call zgemm('N','N',ng,nt,ng,one,fxc,ng,v_right,ng,zero,fv,ng)
   call zgemm('C','N',nt,nt,ng,cmplx(c,0._DP,kind=DP),v_left,ng,fv,ng,zero,kernel,nt)
   deallocate(fv)
   do it=1,nt
     kernel(:,it)=kernel(:,it)*s(it)
     kernel(it,:)=kernel(it,:)*s(it)
   enddo
   ierr=0
 end subroutine TDDFT_Chi_dynamic_kernel
 !
 recursive subroutine TDDFT_Chi_dynamic_response(z,H0,fxc,v_res,v_cpl,s,c,a,b,coupling,resp,ierr)
   complex(DP), intent(in)  :: z,H0(:,:),fxc(:,:),v_res(:,:),v_cpl(:,:),a(:),b(:)
   real(DP),    intent(in)  :: s(:),c
   logical,     intent(in)  :: coupling
   complex(DP), intent(out) :: resp
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: M(:,:),K(:,:),rhs(:)
   integer,     allocatable :: ipiv(:)
   integer :: n,nh,i,info
   ! ierr: 10+ kernel size error, 20 H0 size, 30+info singular z-M (LAPACK info>0), 40 LAPACK argument
   resp=zero
   n=size(a)
   nh=n
   if (coupling) nh=2*n
   ierr=20
   if (size(b)/=n.or.size(H0,1)/=nh.or.size(H0,2)/=nh) return
   allocate(K(n,n),M(nh,nh),rhs(nh),ipiv(nh))
   if (coupling) then
     M=-H0
     call TDDFT_Chi_dynamic_kernel(fxc,v_res,v_res,s,c,K,ierr)
     if (ierr/=0) goto 1
     M(:n,:n)=M(:n,:n)-K
     call TDDFT_Chi_dynamic_kernel(fxc,v_res,v_cpl,s,c,K,ierr)
     if (ierr/=0) goto 1
     M(:n,n+1:)=M(:n,n+1:)-ci*K
     call TDDFT_Chi_dynamic_kernel(fxc,v_cpl,v_res,s,c,K,ierr)
     if (ierr/=0) goto 1
     M(n+1:,:n)=M(n+1:,:n)-ci*K
     call TDDFT_Chi_dynamic_kernel(fxc,v_cpl,v_cpl,s,c,K,ierr)
     if (ierr/=0) goto 1
     M(n+1:,n+1:)=M(n+1:,n+1:)+K
     do i=1,nh
       M(i,i)=M(i,i)+z
     enddo
     rhs(:n)=a
     rhs(n+1:)=ci*b
     call solve(nh,M,rhs,ierr)
     if (ierr/=0) goto 1
     resp=sum(b*rhs(:n))+sum(ci*a*rhs(n+1:))
   else
     ! Resonant block: b^T (z - H0 - Kf_rr)^-1 a
     call TDDFT_Chi_dynamic_kernel(fxc,v_res,v_res,s,c,K,ierr)
     if (ierr/=0) goto 1
     M=-H0-K
     do i=1,n
       M(i,i)=M(i,i)+z
     enddo
     rhs=a
     call solve(n,M,rhs,ierr)
     if (ierr/=0) goto 1
     resp=sum(b*rhs)
     ! Anti-resonant block: (i a)^T (z + conj(H0) + Kf_cc)^-1 (i b)
     call TDDFT_Chi_dynamic_kernel(fxc,v_cpl,v_cpl,s,c,K,ierr)
     if (ierr/=0) goto 1
     M=conjg(H0)+K
     do i=1,n
       M(i,i)=M(i,i)+z
     enddo
     rhs=b
     call solve(n,M,rhs,ierr)
     if (ierr/=0) goto 1
     resp=resp-sum(a*rhs)
   endif
   ierr=0
1  if (ierr>0.and.ierr<10) ierr=10+ierr
   deallocate(K,M,rhs,ipiv)
 contains
   subroutine solve(m_dim,mat,x,err)
     integer,     intent(in)    :: m_dim
     complex(DP), intent(inout) :: mat(:,:),x(:)
     integer,     intent(out)   :: err
     call zgesv(m_dim,1,mat,m_dim,ipiv,x,m_dim,info)
     err=0
     if (info>0) err=30
     if (info<0) err=40
   end subroutine solve
 end subroutine TDDFT_Chi_dynamic_response
 !
 ! Export side (BSEChiOut). The G-resolved coupled response of a BSE matrix M,
 !
 !   chi(G,G') = c B_G^T [z - M]^-1 A_G',  B_G = s (V_r(G,:), i V_c(G,:)),
 !                                          A_G'= s (conj V_r(G',:), i conj V_c(G',:)),
 !
 ! is the head formula of TDDFT_Chi_dynamic_response for every pair of G. With
 ! M = diag(E,-E) it is the bubble of TDDFT_Chi_pair_bubble.
 !
 recursive subroutine TDDFT_Chi_pair_response(z,M,v_res,v_cpl,s,c,chi,ierr)
   complex(DP), intent(in)  :: z,M(:,:),v_res(:,:),v_cpl(:,:)
   real(DP),    intent(in)  :: s(:),c
   complex(DP), intent(out) :: chi(:,:)
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: W(:,:),X(:,:),B(:,:)
   integer,     allocatable :: ipiv(:)
   integer :: n,nh,ng,ig,i,info
   ! ierr: 20 sizes, 30 singular z-M, 40 LAPACK argument
   chi=zero
   n=size(s); nh=2*n; ng=size(v_res,1)
   ierr=20
   if (size(M,1)/=nh.or.size(M,2)/=nh.or.size(v_res,2)/=n.or.any(shape(v_cpl)/=(/ng,n/))) return
   if (size(chi,1)/=ng.or.size(chi,2)/=ng) return
   allocate(W(nh,nh),X(nh,ng),B(ng,nh),ipiv(nh))
   do ig=1,ng
     X(:n,ig)=s*conjg(v_res(ig,:))
     X(n+1:,ig)=ci*s*conjg(v_cpl(ig,:))
     B(ig,:n)=s*v_res(ig,:)
     B(ig,n+1:)=ci*s*v_cpl(ig,:)
   enddo
   W=-M
   do i=1,nh
     W(i,i)=W(i,i)+z
   enddo
   call zgesv(nh,ng,W,nh,ipiv,X,nh,info)
   ierr=0
   if (info>0) ierr=30
   if (info<0) ierr=40
   if (ierr==0) call zgemm('N','N',ng,ng,nh,cmplx(c,0._DP,kind=DP),B,ng,X,nh,zero,chi,ng)
   deallocate(W,X,B,ipiv)
 end subroutine TDDFT_Chi_pair_response
 !
 recursive subroutine TDDFT_Chi_pair_bubble(z,E,v_res,v_cpl,s,c,chi)
   ! chi(G,G') = c sum_t s_t^2 [ V_r V_r^H/(z-E_t) - V_c V_c^H/(z+E_t) ](G,G')
   complex(DP), intent(in)  :: z,v_res(:,:),v_cpl(:,:)
   real(DP),    intent(in)  :: E(:),s(:),c
   complex(DP), intent(out) :: chi(:,:)
   complex(DP), allocatable :: Y(:,:)
   integer :: n,ng,t
   n=size(s); ng=size(v_res,1)
   allocate(Y(ng,n))
   do t=1,n
     Y(:,t)=v_res(:,t)*s(t)**2/(z-E(t))
   enddo
   call zgemm('N','C',ng,ng,n,cmplx(c,0._DP,kind=DP),Y,ng,v_res,ng,zero,chi,ng)
   do t=1,n
     Y(:,t)=v_cpl(:,t)*s(t)**2/(z+E(t))
   enddo
   call zgemm('N','C',ng,ng,n,cmplx(-c,0._DP,kind=DP),Y,ng,v_cpl,ng,one,chi,ng)
   deallocate(Y)
 end subroutine TDDFT_Chi_pair_bubble
 !
 ! Coupling in the whole transition space. Every row A of the coupled BSE matrix (2N
 ! resonant and anti-resonant transitions) has its own vertex v(:,A), occupation factor
 ! f_A (negative for the anti-resonant ones), s_A = sqrt(f_A) (i sqrt|f_A| then, as the
 ! sqrt_f of K_kernel), energy E_A and oscillator d_A: BSS_eh_f, BSS_eh_E and
 ! BSS_dipoles_opt over BS_H_dim. Anti-resonant blocks derived from the resonant ones
 ! (q=0) are the case v = (v_res, v_cpl), s = (s, i s), E = (E, -E), d = (d, conj d),
 ! where these routines reduce to the pair ones above; explicit anti-resonant
 ! transitions (finite q) need them.
 !
 recursive subroutine TDDFT_Chi_full_response(z,M,v,s,c,chi,ierr)
   ! chi(G,G') = c sum_AB s_A v(G,A) [z - M]^-1_AB s_B conj(v(G',B))
   complex(DP), intent(in)  :: z,M(:,:),v(:,:),s(:)
   real(DP),    intent(in)  :: c
   complex(DP), intent(out) :: chi(:,:)
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: W(:,:),X(:,:),B(:,:)
   integer,     allocatable :: ipiv(:)
   integer :: nh,ng,ig,i,info
   ! ierr: 20 sizes, 30 singular z-M, 40 LAPACK argument
   chi=zero
   nh=size(s); ng=size(v,1)
   ierr=20
   if (size(M,1)/=nh.or.size(M,2)/=nh.or.size(v,2)/=nh) return
   if (size(chi,1)/=ng.or.size(chi,2)/=ng) return
   allocate(W(nh,nh),X(nh,ng),B(ng,nh),ipiv(nh))
   do ig=1,ng
     X(:,ig)=s*conjg(v(ig,:))
     B(ig,:)=s*v(ig,:)
   enddo
   W=-M
   do i=1,nh
     W(i,i)=W(i,i)+z
   enddo
   call zgesv(nh,ng,W,nh,ipiv,X,nh,info)
   ierr=0
   if (info>0) ierr=30
   if (info<0) ierr=40
   if (ierr==0) call zgemm('N','N',ng,ng,nh,cmplx(c,0._DP,kind=DP),B,ng,X,nh,zero,chi,ng)
   deallocate(W,X,B,ipiv)
 end subroutine TDDFT_Chi_full_response
 !
 recursive subroutine TDDFT_Chi_full_bubble(z,E,v,s,c,chi)
   ! chi(G,G') = c sum_A s_A^2 v(G,A) conj(v(G',A))/(z-E_A)
   complex(DP), intent(in)  :: z,v(:,:),s(:)
   real(DP),    intent(in)  :: E(:),c
   complex(DP), intent(out) :: chi(:,:)
   complex(DP), allocatable :: Y(:,:)
   integer :: nh,ng,i
   nh=size(s); ng=size(v,1)
   allocate(Y(ng,nh))
   do i=1,nh
     Y(:,i)=v(:,i)*s(i)**2/(z-E(i))
   enddo
   call zgemm('N','C',ng,ng,nh,cmplx(c,0._DP,kind=DP),Y,ng,v,ng,zero,chi,ng)
   deallocate(Y)
 end subroutine TDDFT_Chi_full_bubble
 !
 recursive subroutine TDDFT_Chi_full_dynamic_response(z,H0,fxc,v,s,c,d,resp,ierr)
   ! resp = sum_AB s_A conj(d_A) [z - H0 - Kf(z)]^-1_AB s_B d_B,
   ! Kf(A,B) = c s_A s_B sum_GG' conj(v(G,A)) fxc(G,G') v(G',B)
   complex(DP), intent(in)  :: z,H0(:,:),fxc(:,:),v(:,:),s(:),d(:)
   real(DP),    intent(in)  :: c
   complex(DP), intent(out) :: resp
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: M(:,:),fv(:,:),rhs(:)
   integer,     allocatable :: ipiv(:)
   integer :: nh,ng,i,info
   ! ierr: 11 fxc not square, 12 vertex G size, 20 H0/vertex/d size, 30 singular, 40 LAPACK argument
   resp=zero
   nh=size(s); ng=size(fxc,1)
   ierr=11
   if (ng<1.or.size(fxc,2)/=ng) return
   ierr=12
   if (size(v,1)/=ng) return
   ierr=20
   if (size(v,2)/=nh.or.size(d)/=nh.or.size(H0,1)/=nh.or.size(H0,2)/=nh) return
   allocate(M(nh,nh),fv(ng,nh),rhs(nh),ipiv(nh))
   call zgemm('N','N',ng,nh,ng,one,fxc,ng,v,ng,zero,fv,ng)
   call zgemm('C','N',nh,nh,ng,cmplx(c,0._DP,kind=DP),v,ng,fv,ng,zero,M,nh)
   do i=1,nh
     M(:,i)=-M(:,i)*s(i)
     M(i,:)=M(i,:)*s(i)
   enddo
   M=M-H0
   do i=1,nh
     M(i,i)=M(i,i)+z
   enddo
   rhs=s*d
   call zgesv(nh,1,M,nh,ipiv,rhs,nh,info)
   ierr=0
   if (info>0) ierr=30
   if (info<0) ierr=40
   if (ierr==0) resp=sum(s*conjg(d)*rhs)
   deallocate(M,fv,rhs,ipiv)
 end subroutine TDDFT_Chi_full_dynamic_response
 !
 ! Marini-Del Sole-Rubio kernel (PRL 91, 256402 (2003)). The exchange-free BSE matrix
 ! M_W = Ed + K_off is split into its diagonal Ed (the e-h attraction of each transition
 ! moved into its energy) and the off-diagonal coupling K_off, taken to first order:
 !
 !   [z - M_W]^-1 ~ g + g K_off g,  g = diag(1/(z - Ed))
 !
 !   p0(G,G') = c sum_A   s_A v(G,A) g_A conj(v(G',A)) s_A          (shifted bubble)
 !   dP1(G,G') = c sum_AB  s_A v(G,A) g_A K_off(A,B) g_B conj(v(G',B)) s_B
 !
 ! The kernel on the shifted bubble is p0^-1 dP1 p0^-1 (first order in dP1); on the
 ! reference bubble chi0 the diagonal shift adds chi0^-1 - p0^-1:
 !
 !   fxc = chi0^-1 - P^-1,  P^-1 = p0^-1 - p0^-1 dP1 p0^-1
 !
 ! Exact when K_off = 0. Same vertices, s and c as TDDFT_Chi_full_response.
 !
 recursive subroutine TDDFT_Chi_mdr_response(z,Ed,Koff,v,s,c,p0,dP1)
   complex(DP), intent(in)  :: z,Ed(:),Koff(:,:),v(:,:),s(:)
   real(DP),    intent(in)  :: c
   complex(DP), intent(out) :: p0(:,:),dP1(:,:)
   complex(DP), allocatable :: Y(:,:),X(:,:),YK(:,:)
   integer :: nh,ng,i
   nh=size(s); ng=size(v,1)
   allocate(Y(ng,nh),X(ng,nh),YK(ng,nh))
   ! Y(G,A) = s_A v(G,A) g_A,  X(G',A) = s_A conj(v(G',A))  (s_A, not conj(s_A): i for
   ! the anti-resonant rows, as in TDDFT_Chi_full_response)
   do i=1,nh
     Y(:,i)=s(i)*v(:,i)/(z-Ed(i))
     X(:,i)=s(i)*conjg(v(:,i))
   enddo
   call zgemm('N','T',ng,ng,nh,cmplx(c,0._DP,kind=DP),Y,ng,X,ng,zero,p0,ng)
   ! dP1 = c (Y K_off) (g X)^T
   call zgemm('N','N',ng,nh,nh,one,Y,ng,Koff,nh,zero,YK,ng)
   do i=1,nh
     X(:,i)=X(:,i)/(z-Ed(i))
   enddo
   call zgemm('N','T',ng,ng,nh,cmplx(c,0._DP,kind=DP),YK,ng,X,ng,zero,dP1,ng)
   deallocate(Y,X,YK)
 end subroutine TDDFT_Chi_mdr_response
 !
 recursive subroutine TDDFT_Chi_kernel_mdr(chi0,p0,dP1,d,fxc,P,rcond,ierr)
   ! fxc = chi0^-1 - p0^-1 + p0^-1 dP1 p0^-1 and P = (p0^-1 - p0^-1 dP1 p0^-1)^-1, inverted on
   ! the Coulomb-symmetrized matrices D x D as TDDFT_Chi_kernel_from_response
   complex(DP), intent(in)  :: chi0(:,:),p0(:,:),dP1(:,:),d(:)
   complex(DP), intent(out) :: fxc(:,:),P(:,:)
   real(DP),    intent(out) :: rcond(2)
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: A(:,:),B(:,:),C(:,:),T(:,:)
   real(DP) :: rcP
   integer :: ng,i,j
   ! ierr: 20 sizes, 31/32 singular chi0/p0, 33 singular P^-1
   ng=size(d)
   fxc=zero; P=zero; rcond=0._DP
   ierr=20
   if (any(shape(chi0)/=(/ng,ng/)).or.any(shape(p0)/=(/ng,ng/)).or.any(shape(dP1)/=(/ng,ng/)).or.&
&      any(shape(fxc)/=(/ng,ng/)).or.any(shape(P)/=(/ng,ng/))) return
   allocate(A(ng,ng),B(ng,ng),C(ng,ng),T(ng,ng))
   do j=1,ng
     do i=1,ng
       A(i,j)=d(i)*chi0(i,j)*d(j)
       B(i,j)=d(i)*p0(i,j)*d(j)
       C(i,j)=d(i)*dP1(i,j)*d(j)
     enddo
   enddo
   call invert(A,rcond(1),ierr)
   if (ierr/=0) then; ierr=31; goto 1; endif
   call invert(B,rcond(2),ierr)
   if (ierr/=0) then; ierr=32; goto 1; endif
   ! B <- p0~^-1 - p0~^-1 dP1~ p0~^-1
   call zgemm('N','N',ng,ng,ng,one,C,ng,B,ng,zero,T,ng)
   call zgemm('N','N',ng,ng,ng,one,B,ng,T,ng,zero,C,ng)
   B=B-C
   do j=1,ng
     do i=1,ng
       fxc(i,j)=d(i)*(A(i,j)-B(i,j))*d(j)
     enddo
   enddo
   call invert(B,rcP,ierr)
   if (ierr/=0) then; ierr=33; goto 1; endif
   do j=1,ng
     do i=1,ng
       P(i,j)=B(i,j)/(d(i)*d(j))
     enddo
   enddo
   ierr=0
1  deallocate(A,B,C,T)
 contains
   recursive subroutine invert(Mx,rc,err)
     complex(DP), intent(inout) :: Mx(:,:)
     real(DP),    intent(out)   :: rc
     integer,     intent(out)   :: err
     integer :: ipv(size(Mx,1)),info,m,lw
     complex(DP) :: wq(1)
     complex(DP), allocatable :: wk(:)
     real(DP) :: anorm,rwk(2*size(Mx,1))
     complex(DP) :: cwk(2*size(Mx,1))
     m=size(Mx,1)
     anorm=maxval(sum(abs(Mx),dim=1))
     err=1
     call zgetrf(m,m,Mx,m,ipv,info)
     if (info/=0) return
     call zgecon('1',m,Mx,m,anorm,rc,cwk,rwk,info)
     call zgetri(m,Mx,m,ipv,wq,-1,info)
     lw=max(1,int(real(wq(1))))
     allocate(wk(lw))
     call zgetri(m,Mx,m,ipv,wk,lw,info)
     deallocate(wk)
     if (info==0) err=0
   end subroutine invert
 end subroutine TDDFT_Chi_kernel_mdr
 !
 recursive subroutine TDDFT_Chi_kernel_from_response(chi0,chib,d,exch,fxc,P,rcond,ierr,exch_head)
   ! fxc = chi0^-1 - P^-1 with P^-1 = chib^-1 + vbar, vbar = diag(0, d_G^2) when the
   ! response chib contains the exchange (Lbar) of the BSE. Inversions are done on the
   ! Coulomb-symmetrized matrices x~ = D x D, D = diag(d), as in Chi_fxc:
   !   P~^-1 = chib~^-1 + J, J = diag(0,1,..,1),  fxc = D (chi0~^-1 - P~^-1) D.
   ! exch_head (optional, default .FALSE.): the BSE exchange includes G=0 (Lkind=full, or
   ! "default" with a Coulomb cutoff at q>1), so vbar = v and J = 1.
   ! d = sqrt(4 pi)/bare_qpg is complex with a cut Coulomb (v_cut < 0 gives an imaginary
   ! d), exactly as the BSE exchange divides by bare_qpg**2.
   ! rcond = reciprocal condition numbers of chi0~ and chib~.
   complex(DP), intent(in)  :: chi0(:,:),chib(:,:)
   complex(DP), intent(in)  :: d(:)
   logical,     intent(in)  :: exch
   logical,     intent(in), optional :: exch_head
   complex(DP), intent(out) :: fxc(:,:),P(:,:)
   real(DP),    intent(out) :: rcond(2)
   integer,     intent(out) :: ierr
   complex(DP), allocatable :: A(:,:),B(:,:)
   real(DP) :: rcP
   integer :: ng,i,j
   ! ierr: 20 sizes, 31/32 singular chi0/chib, 33 singular P^-1
   ng=size(d)
   fxc=zero; P=zero; rcond=0._DP
   ierr=20
   if (any(shape(chi0)/=(/ng,ng/)).or.any(shape(chib)/=(/ng,ng/)).or.&
&      any(shape(fxc)/=(/ng,ng/)).or.any(shape(P)/=(/ng,ng/))) return
   allocate(A(ng,ng),B(ng,ng))
   do j=1,ng
     do i=1,ng
       A(i,j)=d(i)*chi0(i,j)*d(j)
       B(i,j)=d(i)*chib(i,j)*d(j)
     enddo
   enddo
   call invert(A,rcond(1),ierr)
   if (ierr/=0) then; ierr=31; goto 1; endif
   call invert(B,rcond(2),ierr)
   if (ierr/=0) then; ierr=32; goto 1; endif
   if (exch) then
     do i=2,ng
       B(i,i)=B(i,i)+one
     enddo
     if (present(exch_head)) then
       if (exch_head) B(1,1)=B(1,1)+one
     endif
   endif
   do j=1,ng
     do i=1,ng
       fxc(i,j)=d(i)*(A(i,j)-B(i,j))*d(j)
     enddo
   enddo
   A=B
   call invert(A,rcP,ierr)
   if (ierr/=0) then; ierr=33; goto 1; endif
   do j=1,ng
     do i=1,ng
       P(i,j)=A(i,j)/(d(i)*d(j))
     enddo
   enddo
   ierr=0
1  deallocate(A,B)
 contains
   recursive subroutine invert(Mx,rc,err)
     complex(DP), intent(inout) :: Mx(:,:)
     real(DP),    intent(out)   :: rc
     integer,     intent(out)   :: err
     integer :: ipv(size(Mx,1)),info,m,lw
     complex(DP) :: wq(1)
     complex(DP), allocatable :: wk(:)
     real(DP) :: anorm,rwk(2*size(Mx,1))
     complex(DP) :: cwk(2*size(Mx,1))
     m=size(Mx,1)
     anorm=maxval(sum(abs(Mx),dim=1))
     err=1
     call zgetrf(m,m,Mx,m,ipv,info)
     if (info/=0) return
     call zgecon('1',m,Mx,m,anorm,rc,cwk,rwk,info)
     call zgetri(m,Mx,m,ipv,wq,-1,info)
     lw=max(1,int(real(wq(1))))
     allocate(wk(lw))
     call zgetri(m,Mx,m,ipv,wk,lw,info)
     deallocate(wk)
     if (info==0) err=0
   end subroutine invert
 end subroutine TDDFT_Chi_kernel_from_response
end module TDDFT_Chi_dynamic_m
