!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS IMA
!
subroutine K_Haydock_response(iq,it,Cf_size,OBS,scheme,W,Af,Bf,Cf,reached_threshold,FG_factor)
 !
 use pars,           ONLY:SP,cZERO,cONE,pi
 use X_m,            ONLY:Resp_ii,Joint_DOS,X_drude_term,global_gauge,skip_cond_sum_rule, &
&                         X_magnons,Resp_ij,i_G_shift
 use R_lattice,      ONLY:bare_qpg
 use frequency,      ONLY:w_samp
 use BS,             ONLY:BSE_mode
 use BS_solvers,     ONLY:Co_factor,para_term_w0,Haydock_v0_mod,&
 &                        Haydock_terminator,Haydock_threshold
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(w_samp),intent(in) :: W
 integer,     intent(in) :: iq,it,Cf_size
 character(*),intent(in) :: scheme
 character(*),intent(in) :: OBS
 real(SP),    intent(in) :: Bf(it+1),Af(it)
 complex(SP), intent(in) :: Cf(Cf_size)
 real(SP),    intent(out):: reached_threshold
 real(SP),    intent(in) :: FG_factor
 !
 ! Work Space
 !
 logical     :: update_thresh
 integer     :: i1,iw,iw_start,i_resp(2)
 real(SP)    :: Co,tmp_tresh
 complex(SP) :: sums,diff
 !
 complex(SP) :: gauge_factor
 complex(SP) :: freq_axis(0:W%n_freqs)
 !
 complex(SP) :: X_o(2,it), X_a(2,it), YbyX(2)
 complex(SP),allocatable :: X_i(:,:)
 !
 ! related to terminator
 !
 complex(SP) :: X_t(2)
 real(SP)    :: Av1, Av2
 integer, parameter :: RES = 3, ARES = 2, COUPL = 1, MIN_ITER = 10
 logical     :: UseTerminator,use_A4,use_A6
 logical     :: retarded,use_Cf
 !
 Co=Co_factor*Haydock_v0_mod**2*FG_factor
 reached_threshold=0._SP
 !
 X_t =cZERO
 UseTerminator = .false.  
 !
 call parser("UseA4",use_A4)    ! For debug pourposes
 use_A6=.not.use_A4
 !
 use_Cf=Cf_size>2
 retarded= (trim(BSE_mode)=='retarded') .and.(trim(scheme)=="hermitian")
 !
 i_resp(1)=2
 i_resp(2)=4
 !
 !
 ! DEBUG <
 !tmp_tresh=0._SP
 !if (Cf_size>2 ) then
 !  do i1=1,it
 !    tmp_tresh=tmp_tresh+Cf(i1)*conjg(Cf(i1))
 !  enddo
 !  write(100,*) "Projection of W0 on q_i is equal to ",tmp_tresh
 !endif
 ! DEBUG >
 !
 if(trim(global_gauge)=='length')   gauge_factor=cONE*Co/bare_qpg(iq,i_G_shift)**2
 if(trim(global_gauge)=='velocity') gauge_factor=cONE*Co
 !
 if (it > MIN_ITER.and.Haydock_terminator) UseTerminator = .true.
 if (use_Cf) allocate(X_i(2,it))
 if (Useterminator) then 
   select case(scheme)
   case("hermitian")
     Av1 = 0._SP
     Av2 = 0._SP
     do i1 = 1, it
       Av1 = Av1 + Af(i1)
       Av2 = Av2 + Bf(i1)
     end do
     Av1 = Av1/it
     Av2 = Av2/it
   case("pseudo-herm")
     Av1 = 0._SP
     Av2 = 0._SP
     do i1 = 2,it+1,2
       Av1 = Av1 + Bf(i1)
       Av2 = Av2 + Bf(i1+1)
     end do
     Av1 = Av1/(it/2+mod(it,2))
     Av2 = Av2/(it/2)
   end select
 end if
 !
 iw_start=1
 if(trim(OBS)=="optics") then
   para_term_w0(2:3)=cZERO
   if(any(Af/=0._SP)) iw_start=0
 endif
 !
 freq_axis(0 )=cZERO
 freq_axis(1:)=W%p(:)
 !
 do iw=iw_start,W%n_freqs
   !
   X_o=cZERO
   !
   X_t=cZERO
   if (UseTerminator) then 
     select case(scheme)
     case("hermitian")
       X_t(1) = terminator(freq_axis(iw), Av1,Av2, RES)
       if (retarded) X_t(2) = -terminator(conjg(freq_axis(iw)),-Av1,Av2,ARES)
     case("pseudo-herm")
       if ((mod(it,2)==0)) X_t(1) = terminator(freq_axis(iw),Av1,Av2,COUPL)
       if ((mod(it,2)==1)) X_t(1) = terminator(freq_axis(iw),Av2,Av1,COUPL)
     end select
   endif
   !
   ! Respoinse functions at iteration it
   !
   X_o(1,it) = 1._SP/(freq_axis(iw)-Af(it)-Bf(it+1)**2*X_t(1))
   if (retarded) X_a(1,it)= -1._SP/(conjg(freq_axis(iw))+Af(it)+Bf(it+1)**2*X_t(2))
   do i1=it-1,1,-1
     X_o(1,i1) = 1._SP/(freq_axis(iw)-Af(i1)-Bf(i1+1)**2*X_o(1,i1+1) )
     if (retarded) X_a(1,i1)= -1._SP/(conjg(freq_axis(iw))+Af(i1)+Bf(i1+1)**2*X_a(1,i1+1))
   enddo
   !
   ! Respoinse functions at iteration it-1
   !
   X_o(2,it-1) = 1._SP/(freq_axis(iw)-Af(it-1)-Bf(it)**2*X_t(1))
   if (retarded) X_a(2,it-1)= -1._SP/(conjg(freq_axis(iw))+Af(it-1)+Bf(it)**2*X_t(2))
   do i1=it-2,1,-1
     X_o(2,i1) = 1._SP/(freq_axis(iw)-Af(i1)-Bf(i1+1)**2*X_o(2,i1+1) )
     if (retarded) X_a(2,i1)= -1._SP/(conjg(freq_axis(iw))+Af(i1)+Bf(i1+1)**2*X_a(2,i1+1))
   enddo
   !
   if (use_Cf) then
     !
     ! (A.4), for debug pourposes to be fixed ...
     if(use_A4) then
       X_i(:,1) = (1-(freq_axis(iw)-Af(1))*X_o(:,1))/Bf(2)
       X_i(:,2) = (-Bf(2)*X_o(:,1)-(freq_axis(iw)-Af(2))*X_i(:,1))/Bf(3)
       YbyX(:) = X_i(:,1)*Cf(1)+X_i(:,2)*Cf(2)
       do i1 = 2,it-1
         X_i(1,i1+1) = (-Bf(i1+1)*X_i(1,i1-1)-(freq_axis(iw)-Af(i1+1))*X_i(1,i1))/Bf(i1+2)
         YbyX(1) = YbyX(1) + Cf(i1+1)*X_i(1,i1+1)
       end do
       do i1 = 2,it-2
         X_i(2,i1+1) = (-Bf(i1+1)*X_i(2,i1-1)-(freq_axis(iw)-Af(i1+1))*X_i(2,i1))/Bf(i1+2)
         YbyX(2) = YbyX(2) + Cf(i1+1)*X_i(2,i1+1)
       end do
       X_o(:,1) = YbyX(:)
     endif
     !
     ! (A.6)
     if(use_A6) then
       !
       X_o= conjg(X_o)
       X_i(:,1)= X_o(:,1)
       X_i(:,2)= Bf(2)*X_o(:,2)*X_i(:,1)
       YbyX(:) =  Cf(1)*X_i(:,1)+Cf(2)*X_i(:,2)
       do i1 = 2,it-1
         X_i(1,i1+1) = Bf(i1+1)*X_o(1,i1+1)*X_i(1,i1)
         YbyX(1) = YbyX(1) + Cf(i1+1)*X_i(1,i1+1)
       end do
       do i1 = 2,it-2
         X_i(2,i1+1) = Bf(i1+1)*X_o(2,i1+1)*X_i(2,i1)
         YbyX(2) = YbyX(2) + Cf(i1+1)*X_i(2,i1+1)
       end do
       X_o(:,1) = conjg(YbyX(:))
       !
       if (retarded) then
         X_a= conjg(X_a)
         X_i(:,1)= X_a(:,1)
         X_i(:,2)= Bf(2)*X_a(:,2)*X_i(:,1)
         YbyX(:) = Cf(1)*X_i(:,1)+Cf(2)*X_i(:,2)
         do i1 = 2,it-1
           X_i(1,i1+1) = Bf(i1+1)*X_a(1,i1+1)*X_i(1,i1)
           YbyX(1) = YbyX(1) + Cf(i1+1)*X_i(1,i1+1)
         end do
         do i1 = 2,it-2
           X_i(2,i1+1) = Bf(i1+1)*X_a(2,i1+1)*X_i(2,i1)
           YbyX(2) = YbyX(2) + Cf(i1+1)*X_i(2,i1+1)
         end do
         X_a(:,1) = YbyX(:)
       endif
       !
     endif
     !
   else if (retarded) then
     X_a=conjg(X_a)
   endif
   !
   if (retarded) then
     X_o(1,:)=X_o(1,:)+X_a(1,:)
     X_o(2,:)=X_o(2,:)+X_a(2,:)
   endif
   !
   !
   select case(trim(OBS))
   case("jdos")
     Joint_DOS(iw,i_resp(:))=  -X_o(1:2,1)*Co
   case("optics")
     if(iw==0) then
       para_term_w0(2)=-X_o(1,1)*gauge_factor
       para_term_w0(3)=-X_o(2,1)*gauge_factor
       cycle
     endif
     Resp_ii(iw,i_resp(:))= -X_o(1:2,1)*Co
   case("kerr")
     Resp_ij(iw,i_resp(:))= -X_o(1:2,1)*Co
   case("magnons")
     X_magnons(iw,1,i_resp(:))=-X_o(1:2,1)*Co/(4._SP*pi)
   end select
   !
 enddo
 !
 do iw=1,W%n_freqs
   !
   select case(trim(OBS))
   case("jdos")
     sums=( Joint_DOS(iw,2)+ Joint_DOS(iw,4))/2._SP
     diff=  Joint_DOS(iw,2)- Joint_DOS(iw,4)
   case("optics")
     sums=(Resp_ii(iw,2)+Resp_ii(iw,4))/2._SP
     diff= Resp_ii(iw,2)-Resp_ii(iw,4)
   case("kerr")
     sums=(Resp_ij(iw,2)+Resp_ij(iw,4))/2._SP
     diff= Resp_ij(iw,2)-Resp_ij(iw,4)
   case("magnons")
     sums=(X_magnons(iw,1,2)+X_magnons(iw,1,4))/2._SP
     diff= X_magnons(iw,1,2)-X_magnons(iw,1,4)
   end select
   !
   update_thresh = abs(real(sums))>0._SP .and. abs(aimag(sums))>0._SP
   !
   if (update_thresh) then
     tmp_tresh=max( abs(real(diff)/real(sums)) , abs(aimag(diff)/aimag(sums)) )
     if (Haydock_threshold>0._SP ) reached_threshold=max(reached_threshold,tmp_tresh)
     if (Haydock_threshold<0._SP ) reached_threshold=reached_threshold+tmp_tresh/real(W%n_freqs)
   endif
   !
 enddo
 !
 if (use_Cf) deallocate(X_i)
 !
 contains
   !
   function terminator(x,c1,c2,which)
     !
     complex(SP) :: terminator, f, g, x
     real(SP) :: c1,c2
     integer  :: which
     !
     select case(which)
     case (1) 
       f = x**2 - c1**2 + c2**2
       g = 2._SP*x*c2**2
       terminator = f + sqrt(f**2 -2._SP*x*g) 
       terminator = terminator/g
     case (2)
       f = (x+c1)
       terminator = f + sqrt(f**2 -4._SP*c2**2)
       terminator = terminator/2._SP/c2**2
     case (3)
       f = (x-c1)
       terminator = f + sqrt(f**2 -4._SP*c2**2)
       terminator = terminator/2._SP/c2**2
     end select
   end function terminator
   !
end subroutine
