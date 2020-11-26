!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG DS
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
subroutine K_Haydock_response(iq,it,Cf_size,mode,W,Af,Bf,Cf,reached_threshold)
 !
 use pars,           ONLY:SP,schlen,cI,cZERO,cONE,pi
 use X_m,            ONLY:Epsilon_ii,Joint_DOS,X_drude_term,global_gauge,skip_cond_sum_rule
 use R_lattice,      ONLY:bare_qpg
 use frequency,      ONLY:w_samp
 use BS,             ONLY:BSE_mode
 use BS_solvers,     ONLY:BSS_eels_to_eps,Co_factor,diam_term_exact,Haydock_v0_mod,&
 &                        Haydock_terminator,Haydock_threshold
#if defined _KERR
 use X_m,            ONLY:X_magnons,Epsilon_ij
 use BS_solvers,     ONLY:B_Hall
 use BS,             ONLY:l_BS_anomalous_Hall
#endif
 use descriptors,    ONLY:IO_desc_add
 !
 implicit none
 !
 type(w_samp),intent(in) :: W
 integer,     intent(in) :: iq,it,Cf_size
 character(*),intent(in) :: mode
 real(SP),    intent(in) :: Bf(it+1),Af(it)
 complex(SP), intent(in) :: Cf(Cf_size)
 real(SP),    intent(out):: reached_threshold
 !
 ! Work Space
 !
 logical     :: update_thresh
 integer     :: i1,iw,iw_start,i_eps(2)
 real(SP)    :: Co,tmp_tresh
 complex(SP) :: sums,diff
 !
 complex(SP) :: para_term_w0(2),diam_term_used(2)
 complex(SP) :: gauge_factor
 complex(SP) :: freq_axis(0:W%n_freqs)
 !
 complex(SP) :: X_o(4,it), YbyX(2)
 complex(SP),allocatable :: X_i(:,:)
 !
 ! related to terminator
 !
 complex(SP) :: X_t(2)
 real(SP)    :: Av1, Av2
 integer, parameter :: RES = 3, ARES = 2, COUPL = 1, MIN_ITER = 10
 logical     :: UseTerminator
 logical     :: retarded, coupling
 !
 Co=Co_factor*Haydock_v0_mod**2
 reached_threshold=0._SP
 !
 X_t =cZERO
 UseTerminator = .false.  
 !
 coupling= Cf_size>2
 retarded= (trim(BSE_mode)=='retarded'.and..not.coupling)
 !
 i_eps(1)=2
 i_eps(2)=4
 !
 para_term_w0  =cZERO
 diam_term_used=cZERO
 !
 if(.not.trim(global_gauge)=='velocity') gauge_factor=cONE*Co/bare_qpg(iq,1)**2
 !
 if (it > MIN_ITER.and.Haydock_terminator) UseTerminator = .true.
 if (coupling) allocate(X_i(2,it))
 if (Useterminator) then 
   if (.not.coupling) then
     Av1 = 0._SP
     Av2 = 0._SP
     do i1 = 1, it
       Av1 = Av1 + Af(i1)
       Av2 = Av2 + Bf(i1)
     end do
     Av1 = Av1/it
     Av2 = Av2/it
   else
     Av1 = 0._SP
     Av2 = 0._SP
     do i1 = 2,it+1,2
       Av1 = Av1 + Bf(i1)
       Av2 = Av2 + Bf(i1+1)
     end do
     Av1 = Av1/(it/2+mod(it,2))
     Av2 = Av2/(it/2)
   end if
 end if
 !
 iw_start=1
 if(trim(global_gauge)=='velocity') iw_start=0
 !
 freq_axis(0 )=cZERO
 freq_axis(1:)=W%p(:)
 !
 do iw=iw_start,W%n_freqs
   X_o=cZERO
   if (UseTerminator) then 
     if (.not.coupling) then
       X_t(1) = terminator(freq_axis(iw), Av1,Av2, RES)
       if (retarded) X_t(2) = terminator(freq_axis(iw),-Av1,Av2,ARES)
     endif
     if (coupling.and.(mod(it,2)==0)) X_t(1) = terminator(freq_axis(iw),Av1,Av2,COUPL)
     if (coupling.and.(mod(it,2)==1)) X_t(1) = terminator(freq_axis(iw),Av2,Av1,COUPL)
   endif
   X_o(1:2,it) = 1._SP/(freq_axis(iw)-Af(it)-Bf(it+1)**2*X_t(1))
   if (retarded) X_o(3:4,it)= 1._SP/(freq_axis(iw)+Af(it)-Bf(it+1)**2*X_t(2))
   do i1=it-1,1,-1
     X_o(1,i1) = 1._SP/(freq_axis(iw)-Af(i1)-Bf(i1+1)**2*X_o(1,i1+1) )
     if (retarded) X_o(3,i1)= 1._SP/(freq_axis(iw)+Af(i1)-Bf(i1+1)**2*X_o(3,i1+1))
   enddo
   do i1=it-2,1,-1
     X_o(2,i1) = 1._SP/(freq_axis(iw)-Af(i1)-Bf(i1+1)**2*X_o(2,i1+1) )
     if (retarded) X_o(4,i1)= 1._SP/(freq_axis(iw)+Af(i1)-Bf(i1+1)**2*X_o(4,i1+1))
   enddo
   if (coupling) then
     X_i(:,1) = -X_o(:2,1)
     X_i(:,2) = -Bf(2)*X_o(:2,2)*X_i(:,1)
     YbyX(:) = X_i(:,1)*Cf(1) + X_i(:,2)*Cf(2)
     do i1 = 2,it-1
       X_i(1,i1+1) = -Bf(i1+1)*X_o(1,i1+1)*X_i(1,i1)
       YbyX(1) = YbyX(1) + Cf(i1+1)*X_i(1,i1+1)
     end do
     do i1 = 2,it-2
       X_i(2,i1+1) = -Bf(i1+1)*X_o(2,i1+1)*X_i(2,i1)
       YbyX(2) = YbyX(2) + Cf(i1+1)*X_i(2,i1+1)
     end do
     X_o(:2,1) = YbyX(:)
   endif
   !
   if(trim(global_gauge)=='velocity'.and.iw==0) then
     if (.not.retarded) para_term_w0(:)=X_o(1:2,1)
     if (     retarded) para_term_w0(:)=X_o(1:2,1)-X_o(3:4,1)
     cycle
   endif
   !
   select case(trim(mode))
   case("jdos")
     if (.not.retarded)  Joint_DOS(iw,i_eps(:))=-(X_o(1:2,1)           )
     if (     retarded)  Joint_DOS(iw,i_eps(:))=-(X_o(1:2,1)-X_o(3:4,1))
   case("abs")
     if (.not.retarded) Epsilon_ii(iw,i_eps(:))=-(X_o(1:2,1)           )
     if (     retarded) Epsilon_ii(iw,i_eps(:))=-(X_o(1:2,1)-X_o(3:4,1))
#if defined _KERR
   case("kerr")
     if (.not.retarded) Epsilon_ij(iw,i_eps(:))=-(X_o(1:2,1)           )
     if (     retarded) Epsilon_ij(iw,i_eps(:))=-(X_o(1:2,1)-X_o(3:4,1))
   case("magnons")
     if (.not.retarded) X_magnons(iw,1,i_eps(:))=-(X_o(1:2,1)           )*Co/(4._SP*pi)
     if (     retarded) X_magnons(iw,1,i_eps(:))=-(X_o(1:2,1)-X_o(3:4,1))*Co/(4._SP*pi)
#endif
   end select
   !
 enddo
 !
 if (trim(global_gauge)=="velocity") then
   if (     skip_cond_sum_rule) diam_term_used(:)=diam_term_exact
   if (.not.skip_cond_sum_rule) diam_term_used(:)=para_term_w0(:)
 endif
 !
 do iw=1,W%n_freqs
   !
   if (trim(global_gauge)=="velocity") gauge_factor=Co/W%p(iw)**2
   !
   if (trim(mode)=="abs")  Epsilon_ii(iw,i_eps(:))=(Epsilon_ii(iw,i_eps(:))+diam_term_used(:))*gauge_factor
#if defined _KERR
   if (trim(mode)=="kerr") Epsilon_ij(iw,i_eps(:))=(Epsilon_ij(iw,i_eps(:))+diam_term_used(:))*gauge_factor
#endif
   !
   if (trim(mode)=="abs") then
     if(BSS_eels_to_eps) Epsilon_ii(iw,i_eps(:))=(1._SP-Epsilon_ii(iw,i_eps(:)))**(-1)-1._SP
     Epsilon_ii(iw,i_eps(:))=Epsilon_ii(iw,i_eps(:))+X_drude_term(iw)*4._SP*pi/bare_qpg(iq,1)**2
   endif
   !
#if defined _KERR
   !
   ! I add the term describing the Anomalous Hall effect which is
   ! missing in the length gauge (this should be zero for systems with a gap)
   !
   if (l_BS_anomalous_Hall.and.trim(mode)=="kerr") then
     if (trim(global_gauge)=='length') then
       B_Hall(2)=B_Hall(2)*Co/bare_qpg(iq,1)**2
       Epsilon_ij(iw,2)=Epsilon_ij(iw,2)+B_Hall(2)/W%p(iw)
     else if (trim(global_gauge)=='velocity') then
       B_Hall(2)=B_Hall(2)*Co
     endif
   endif
   !
#endif
   !
   select case(trim(mode))
   case("jdos")
     sums=( Joint_DOS(iw,2)+ Joint_DOS(iw,4))/2._SP
     diff=  Joint_DOS(iw,2)- Joint_DOS(iw,4)
   case("abs")
     sums=(Epsilon_ii(iw,2)+Epsilon_ii(iw,4))/2._SP
     diff= Epsilon_ii(iw,2)-Epsilon_ii(iw,4)
#if defined _KERR
   case("kerr")
     sums=(Epsilon_ij(iw,2)+Epsilon_ij(iw,4))/2._SP
     diff= Epsilon_ij(iw,2)-Epsilon_ij(iw,4)
   case("magnons")
     sums=(X_magnons(iw,1,2)+X_magnons(iw,1,4))/2._SP
     diff= X_magnons(iw,1,2)-X_magnons(iw,1,4)
#endif
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
 if (coupling) deallocate(X_i)
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
