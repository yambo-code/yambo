!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,DS,AG
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
subroutine X_Drude(iq,Xen,Xk,Xw,Wd,drude_GreenF)
 !
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,spin_occ,n_sp_pol,filled_tresh
 use R_lattice,     ONLY:bz_samp,q_norm,RIM_W_for_graph
 use X_m,           ONLY:use_X_DbGd,l_drude,drude_n_states
 use parser_m,      ONLY:parser
 use drivers,       ONLY:l_ppa,l_mpa !DALV: needed for the complex sampling in MPA
 !
 implicit none
 !
 type(levels), intent(in)   :: Xen
 type(bz_samp),intent(in)   :: Xk
 type(w_samp), intent(in)   :: Xw
 !
 integer,      intent(in)   :: iq
 complex(SP),  intent(inout):: Wd
 complex(SP),  intent(out)  :: drude_GreenF(Xw%n_freqs)
 !
 real(SP)             :: f_eh,f_occ
 logical              :: Drude_term(4),l_Jellium,l_Classical
 integer              :: iw,ik,ib,is
 !
 drude_GreenF = cZERO
 !
 ! Compute metallic occupations
 !
 drude_n_states=0
 f_eh=0._SP
 do is=1,n_sp_pol
   do ik=1,Xen%nk
     do ib=1,Xen%nb
       !
       f_occ=Xen%f (ib,ik,is)
       !
       if (            f_occ < filled_tresh ) cycle
       if ( spin_occ - f_occ < filled_tresh ) cycle
       !
       drude_n_states=drude_n_states+int(Xk%weights(ik)*real(Xk%nbz,SP))
       f_eh=f_eh+f_occ*(spin_occ-f_occ)*Xk%weights(ik)*real(Xk%nbz,SP)
       !
     enddo
   enddo
 enddo
 !
 if(drude_n_states>0) f_eh=f_eh/real(drude_n_states,SP)
 !
 ! Drude Contrib. if all of following conditions hold
 !====================================================
 !  . Non zero Drude Freq. or graphene model
 !  . Metallic system 
 !  . Metallic occupations
 !  . Optical response @ q = Gamma
 !
 Drude_term(1) = ((real(Wd)>0._SP .and. aimag(Wd)>0._SP) .or. (RIM_W_for_graph))
 Drude_term(2) = any( (/Xen%nbf(:n_sp_pol)/=Xen%nbm(:n_sp_pol)/) )
 Drude_term(3) = (f_eh>0._SP) 
 Drude_term(4) = (iq==1)
 !
 ! Checks
 !
 if (.not.Drude_term(4)) then
   call warning(' Not q=0. I should not be here!')
 endif
 !
 if (.not.Drude_term(1).and. all((/Drude_term(2:3)/)) .and..not.use_X_DbGd ) then
   call warning(' The system is a metal but Drude term not included.')
 endif
 !
 if(      Drude_term(1) .and. .not.Drude_term(2) ) then
   call warning(' System is not a metal. Drude term not included')
 endif
 !
 if(      Drude_term(1) .and.      Drude_term(2) .and. .not.Drude_term(3) ) then
   call warning(' Too low ElecTemp. Drude term not included.')
 endif
 !
 l_drude=all(Drude_term)
 if(.not.l_drude) return
 !
 if(RIM_W_for_graph) then
   l_Classical = .False.
   l_Jellium = .False.
 else
   call parser('DrClassic',l_Classical)
   l_Jellium=.not.l_Classical
 end if
 if(.not.all(Drude_term)) return
 !
 call msg(' rs','[X/K] Drude nstates            ',drude_n_states)
 call msg(' rs','[X/K] Drude average occ        ',f_eh)
 call msg('nrs','[X/K] Drude energies           ',(/real(Wd),aimag(Wd)/)*HA2EV,'[eV]')
 if(RIM_W_for_graph) call msg('r', 'Graphene analytical model of intraband transitions')
 !
 do iw=1,Xw%n_freqs
   !
   !DALV: generalization for the complex sampling in MPA
   !
   if(l_ppa.or.l_mpa) then
     !
     if(l_Jellium)   drude_GreenF(iw)=  Jellium_drude(Xw%p(iw),Wd,q_norm(1))
     if(l_Classical) drude_GreenF(iw)=Classical_drude_T(Xw%p(iw),Wd,q_norm(1))
     !
   else
     !
     if(l_Jellium)   drude_GreenF(iw)=  Jellium_drude(real(Xw%p(iw),SP)+cZERO,Wd,q_norm(1))
     if(l_Classical) drude_GreenF(iw)=Classical_drude(real(Xw%p(iw),SP)+cZERO,Wd,q_norm(1))
     !
   endif
   !
 enddo
 !
 contains
  !
  function Jellium_drude(W,Wd,q0_norm)
   !
   !See R.D. Mattuck "A guide to Feynmann diagrams in the Many-Body problem", pag. 197.
   !Jellium Xo propagator with a given plasma frequency.
   !The propagator is T-ordered.
   !Davide: If Drude is used at finite temperature / with fractional occupations
   !        I would need the retarded propagator ... occupations are not used here
   !
   use pars,  ONLY:SP,pi,cI
   implicit none
   !
   ! DALV: W is in general complex for the sampling in MPA
   !
   real(SP)    :: q0_norm
   complex(SP) :: Wd,W
   !
   ! Work Space
   !
   complex(SP) :: xi1,xi2,Jellium_drude
   real(SP)    :: Kf,rxi,fac
   !
   Kf =((3._SP*pi*real(Wd)**2)/4._SP)**(1._SP/3._SP)
   fac=Kf*q0_norm
   xi1=W/fac+cI*aimag(Wd)
   xi2=W/fac-cI*aimag(Wd)
   rxi=abs(xi1)
   !
   if (abs(rxi)<=10._SP) then
     Jellium_drude=-Kf/(2._SP*pi**2)*( 2._SP + xi1*log((xi1-1._SP)/(xi1)) - xi2*log((xi2+1._SP)/(xi2)) )
   else
     Jellium_drude=-Kf/(2._SP*pi**2)*(-1._SP/(2._SP*xi1)+1._SP/(2._SP*xi2)-1._SP/(3._SP*xi1**2)-1._SP/(3._SP*xi2**2))
   endif
   !
  end function Jellium_drude
  !
  !
  function Classical_drude(W,Wd,q0_norm)
   !  
   use pars,  ONLY:SP,pi,cI,cZERO
   implicit none
   !
   ! DALV: W,W_ are in general complex for the sampling in MPA
   !
   real(SP)    :: q0_norm
   complex(SP) :: Wd,W
   !
   ! Work Space
   !
   complex(SP) :: W_,xi1,Classical_drude
   real(SP)    :: Kf,rxi,fac
   !
   W_=W
   if (abs(real(W_))<=1.E-8) W_=1.E-8 + CI*aimag(W_)
   !
   Kf =((3._SP*pi*real(Wd)**2)/4._SP)**(1._SP/3._SP)
   fac=Kf*q0_norm
   xi1=W/fac+cI*aimag(Wd)
   rxi=abs(W/fac)
   !
   if (abs(rxi)<=10._SP) then
       ! eps_D(w)= -w_p^2/(W*(i*g_p-W)) with Wd=(w_p,g_p)
       Classical_drude =-real(Wd)**2/(W_*(cI*aimag(Wd)-W_) )
   else
       ! DALV: Taylor expansion at W_=infinity
       Classical_drude =-real(Wd)**2*(1._SP+cI*aimag(Wd)/W_)/(W_**2._SP)
   endif
   !
   ! X_D(w)=-q^2/(4pi) eps_D(w)
   Classical_drude =  -Classical_drude* ( q0_norm**2/(4._SP*pi) )
   !
  end function Classical_drude
  !
  !
  function Classical_drude_T(W,Wd,q0_norm)
   !
   !
   use pars,  ONLY:SP,pi,cI,cZERO
   !
   implicit none
   !
   real(SP)    :: q0_norm !DALV: W,W_ are in general complex for the sampling in MPA
   complex(SP) :: Wd,W
   !
   ! Work Space
   !
   complex(SP) :: W_,xi1,Classical_drude_T
   !
   W_=W
   if (abs(real(W_))<=1.E-8) W_=1.E-8 + CI*aimag(W_)
   !
   ! eps_D(w)= -w_p^2/(W*(i*g_p-W)) with Wd=(w_p,g_p)
   Classical_drude_T =real(Wd)*(real(Wd)-CI*aimag(Wd))/(W_**2)
   !
   ! X_D(w)=-q^2/(4pi) eps_D(w)
   Classical_drude_T =  -Classical_drude_T* ( q0_norm**2/(4._SP*pi) )
   !
  end function Classical_drude_T
  !
  !
  function Graphene_drude(Wd,q0_norm)
   !
   use pars,       ONLY:SP,pi
   use R_lattice,  ONLY:idir
   use D_lattice,  ONLY:alat
   implicit none
   !
   real(SP)    :: q0_norm
   complex(SP) :: Graphene_drude,Wd
   real(SP)    :: lcut
   !
   !Get graph_v in a.u.
   !
   lcut = alat(idir(1))
   !
   !Analytical model for graphene:
   !X_D(w=0) = -1/(4*vf)*q/L
   !real(Wd) = 1/(4*vf)
   Graphene_drude = -real(Wd)*q0_norm/lcut
   !
  end function Graphene_drude
  !
end subroutine
