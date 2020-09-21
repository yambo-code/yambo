!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine rim(mode)
 !
 use pars,          ONLY:SP,pi,DP,schlen
 use com,           ONLY:msg
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 use LIVE_t,        ONLY:live_timing
 use vec_operate,   ONLY:v_norm,iku_v_norm
 use D_lattice,     ONLY:alat
 use R_lattice,     ONLY:RL_vol,k_grid_uc_vol,k_grid_b,nqbz,&
&                        nqibz,RIM_epsm1,RIM_is_diagonal,RIM_RL_vol,&
&                        RIM_n_rand_pts,RIM_ng,RIM_qpg,RIM_W_is_diagonal,&
&                        RIM_id_epsm1_reference,RIM_anisotropy,q_pt,RIM_W,&
&                        cut_is_slab,idir
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 character(1)       :: mode
 !
 ! Work Space
 !
 type(PP_indexes)::px
 integer   :: iq
 real(SP)  :: em1_anis(3),G_radii,G_circ
 !
 ! Random generator
 !
 real(SP)  :: first_neighbour(26,3)
 integer   :: N_out,N_in,N_out_G,rep_factor,inn1,inn2,inn3,ic
 real(SP)  :: v1(3),v2(3),qr(RIM_n_rand_pts,3),v1_norm(2)
 character(12)      :: ch(3)
 integer            :: iseed(8)
 real(DP), external :: dlaran
 character(schlen) :: msg_dum(2)
 ! 
 if (mode == "x") then
   call timing('RIM',OPR='start')
 else 
   call timing('RIM-W',OPR='start')
 end if
 !
 call PP_indexes_reset(px)
 !
 ! Anisotropy Setup
 !
 ! em1_anis refers to the X part only of epsm1 as in the self-energy
 ! the bare part is embodied in the exchange.
 !
 em1_anis=RIM_epsm1(:)-1.
 if (RIM_id_epsm1_reference<0.or.RIM_id_epsm1_reference>3) RIM_id_epsm1_reference=0
 if (RIM_id_epsm1_reference==0) em1_anis=0.
 if (RIM_id_epsm1_reference>0 ) em1_anis=em1_anis/em1_anis(RIM_id_epsm1_reference)
 !
 ! Filling the small BZ with the random grid
 !===========================================
 !
 if (mode == "x") then
   !
   if (RIM_is_diagonal) call msg('r','* Diagonal components only detected *')
   call section('+','RIM initialization')
   call msg('nr','8 x (sBL volume)    [au]:',8.*k_grid_uc_vol)
   call msg('r', 'sBZ random points       :',RIM_n_rand_pts)
   !
 else 
   call section('+','RIM-W initialization')
   if (RIM_W_is_diagonal) call msg('r','* Diagonal components only detected *')
 end if  
 !
 ! Random generator seed
 !
 call date_and_time(ch(1),ch(2),ch(3),iseed)
 iseed=iabs(iseed)
 ! iseed(4) must be odd
 iseed(4)=2*(iseed(4)/2)+1
 !
 ! First neighbours of the gamma point in the k_grid_b units
 !
 ic=0
 do inn1=-1,1
   do inn2=-1,1
    do inn3=-1,1
      if (all((/inn1,inn2,inn3/)==0)) cycle
      ic=ic+1
      first_neighbour(ic,:)=matmul(transpose(k_grid_b),(/inn1,inn2,inn3/))
     enddo
   enddo
 enddo
 !
 ! Loop setup
 !
 N_in=1
 N_out=0
 rep_factor=RIM_n_rand_pts/100
 if (rep_factor==0) rep_factor=1
 !
 call live_timing('Random points',RIM_n_rand_pts/rep_factor)
 loop: do while(.not.N_in==RIM_n_rand_pts+1)
   !
   do ic=1,3
     v2(ic)=2.*dlaran(iseed(4:))-1
   enddo
   N_out=N_out+1
   !
   ! From rlu in the k_grid_b units (v2) to Cartesian (v1)
   !
   v1=matmul(transpose(k_grid_b),v2) 
   v1_norm(1)=v_norm(v1)
   do inn1=1,26
     v1_norm(2)=v_norm(v1-first_neighbour(inn1,:))
     if (v1_norm(2)<v1_norm(1)) cycle loop
     !if (v1_norm(2)<v1_norm(1)-1.E-5) cycle loop
     !
     ! The small number 1E-5 seems unessential and bring to erroneous integration in
     ! the case of large samplings.
     ! 
   enddo
   qr(N_in,:)=v1*alat(:)/2./pi
   !
   N_in=N_in+1
   if (mod(N_in,rep_factor)==0) call live_timing(steps=1)
 enddo loop
 !
 !Only the 2D-BZ is sampled for 2D systems
 if (cut_is_slab) qr(:,idir(1)) = 0._SP
 !
 call live_timing()
 call msg('r','Points outside the sBZ  :',N_out)
 !  
 !Integrated RL VOLUME 
 !
 RIM_RL_vol=8.*k_grid_uc_vol*real(RIM_n_rand_pts)/real(N_out)*real(nqbz)
 !if (cut_is_slab) RIM_RL_vol = RIM_RL_vol/2._SP
 call msg('r', 'RL volume           [au]:',RL_vol)
 call msg('rn','Integrated volume   [au]:',RIM_RL_vol)
 !
 call section('=','RIM integrals')
 !
 if (mode == "x") then
   YAMBO_ALLOC(RIM_qpg,(nqibz,RIM_ng,RIM_ng))
   RIM_qpg=0._SP
 else if (mode == "c") then
   YAMBO_ALLOC(RIM_W,(nqibz,RIM_ng,RIM_ng))
   RIM_W=0._SP  
 end if
 !
 call PARALLEL_index(px,(/nqibz/))
 call live_timing('Momenta loop',px%n_of_elements(myid+1))

 do iq=1,nqibz
   if (.not.px%element_1D(iq)) cycle
   !
   if (mode == "x") then
     call rim_integrate_v(iq,qr,em1_anis,N_out,N_out_G,G_radii,G_circ)
     !
     if (iq==1) then
       call msg('r','Gamma point circle area         [au]:',G_circ)
       call msg('r','Gamma point sphere radius         [au]:',G_radii)
       call msg('r','Points outside the sphere             :',n_out_G)
       call msg('r','[Int_sBZ(q=0) 1/q^2]*(Vol_sBZ)^(-1/3) =',&
&                     4.*pi**3.*RIM_qpg(1,1,1)*k_grid_uc_vol**(-1./3.))
       call msg('r','                               should be <',7.7956_SP)
       if (RIM_id_epsm1_reference/=0) call &
&           msg('r','Anisotropy correction            [o/o]:',&
&                 (RIM_anisotropy-RIM_qpg(iq,1,1))/RIM_qpg(iq,1,1)*100.)
     end if
   else
     !
     call msg('r', "Evaluating integrals for q = ", iq)
     call rim_integrate_w(iq,qr,N_out,N_out_G)
     !
   end if
   call live_timing(steps=1)
 enddo

 call live_timing()
 !
 if (mode == "x") then
   call PP_redux_wait(RIM_qpg)
 else if (mode == "c") then
   call PP_redux_wait(RIM_W)
 end if
 call PP_redux_wait(RIM_anisotropy)
 !
 ! CLEAN
 !
 call PP_indexes_reset(px)
 !
 if (mode == "x") then
   call timing('RIM',OPR='stop')
 else 
   call timing('RIM-W',OPR='stop')
 end if
 !
end subroutine
