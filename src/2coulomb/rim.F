!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine rim()
 !
 use pars,          ONLY:SP,pi,DP
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,master_node,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use timing,        ONLY:live_timing
 use vec_operate,   ONLY:c2a
 use R_lattice,     ONLY:RL_vol,k_grid_uc_vol,k_grid_b,bz_samp,nqbz,&
&                        nqibz,RIM_epsm1,RIM_is_diagonal,RIM_RL_vol,&
&                        RIM_n_rand_pts,RIM_ng,RIM_qpg,&
&                        RIM_id_epsm1_reference,RIM_anisotropy
 implicit none
 !
 ! Work Space
 !
 type(pp_indexes)::px
 integer   :: i1,i2,i3,i4,i5,N_out,N_out_G,rep_factor
 real(SP)  :: v1(3),v2(3),r1,r2,qr(RIM_n_rand_pts,3),em1_anis(3),&
&             G_sph_radii,anisotropy
 real(DP)  :: dlaran,rr
 integer, parameter :: ni = 2
 integer, parameter :: iseed(4) = (/11,123,44,1546/)
 !
 call pp_indexes_reset(px)
 !
 ! Anisotropy Setup
 !
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
 !
 call section('+','RIM initialization')
 if (RIM_is_diagonal) call msg('r','* Diagonal components only detected *')
 !
 call msg('nr','8 x (sBL volume)    [au]:',8.*k_grid_uc_vol)
 call msg('r', 'sBZ random points       :',RIM_n_rand_pts)
 !
 i1=1
 N_out=0
 rep_factor=1000
 if (RIM_n_rand_pts<=1000) rep_factor=RIM_n_rand_pts/10
 if (rep_factor==0) rep_factor=1
 call live_timing('Random points',RIM_n_rand_pts/rep_factor,SERIAL=.true.)
 loop: do while(.not.i1==RIM_n_rand_pts+1)
   do i2=1,3
     !
     call random_number(rr)
     v2(i2)=(2.*rr-1.)
     !
     ! lapack
     !    v2(i2)=(2.*dlaran(iseed)-1.)
     !
   enddo
   N_out=N_out+1
   v1=matmul(transpose(k_grid_b),v2) ! cartesian
   r1=sqrt(dot_product(v1,v1))
   do i3=-ni,ni
     do i4=-ni,ni
       do i5=-ni,ni
         v1=matmul(transpose(k_grid_b),v2-(/i3,i4,i5/))
         r2=sqrt(dot_product(v1,v1))
         if (r2<r1-1.E-5) cycle loop
       enddo
     enddo
   enddo
   v1=matmul(transpose(k_grid_b),v2)
   call c2a(k_grid_b,v1,qr(i1,:),'kc2i')
   i1=i1+1
   if (mod(i1,rep_factor)==0) call live_timing(steps=1)
 enddo loop
 call live_timing()
 call msg('r','Points outside the sBZ  :',N_out)
 !  
 !Integrated RL VOLUME 
 !
 RIM_RL_vol=8.*k_grid_uc_vol*real(RIM_n_rand_pts)/real(N_out)*real(nqbz)
 call msg('r', 'RL volume           [au]:',RL_vol)
 call msg('rn','Integrated volume   [au]:',RIM_RL_vol)
 !
 call section('=','RIM integrals')
 !
 allocate(RIM_qpg(nqibz,RIM_ng,RIM_ng))
 RIM_qpg=0._SP
 call mem_est("RIM_qpg",(/size(RIM_qpg)/),(/SP/))
 !
 call par_indexes(px,(/nqibz/))
 call live_timing('Momenta loop',px%stps(myid+1))
 do i1=1,nqibz
   if (.not.px%i1p(i1)) cycle
   !
   call rim_integrate(i1,qr,em1_anis,N_out,N_out_G,G_sph_radii)
   !
   if (i1==1) then
     call msg('r','Gamma point sphere radius         [au]:',G_sph_radii)
     call msg('r','Points outside the sphere             :',n_out_G)
     call msg('r','[Int_sBZ(q=0) 1/q^2]*(Vol_sBZ)^(-1/3) =',&
&                     4.*pi**3.*RIM_qpg(1,1,1)*k_grid_uc_vol**(-1./3.))
     call msg('r','                               should be <',7.7956_SP)
     r1=(RIM_anisotropy-RIM_qpg(i1,1,1))/RIM_qpg(i1,1,1)*100.
     if (RIM_id_epsm1_reference/=0) call &
&         msg('r','Anisotropy correction            [o/o]:',r1)
   endif
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 call pp_redux_wait(RIM_qpg)
 call pp_redux_wait(RIM_anisotropy)
 !
 ! CLEAN
 !
 call pp_indexes_reset(px)
 !
 end subroutine
