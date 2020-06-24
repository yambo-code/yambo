!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine k_random(No_Weight)
 !
 use pars,                ONLY:SP,DP,lchlen,schlen
 use stderr,              ONLY:set_real_printed_length
 use com,                 ONLY:msg,of_open_close
 use vec_operate,         ONLY:v_norm,c2a,k2bz
 use YPP,                 ONLY:BZ_random_nkpts,gamma_radius,K_transform
 use LIVE_t,              ONLY:live_timing
 !
 implicit none
 !
 logical, intent(in)     :: No_Weight
 !
 ! Work Space
 !
 real(SP)     ::rand_k(BZ_random_nkpts+1,3)
 integer      ::i1
 character(lchlen) :: k_rand_fname
 character(schlen) :: dump
 !
 ! Random Generator
 !
 character(12)      :: ch(3)
 integer            :: iseed(8),k_counter
 logical            :: impose_gamma_radius
 !
 impose_gamma_radius=gamma_radius>0.
 !
 call date_and_time(ch(1),ch(2),ch(3),iseed)
 iseed=iabs(iseed)
 ! iseed(4) must be odd
 iseed(4)=2*(iseed(4)/2)+1
 !
 call section('*',"== Random k-points generator ==")
 !
 call live_timing('Random k-points',BZ_random_nkpts)
 !
 call set_real_printed_length(f_length=20,g_length=20)
 !
 k_rand_fname="random_k_pts"
 call of_open_close(k_rand_fname,'ot')
 if(No_Weight) then
   call msg('o random',"#",(/"c1","c2","c3"/),INDENT=0,USE_TABS=.true.)
 else 
   call msg('o random',"#",(/"c1","c2","c3","wt"/),INDENT=0,USE_TABS=.true.)
 endif
 !
 k_counter=0
 !
 do while ( k_counter < BZ_random_nkpts )
   !
   if (impose_gamma_radius) then
     !
     ! select only point with |k| < gamma_radius [a.u.]
     !
     call build_spherical_k()
     !
   else
     !
     call build_3D_k()
     !
   endif
   !
   call K_transform(rand_k(k_counter+1,:),'iku')
   !
   if(No_Weight) then
     write (dump,'(3f12.7)') rand_k(k_counter+1,:)
   else
     write (dump,'(3f12.7,3x,i2)') rand_k(k_counter+1,:),1
   endif
   call msg('o random',"",trim(dump),USE_TABS=.true.)
   !
   k_counter=k_counter+1
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call of_open_close(k_rand_fname)
 call live_timing()
 call set_real_printed_length()
 !
 return
 !
 contains
   !
   subroutine build_3D_k()
     real(DP), external :: dlaran
     do i1=1,3
       rand_k(k_counter+1,i1)=(dlaran(iseed(4:))-1./2.)
     enddo
     call c2a(v_in=rand_k(k_counter+1,:),mode="ka2i")
     call k2bz(v_in=rand_k(k_counter+1,:))
   end subroutine
   !
   subroutine build_spherical_k()
     real(DP), external :: dlaran
     real(SP)           :: k_module
     k_module=gamma_radius+1.
     do while ( k_module > gamma_radius) 
       do i1=1,3
         rand_k(k_counter+1,i1)=gamma_radius*(2.*dlaran(iseed(4:))-1.)
       enddo
       k_module=v_norm(rand_k(k_counter+1,:))
       call c2a(v_in=rand_k(k_counter+1,:),mode="kc2a")
       call c2a(v_in=rand_k(k_counter+1,:),mode="ka2i")
     enddo
   end subroutine
   !
end subroutine
