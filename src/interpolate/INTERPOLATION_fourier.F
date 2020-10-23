!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, AM
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
subroutine INTERPOLATION_fourier(K,VALUES,engre,nvalues,NK,ID)
 !
 use pars,              ONLY:SP,pi,cI,cZERO,DP,rZERO
 use interpolate,       ONLY:nshells,lattice_vectors,int_sop,INTERPOLATE_is_serial,INTERPOLATE_is_quiet,&
&                            INTERP_obj,INTERP_FineGd
 use interpolate_tools, ONLY:make_star
 use R_lattice,         ONLY:bz_samp
 use D_lattice,         ONLY:nsym
 use vec_operate,       ONLY:c2a
 use timing_m,          ONLY:timing
 use LIVE_t,            ONLY:live_timing
 use parallel_m,        ONLY:PP_indexes,PP_indexes_reset
 use parallel_int,      ONLY:PP_redux_wait,PARALLEL_index 
 !
 implicit none
 !
 integer,intent(in)        :: nvalues
 integer,intent(in)        :: NK,ID
 type(bz_samp), intent(in) :: K
 real(SP), intent(out)     :: VALUES(nvalues,NK)
 complex(DP), intent(in)   :: engre(nshells,nvalues)
 !
 ! Work Space
 !
 real(SP), parameter:: twopi=2._SP*pi
 !
 integer            :: i_shell,iv,ik,is,nstar
 real(SP)           :: star_vec(3,nsym),v(3)
 complex(SP)        :: F_star(nshells)
 type(PP_indexes)   :: px
 !
 call timing('INTERPOLATE Fourier',OPR='start')
 !
 ! Parallel setup and distribution
 !---------------------------------
 if (.not.INTERPOLATE_is_serial) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/Nk/))
 endif
 VALUES=rZERO
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing("Interpolate@"//trim(INTERP_obj(ID)%what)//" Fourier",nshells)
 !
 do ik=1,Nk
   F_star=cZERO
   if (.not.INTERPOLATE_is_serial) then
     if (.not.px%element_1D(ik)) cycle
   endif
   !
   if (trim(INTERP_FineGd)=="NONE") call c2a(v_in=K%pt(ik,:),       v_out=v,mode='ki2a')
   if (trim(INTERP_FineGd)=="IBZ")  call c2a(v_in=K%FGibz%pt(ik,:), v_out=v,mode='ki2a')
   if (trim(INTERP_FineGd)=="BZ")   call c2a(v_in=K%FGbz%pt(ik,:),  v_out=v,mode='ki2a')
   if (trim(INTERP_FineGd)=="BARE") call c2a(v_in=K%FGbare%pt(ik,:),v_out=v,mode='ki2a')
   !
   do i_shell=1,nshells
     call make_star(lattice_vectors(:,i_shell),nsym,int_sop,nstar,star_vec)  
     do is=1,nstar
       F_star(i_shell)=F_star(i_shell) + exp(cI*twopi*dot_product(v(:),star_vec(:,is)))
     enddo
     F_star(i_shell)=F_star(i_shell)/real(nstar)
   enddo
   !
   do iv=1,nvalues
     VALUES(iv,ik) = sum(engre(1:nshells,iv)*F_star(1:nshells))
   enddo
   !
   if (.not.INTERPOLATE_is_quiet) call live_timing(steps=1)
   !
 enddo
 !
 if (.not.INTERPOLATE_is_serial) call PP_redux_wait(VALUES)
 if (.not.INTERPOLATE_is_quiet)  call live_timing()
 !
 call timing('INTERPOLATE Fourier',OPR='stop')
 !
end subroutine INTERPOLATION_fourier
