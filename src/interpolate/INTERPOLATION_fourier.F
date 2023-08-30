!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AM
!
subroutine INTERPOLATION_fourier(K,VALUES,engre,nvalues,NK,ID)
 !
 use pars,              ONLY:SP,pi,cI,cZERO,DP,rZERO
 use interpolate,       ONLY:nshells,lattice_vectors,int_sop,INTERPOLATE_is_serial,INTERPOLATE_is_quiet,&
&                            INTERP_obj,INTERP_FineGd,INTERPOLATE_use_symm
 use interpolate_tools, ONLY:make_star
 use R_lattice,         ONLY:bz_samp
 use D_lattice,         ONLY:nsym
 use vec_operate,       ONLY:c2a
 use timing_m,          ONLY:timing
 use LIVE_t,            ONLY:live_timing
 use parallel_m,        ONLY:PP_indexes,PP_indexes_reset,myid
 use parallel_int,      ONLY:PP_redux_wait,PARALLEL_index 
 !
#include<memory.h>
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
 integer               :: i_shell,iv,ik,is,nstar
 integer               :: nsym_interp
 real(SP)              :: v(3)
 real(SP), allocatable :: star_vec(:,:)
 complex(SP)           :: F_star(nshells)
 type(PP_indexes)      :: px
 integer               :: n_of_elements
 !
 call timing('INTERPOLATE Fourier',OPR='start')
 !
 nsym_interp=nsym
 if(.not.INTERPOLATE_use_symm) nsym_interp=1
 !
 YAMBO_ALLOC(star_vec,(3,nsym_interp))
 !
 ! Parallel setup and distribution
 !---------------------------------
 n_of_elements=Nk
 if (.not.INTERPOLATE_is_serial) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/Nk/))
   n_of_elements=px%n_of_elements(myid+1)
 endif
 VALUES=rZERO
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing("Interpolate@"//trim(INTERP_obj(ID)%what)//" Fourier",n_of_elements)
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
     call make_star(lattice_vectors(:,i_shell),nsym_interp,int_sop,nstar,star_vec)  
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
 YAMBO_FREE(star_vec)
 !
 call timing('INTERPOLATE Fourier',OPR='stop')
 !
end subroutine INTERPOLATION_fourier
