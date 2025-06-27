!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine DIPOLE_rotate(ic,iv,ik_bz,i_sp_pol,what,kpt,dipole)
 !
 use pars,           ONLY:cZERO,SP
 use DIPOLES,        ONLY:DIP_P,DIP_v,DIP_iR,DIP_spin,DIP_orbital
 use D_lattice,      ONLY:i_time_rev,dl_sop,nsym
 use electrons,      ONLY:n_spinor
 use parallel_m,     ONLY:PAR_K_scheme
 use R_lattice,      ONLY:bz_samp
 use matrix_operate, ONLY:m3det
 !
 implicit none
 !
 complex(SP),   intent(out):: dipole(3)
 integer,       intent(in) :: ic,iv,ik_bz,i_sp_pol
 character(*),  intent(in) :: what
 type(bz_samp), intent(in) :: kpt
 !
 ! Work space
 !
 complex(SP) :: DIP_in(3)
 real(SP)    :: tmp_sop(3,3)
 integer :: ik,is,ik_mem
 logical :: t_rev
 !
 ik=kpt%sstar(ik_bz,1)
 is=kpt%sstar(ik_bz,2)
 !
 ik_mem    = PAR_K_scheme%ibz_index(ik)
 !
 if ( is<= nsym/(i_time_rev+1) ) t_rev=.false.
 if ( is > nsym/(i_time_rev+1) ) t_rev=.true.
 !
 tmp_sop=dl_sop(:,:,is)
 !
 if(trim(what)=="DIP_S" ) then
   if (n_spinor==2) then
     ! pseudo vectors are invariant under inversion but reverse under t_rev
     tmp_sop=tmp_sop*m3det(dl_sop(:,:,is))
     if( t_rev ) tmp_sop=-tmp_sop
   else
     ! in case n_sp_pol==2 the spin is assumed to be independent on symmetries
     tmp_sop=dl_sop(:,:,1)
     t_rev=.false.
   endif
 endif
 !
 if(trim(what)=="DIP_L" ) then
   ! pseudo vectors are invariant under inversion but reverse under t_rev
   tmp_sop=tmp_sop*m3det(dl_sop(:,:,is))
   if( t_rev ) tmp_sop=-tmp_sop
 endif
 !
 DIP_in=cZERO
 if(trim(what)=="DIP_P" ) DIP_in=DIP_P(:,ic,iv,ik_mem,i_sp_pol)
 if(trim(what)=="DIP_v" ) DIP_in=DIP_v(:,ic,iv,ik_mem,i_sp_pol)
 if(trim(what)=="DIP_iR") DIP_in=DIP_iR(:,ic,iv,ik_mem,i_sp_pol)
 if(trim(what)=="DIP_S" ) DIP_in=DIP_spin(:,ic,iv,ik_mem,i_sp_pol,1)
 if(trim(what)=="DIP_L" ) DIP_in=DIP_orbital(:,ic,iv,ik_mem,i_sp_pol,1)  
 !
 dipole = matmul( tmp_sop , DIP_in  )
 !
 if (t_rev) dipole = conjg( dipole )
 !
end subroutine DIPOLE_rotate
