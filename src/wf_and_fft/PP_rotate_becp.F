!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
subroutine PP_rotate_becp(isc,ibec,obec)
 !
 ! isc(1), ibnd
 ! isc(2), ikpt
 ! isc(3), isymm
 ! isc(4), ispin
 !
 ! ibec    becp corresponding to (ikpt, ispin)
 ! obec    becp corresponding to (isym*ikpt, ispin)
 !
 use pars,          ONLY:SP,pi,cONE,cZERO
 use com,           ONLY:error
 use vec_operate,   ONLY:c2a
 use wrapper,       ONLY:M_by_M
 use stderr,        ONLY:intc
 use pseudo,        ONLY:pp_is_uspp,qe_atoms_map
 use electrons,     ONLY:l_spin_orbit,n_spinor
 use D_lattice,     ONLY:nsym,spin_sop,dl_sop,i_time_rev,idt_index,i_space_inv,inv_index,&
&                        irt,icell,atom_pos,n_atoms_species,n_atomic_species
 use R_lattice,     ONLY:rl_sop,kibz_vec,b
 use qe_pseudo_m,   ONLY:bec_type,allocate_bec_type,deallocate_bec_type,&
&                        beccopy,d1,d2,d3,nh,upf,indv_ijkb0
 !
#include<memory.h>
 !
 ! vars
 !
 integer :: isc(4)
 type(bec_type), intent(in)    :: ibec
 type(bec_type), intent(inout) :: obec

 !
 ! Workspace
 !
 integer     :: ikibz,isym,isym_,ia,is
 integer     :: nbnd,nkb
 real(SP)    :: xk(3),arg
 complex(SP) :: c1(3,3),c2(5,5),c3(7,7),phase
 logical     :: use_trev
 integer     :: iaeq,ll,jj,ibs,ibseq,ibsn,iben,n
 integer,     allocatable :: check(:)


 if (.not.pp_is_uspp) return
 !
 ikibz=isc(2)
 isym=isc(3)
 nbnd=ibec%nbnd
 nkb=ibec%nkb
 !
 if (isym==idt_index) then
   call beccopy(ibec, obec)
   return
 endif
 
 !
 ! checks
 !
 if (l_spin_orbit) call error("[PPs] SO not implementd in PP_rotate_becp")
 if (n_spinor>1)   call error("[PPs] NC not implementd in PP_rotate_becp")

 !
 ! whether to use time-reversal
 !
 isym_ = isym
 use_trev = .FALSE.
 !
 if ( isym_ > nsym/(1+i_time_rev) ) then
   isym_=isym_-nsym/(1+i_time_rev)
   use_trev = .TRUE.
 endif
 !
 if ( isym_ <= 0 .or. isym_ > nsym/(1+i_time_rev) ) &
&  call error("[PPs] invalid isym index in becp rotate")

 !
 ! kpt
 ! get xk in crystal units (rlu) 
 call c2a(b,kibz_vec(ikibz,:),xk,mode="ki2a")

 !
 ! local workspace
 !
 YAMBO_ALLOC( check,(nkb))

 !
 ! main loop
 !
 ! This is valid only in the ordinary case,
 ! NC and SO need to be implemented here
 !
 c1    = d1(:,:,isym_) 
 c2    = d2(:,:,isym_) 
 c3    = d3(:,:,isym_) 
 !
 check(:) = 0
 !
 do is=1,n_atomic_species
 do ia=1,n_atoms_species(is)
    !
    iaeq = irt(isym_,ia,is)
    !
    ibs   = indv_ijkb0( qe_atoms_map(ia,is) )
    ibseq = indv_ijkb0( qe_atoms_map(iaeq,is) )
    !
    ibsn  = 0
    iben  = 0
    !
    do n = 1, upf(is)%nbeta
      !
      ibsn = iben+1 
      ll   = upf(is)%lll(n)
      jj   = upf(is)%jjj(n)
      iben = ibsn+(2*ll+1) -1
      !
      select case (ll)
      case ( 0 )    
         !
         obec%k(ibs+ibsn-1,:) = ibec%k(ibseq+ibsn-1,:)
         check(ibs+ibsn-1)  = check(ibs+ibsn-1)+1
         !
      case ( 1 )
         !
         CALL M_by_M( "N","N", 3, nbnd, 3,  cONE, c1, 3, &
&                      ibec%k(ibseq+ibsn-1:ibseq+ibsn+1,:), 3, cZERO, &
&                      obec%k(ibs+ibsn-1:ibs+ibsn+1,:), 3)
         !CALL mat_mul( obec%k(ibs+ibsn-1:ibs+ibsn+1,:), c1, "N", &
         !              ibec%k(ibseq+ibsn-1:ibseq+ibsn+1,:), "N", 3, nbnd, 3)
         check(ibs+ibsn-1:ibs+ibsn+1) = check(ibs+ibsn-1:ibs+ibsn+1)+1
         !
      case ( 2 )
         !
         CALL M_by_M( "N","N", 5, nbnd, 5,  cONE, c2, 5, &
&                      ibec%k(ibseq+ibsn-1:ibseq+ibsn+3,:), 5, cZERO, &
&                      obec%k(ibs+ibsn-1:ibs+ibsn+3,:), 5)
         !CALL mat_mul( obec%k(ibs+ibsn-1:ibs+ibsn+3,:), c2, "N", &
         !              ibec%k(ibseq+ibsn-1:ibseq+ibsn+3,:), "N", 5, nbnd, 5)
         check(ibs+ibsn-1:ibs+ibsn+3)  = check(ibs+ibsn-1:ibs+ibsn+3)+1
         !
      case ( 3 )
         !
         CALL M_by_M( "N","N", 7, nbnd, 7,  cONE, c3, 7, &
&                      ibec%k(ibseq+ibsn-1:ibseq+ibsn+5,:), 7, cZERO, &
&                      obec%k(ibs+ibsn-1:ibs+ibsn+5,:), 7)
         !CALL mat_mul( obec%k(ibs+ibsn-1:ibs+ibsn+5,:), c3, "N", &
         !              ibec%k(ibseq+ibsn-1:ibseq+ibsn+5,:), "N", 7, nbnd, 7)
         check(ibs+ibsn-1:ibs+ibsn+5)  = check(ibs+ibsn-1:ibs+ibsn+5)+1
         !
      case default
          call error("invalid ll = "//trim(intc(ll)))
      end select
      !
    enddo
    !
    ! add a phase in case atom ia is shifted into a different cell
    !
    arg = 2.0_SP*pi*dot_product(xk,real(icell(:,isym_,ia,is),SP))
    !
    if (.not.use_trev) then
      phase = cmplx(cos(arg), sin(arg), SP)
    else
      phase = cmplx(cos(arg),-sin(arg), SP)
    endif
    !
    obec%k(ibs:ibs+nh(is)-1,:) = obec%k(ibs:ibs+nh(is)-1,:) * phase
    !
 enddo
 enddo
 !
 if(any(check(:)== 0)) call error("[PPs] unexpected error in PP beta mapping")

 !
 ! cleanup
 !
 YAMBO_FREE(check)
 !
 return
 !
end subroutine PP_rotate_becp

