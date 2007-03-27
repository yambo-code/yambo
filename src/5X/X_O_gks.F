!
! Copyright (C) 2000-2005 M. Gruening and the SELF team 
!          http://www.fisica.uniroma2.it/~self
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
 subroutine X_O_gks(Xen,Xk,X)
!============================
 use pars,          ONLY:SP,DP
 use timing,        ONLY:live_timing
! use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,&
!&                        myid,master_node,pp_indexes_reset
! use par_indexes_m, ONLY:par_indexes
 use X_m,            ONLY:X_q0grad,X_alloc,X_t
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use DFT,            ONLY:igks
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
!ws
! type(pp_indexes) ::px
 complex(DP) :: zsum(3)
 complex(SP), allocatable :: lda_coeff(:,:,:), X_q0grad_aux(:,:,:,:,:)
 integer :: bn_lda, bn_aux, bn_min, idum(5), ik, iv, ic, im, in, i_spin
!
! call pp_indexes_reset(px)
!
! bn_aux = number of bands of the auxiliary wavefunctions
! bn_lda = number of bands for the lda coefficients
! note the bn_lda < bn_aux (no problem when directly calculated 
! otherwise a check is needed) 
!
 idum=shape(X_q0grad)
 bn_aux = idum(2)
 allocate(X_q0grad_aux(3,bn_aux,bn_aux,Xk%nibz,n_sp_pol))
 X_q0grad_aux = X_q0grad
 call X_alloc('OptGrad')
 call X_alloc('OptGrad',(/3,X%ib(2),Xen%nbm,Xk%nibz/))  
 X_q0grad=(0.,0.)
 call read_ldacoeff()
 bn_min = min(bn_lda,bn_aux)
 do i_spin = 1, n_sp_pol
 !  call par_indexes(px,(/Xk%nibz,Xen%nbm/),(/1,X%ib(1)/))
 !  call pp_redux_wait
 !  call live_timing('X_GKS',px%stps(myid+1))
   do ik = 1, Xk%nibz
     do iv = X%ib(1), Xen%nbm
!       if (.not.px%i2p(ik,iv)) cycle
       do ic=Xen%nbf+1,X%ib(2)
         zsum (:) = (0._DP,0._DP)
         do im = 1, bn_min
           do in = 1, bn_min
             zsum(:) = zsum(:) + conjg(lda_coeff(ik,iv,im))*lda_coeff(ik,ic,in)*&
&                 X_q0grad_aux(:,in,im,ik,i_spin)
           end do
         end do
         X_q0grad(:,ic,iv,ik,i_spin) = zsum(:)
       end do
     end do
  !   call live_timing(steps=1)
   end do
  !call live_timing()
  !call pp_redux_wait(X_q0grad(:,:,:,:,i_spin))
  !call pp_indexes_reset(px)
 end do
 deallocate(X_q0grad_aux,lda_coeff)
!
contains
!
!===========================
  subroutine read_ldacoeff()
!===========================
 use pars, ONLY:SP,lchlen
 use com,  ONLY:inpath,error
 use DFT,  ONLY:igks
!ws
 implicit none
 character*10 :: rd_xctyp,rd_xcapprx
 character(lchlen) :: coeff_file, errmsg
 integer      :: rd_xc, iter, rd_nibz, ib1,ib2, ialloc
 complex(SP), allocatable :: dum(:,:,:)
 logical :: lexist
 integer, parameter       :: un = 56
!read
 coeff_file = 'AUX/db.coeff'
 inquire (file =coeff_file, exist = lexist) 
 if(.not.lexist) then 
   write(errmsg,'(2a)') 'Unable to find file ',trim(coeff_file)
   call error(errmsg)
 endif
 open(un, file = trim(coeff_file),FORM='unformatted',err=998)
 read(un,err=991) !depends on header (if no header comment this line)
 read(un,err=991, end=991) iter, rd_nibz, bn_lda
 read(un,err=992, end=992) rd_xctyp, rd_xcapprx
 allocate(dum(rd_nibz,bn_lda,bn_lda),stat=ialloc)
 if (ialloc.ne.0) THEN
 stop 'deallocation error dum' 
 endif
 read(un,err=993, end=993) (((dum(ik,ib1,ib2),ik=1,rd_nibz),ib1=1,bn_lda),ib2=1,bn_lda)
 close(un)
! consistency checks: introduce ID instead!?
 if (rd_nibz.ne.Xk%nibz)  call error('nibz in db.coeff not consistent with s.db files')
 rd_xc = translate_xc(rd_xctyp, rd_xcapprx)
 if (igks.ne.rd_xc) then
!   write(errmsg,'(aiai)') 'Wrong xc functional in db.coeff: ',rd_xc, ' instead of ', igks
!   call error(errmsg)
 endif
 allocate(lda_coeff(Xk%nibz,X%ib(2),bn_lda),stat=ialloc)
 if (ialloc.ne.0) THEN
 stop 'deallocation error dum'
 endif
 do ik = 1, Xk%nibz
   do ib1 = 1, X%ib(2)
     do ib2 = 1, bn_lda
       lda_coeff(ik,ib1,ib2) = dum(ik,ib1,ib2)
     end do
   end do
 end do 
 deallocate(dum)
 return
991  continue
    close(un)
    call error('Strange problem reading db.coeff (dimensions)')
992  continue
    close(un)
    call error('Strange problem reading db.coeff (xc type)')
993  continue
    close(un)
    call error('Strange problem reading db.coeff (coeff)')
998 continue
    call error('Problem opening db.coeff for reading.')
  end subroutine read_ldacoeff
!
!===========================================
integer function translate_xc(xctyp,xcapprx)
!===========================================
! This function is needed to translate the xc
! from the old self format (maybe transfer it!)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 character*10 xctyp,xcapprx
!ws
! 
  translate_xc = 0
  if (trim(xcapprx)=='gks') translate_xc =translate_xc+ 400
  if (trim(xcapprx)=='') translate_xc =translate_xc+ 300
  select case(trim(xctyp))
  case("exx")
    translate_xc =translate_xc+1
  case("ssx")
    translate_xc =translate_xc+2
  case("sxlda")
    translate_xc =translate_xc+3
  case("cohsex")
    translate_xc =translate_xc+4
  end select
  if (trim(xctyp)=='lda') translate_xc = 8
end function translate_xc
!
end subroutine X_O_gks
