!
! Copyright (C) 2012 WanT Group
! Adapted for p2y by DS
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************************
SUBROUTINE  atmproj_rotate_proj( natomwfc, nbnd, isym, vkpt_c, proj)
   !************************************************************
   !
   USE symmetry_module, ONLY : d1, d2, d3, srot, strasl, nsym, &
                               irt, icell, symm_alloc => alloc
   USE ions_module,     ONLY : nat, ityp, nsp, ions_alloc => alloc
   !
   IMPLICIT NONE
   !
   INTEGER,      INTENT(IN)    :: natomwfc, nbnd, isym
   REAL(dbl),    INTENT(IN)    :: vkpt_c(3)     ! crystal units are expected
   COMPLEX(dbl), INTENT(INOUT) :: proj(natomwfc,nbnd)
   !
   CHARACTER(19) :: subname="atmproj_rotate_proj"
   INTEGER       :: ia, iaeq, nt, ierr
   INTEGER       :: isym_, il, is, iseq, isn, ien, n
   LOGICAL       :: use_trev
   REAL(dbl)     :: rvkpt_c(3), arg
   COMPLEX(dbl)  :: c1(3,3), c2(5,5), c3(7,7), phase
   !
   INTEGER,      ALLOCATABLE :: iatom_map(:), itypl(:,:)
   INTEGER,      ALLOCATABLE :: natomwfc_sp(:)
   INTEGER,      ALLOCATABLE :: check(:)
   COMPLEX(dbl), ALLOCATABLE :: proj0(:,:)


   !
   ! if we deal with the identity, nothing to do
   IF ( isym == 1 ) RETURN
   !CALL log_push(subname)
   !
   IF ( .NOT. symm_alloc ) CALL errore(subname,"symmetry module not alloc",10)
   IF ( .NOT. ions_alloc ) CALL errore(subname,"ions module not alloc",10)

   !
   ! whether to use time-reversal
   !
   isym_ = isym
   use_trev = .FALSE.
   !
   IF ( isym_ > nsym ) THEN
       isym_=isym_-nsym
       use_trev = .TRUE.
   ENDIF
   !
   IF ( isym_ <= 0 .OR. isym_ > nsym ) CALL errore(subname,"invalid isym index",10)
   !
   ! local workspace
   !
   ALLOCATE( iatom_map(nat), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating iatom_map",10)
   ALLOCATE( proj0(natomwfc,nbnd), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating proj0",10)
   ALLOCATE( natomwfc_sp(nsp), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating natomwfc_sp",10)
   ALLOCATE( itypl(nwfcx,nsp), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating itypl",10)
   ALLOCATE(check(natomwfc), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,"allocating check",10)

   !
   ! build iatom_map
   !
   CALL atmproj_get_natomwfc( nsp, upf(1:nsp), natomwfc_sp, itypl )
   !
   iatom_map(1)=1
   !
   DO ia = 2, nat
       !
       nt=ityp(ia-1)
       iatom_map(ia) = iatom_map(ia-1) + natomwfc_sp(nt) 
       !
   ENDDO
   !
   !CALL mat_mul( rvkpt_c, REAL(srot(:,:,isym_),dbl), "T", vkpt_c, 3, 3)
   rvkpt_c = vkpt_c

   !
   ! main loop
   !
   proj0 = proj
   c1    = d1(:,:,isym_) 
   c2    = d2(:,:,isym_) 
   c3    = d3(:,:,isym_) 
   !
   check(:) = 0
   !
   DO ia = 1, nat
       !
       iaeq = irt(isym_,ia)
       nt   = ityp(ia)
       !
       is   = iatom_map(ia)
       iseq = iatom_map(iaeq)
       !
       isn  = 0
       ien  = 0
       !
       DO n = 1, upf(nt)%nwfc
           !
           isn = ien+1 
           il  = itypl(isn,nt)
           ien = isn+(2*il+1) -1
           !
           SELECT CASE ( il)
           CASE ( 0 )    
              !
              proj(is+isn-1,:) = proj0(iseq+isn-1,:)
              check(is+isn-1)  = check(is+isn-1)+1
              !
           CASE ( 1 )
              !
              CALL mat_mul( proj(is+isn-1:is+isn+1,:), c1, "N", &
                            proj0(iseq+isn-1:iseq+isn+1,:), "N", 3, nbnd, 3)
              check(is+isn-1:is+isn+1) = check(is+isn-1:is+isn+1)+1
              !
           CASE ( 2 )
              !
              CALL mat_mul( proj(is+isn-1:is+isn+3,:), c2, "N", &
                            proj0(iseq+isn-1:iseq+isn+3,:), "N", 5, nbnd, 5)
              check(is+isn-1:is+isn+3)  = check(is+isn-1:is+isn+3)+1
              !
           CASE ( 3 )
              !
              CALL mat_mul( proj(is+isn-1:is+isn+5,:), c3, "N", &
                            proj0(iseq+isn-1:iseq+isn+5,:), "N", 7, nbnd, 7)
              check(is+isn-1:is+isn+5)  = check(is+isn-1:is+isn+5)+1
              !
           CASE DEFAULT
               CALL errore(subname,"invalid il",ABS(il))
           END SELECT
           !
       ENDDO
       !
       ! add a phase in case atom ia is shifted into a different cell
       !
       arg   = TPI * DOT_PRODUCT( rvkpt_c, REAL(icell(:,isym_,ia)) )
       !
       IF ( .NOT. use_trev ) THEN
           phase = CMPLX( COS(arg),  SIN(arg), dbl )
       ELSE
           phase = CMPLX( COS(arg), -SIN(arg), dbl )
       ENDIF
       !
       proj(is:is+natomwfc_sp(nt)-1,:) = proj(is:is+natomwfc_sp(nt)-1,:) * phase
       !
   ENDDO
   !
   IF ( ANY(check(:)== 0 ) ) CALL errore(subname,"unexpected error in atmwfc mapping",10)
   
   !
   ! cleanup
   !
   DEALLOCATE( iatom_map, proj0, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating iatom_map, proj0",ABS(ierr))
   DEALLOCATE( natomwfc_sp, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating natomwfc_sp",ABS(ierr))
   DEALLOCATE( itypl, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating itypl",ABS(ierr))
   DEALLOCATE( check, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating check",ABS(ierr))
   !
   !CALL log_pop(subname)
   RETURN
   !
END SUBROUTINE atmproj_rotate_proj


!************************************************************
SUBROUTINE  atmproj_rotate_ovp( natomwfc, isym, vkpt_c, ovp)
   !************************************************************
   !
   USE symmetry_module, ONLY : d1, d2, d3, srot, strasl, nsym, &
                               irt, icell, symm_alloc => alloc
   USE ions_module,     ONLY : nat, ityp, nsp, ions_alloc => alloc
   !
   IMPLICIT NONE
   !
   INTEGER,      INTENT(IN)    :: natomwfc, isym
   REAL(dbl),    INTENT(IN)    :: vkpt_c(3)     ! crystal units are expected
   COMPLEX(dbl), INTENT(INOUT) :: ovp(natomwfc,natomwfc)
   !
   CHARACTER(18) :: subname="atmproj_rotate_ovp"
   INTEGER       :: ia, iaeq, nt_a
   INTEGER       :: il_a, is_a, iseq_a, isn_a, ien_a, na, nd_a
   INTEGER       :: ib, ibeq, nt_b
   INTEGER       :: il_b, is_b, iseq_b, isn_b, ien_b, nb, nd_b
   INTEGER       :: ierr, isym_
   LOGICAL       :: use_trev
   REAL(dbl)     :: rvkpt_c(3), arg
   COMPLEX(dbl)  :: phase
   COMPLEX(dbl), TARGET   :: c0(1,1), c1(3,3), c2(5,5), c3(7,7)
   COMPLEX(dbl), POINTER  :: c_a(:,:), c_b(:,:)
   !
   INTEGER,      ALLOCATABLE :: iatom_map(:), itypl(:,:)
   INTEGER,      ALLOCATABLE :: natomwfc_sp(:)
   INTEGER,      ALLOCATABLE :: check(:,:)
   COMPLEX(dbl), ALLOCATABLE :: ovp0(:,:)


   !
   ! if we deal with the identity, nothing to do
   IF ( isym == 1 ) RETURN
   !CALL log_push(subname)
   !
   IF ( .NOT. symm_alloc ) CALL errore(subname,"symmetry module not alloc",10)
   IF ( .NOT. ions_alloc ) CALL errore(subname,"ions module not alloc",10)

   !
   ! whether to use time-reversal
   !
   isym_ = isym
   use_trev = .FALSE.
   !
   IF ( isym_ > nsym ) THEN
       isym_=isym_-nsym
       use_trev = .TRUE.
   ENDIF
   !
   IF ( isym_ <= 0 .OR. isym_ > nsym ) CALL errore(subname,"invalid isym index",10)
   !
   ! local workspace
   !
   ALLOCATE( iatom_map(nat), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating iatom_map",10)
   ALLOCATE( ovp0(natomwfc,natomwfc), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating proj0",10)
   ALLOCATE( natomwfc_sp(nsp), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating natomwfc_sp",10)
   ALLOCATE( itypl(nwfcx,nsp), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating itypl",10)
   ALLOCATE(check(natomwfc,natomwfc), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,"allocating check",10)

   !
   ! build iatom_map
   !
   CALL atmproj_get_natomwfc( nsp, upf(1:nsp), natomwfc_sp, itypl )
   !
   iatom_map(1)=1
   !
   DO ia = 2, nat
       !
       nt_a=ityp(ia-1)
       iatom_map(ia) = iatom_map(ia-1) + natomwfc_sp(nt_a) 
       !
   ENDDO
   !
   !CALL mat_mul( rvkpt_c, REAL(srot(:,:,isym_),dbl), "T", vkpt_c, 3, 3)
   rvkpt_c = vkpt_c

   !
   ! main loop
   !
   ovp0 = ovp
   c0    = 1.0d0
   c1    = d1(:,:,isym_) 
   c2    = d2(:,:,isym_) 
   c3    = d3(:,:,isym_) 
   !
   check(:,:) = 0
   !
   DO ib = 1, nat
       ! 
       ibeq   = irt(isym_,ib)
       nt_b   = ityp(ib)
       !
       is_b   = iatom_map(ib)
       iseq_b = iatom_map(ibeq)
       !
       isn_b  = 0
       ien_b  = 0
       !
       DO nb = 1, upf(nt_b)%nwfc
           !
           isn_b = ien_b+1 
           il_b  = itypl(isn_b,nt_b)
           nd_b  = (2*il_b+1)
           ien_b = isn_b+(2*il_b+1) -1
           !
           CALL associate_pointer(c_b,il_b)
           !
           !
           DO ia = 1, nat
               !
               iaeq   = irt(isym_,ia)
               nt_a   = ityp(ia)
               !
               is_a   = iatom_map(ia)
               iseq_a = iatom_map(iaeq)
               !
               isn_a  = 0
               ien_a  = 0
               !
               DO na = 1, upf(nt_a)%nwfc
                   !
                   isn_a = ien_a+1 
                   il_a  = itypl(isn_a,nt_a)
                   nd_a  = (2*il_a+1)
                   ien_a = isn_a+(2*il_a+1) -1

                   CALL associate_pointer(c_a,il_a)

                   !
                   ! sanity check
                   !
                   check(is_a+isn_a-1:is_a+isn_a-1+nd_a,is_b+isn_b-1:is_b+isn_b-1+nd_b) = &
                        check(is_a+isn_a-1:is_a+isn_a-1+nd_a,is_b+isn_b-1:is_b+isn_b-1+nd_b) +1

                   !
                   ! actual rotation
                   !
                   CALL mat_mul( ovp(is_a+isn_a-1:is_a+isn_a-1+nd_a, is_b+isn_b-1:is_b+isn_b-1+nd_b), c_a, "N", &
                                 ovp0(iseq_a+isn_a-1:iseq_a+isn_a-1+nd_a, iseq_b+isn_b-1:iseq_b+isn_b-1+nd_b), "N", &
                                 nd_a, nd_b, nd_a)
                   CALL mat_mul( ovp(is_a+isn_a-1:is_a+isn_a-1+nd_a, is_b+isn_b-1:is_b+isn_b-1+nd_b), & 
                                 ovp(is_a+isn_a-1:is_a+isn_a-1+nd_a, is_b+isn_b-1:is_b+isn_b-1+nd_b), "N", &
                                 c_b, "N", nd_a, nd_b, nd_b )
                   !
               ENDDO
           ENDDO
           ! 
       ENDDO
   ENDDO

   !
   ! add a phase in case atom ia is shifted into a different cell
   !
   DO ib = 1, nat
       !
       nt_b   = ityp(ib)
       is_b   = iatom_map(ib)
       !
       DO ia = 1, nat
           !
           nt_a   = ityp(ia)
           is_a   = iatom_map(ia)
           !
           !
           phase = 1.0d0
           !
           arg   = TPI * DOT_PRODUCT( rvkpt_c, REAL(icell(:,isym_,ia)) )
           phase = phase * CMPLX( COS(arg),  SIN(arg), dbl )
           !
           arg   = TPI * DOT_PRODUCT( rvkpt_c, REAL(icell(:,isym_,ib)) )
           phase = phase * CMPLX( COS(arg), -SIN(arg), dbl )
           !
           ! AF: do we really need this cmplx conjg here ?
           IF ( use_trev ) phase = CONJG( phase )
           !
           !
           ovp(is_a:is_a+natomwfc_sp(nt_a)-1,is_b:is_b+natomwfc_sp(nt_b)-1) = &
               ovp(is_a:is_a+natomwfc_sp(nt_a)-1,is_b:is_b+natomwfc_sp(nt_b)-1) * phase
           !
       ENDDO
       !
   ENDDO
   !
   IF ( ANY(check(:,:)== 0 ) ) CALL errore(subname,"unexpected error in atmwfc mapping",10)
   
   !
   ! cleanup
   !
   DEALLOCATE( iatom_map, ovp0, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating iatom_map, ovp0",ABS(ierr))
   DEALLOCATE( natomwfc_sp, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating natomwfc_sp",ABS(ierr))
   DEALLOCATE( itypl, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating itypl",ABS(ierr))
   DEALLOCATE( check, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating check",ABS(ierr))
   !
   !CALL log_pop(subname)
   !
   RETURN

CONTAINS

   SUBROUTINE associate_pointer(cp,il)
      IMPLICIT NONE
      COMPLEX(dbl), POINTER :: cp(:,:)
      INTEGER               :: il

      SELECT CASE (il)
      CASE (0)
        cp => c0
      CASE (1)
        cp => c1
      CASE (2)
        cp => c2
      CASE (3)
        cp => c3
      CASE DEFAULT
         CALL errore(subname,"invalid il",ABS(il)+1)
      END SELECT
   END SUBROUTINE associate_pointer
   !
END SUBROUTINE atmproj_rotate_ovp


