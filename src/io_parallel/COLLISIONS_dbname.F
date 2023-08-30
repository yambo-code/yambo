!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine COLLISIONS_dbname(coll_kind,db_name)
 !
 use pars,          ONLY:schlen
 use xc_functionals,ONLY:GS_xc_kind
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED,&
&                        H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_HARTREE
 !
 implicit none
 !
 integer,           intent(in)  :: coll_kind
 character(schlen), intent(out) :: db_name
 !
 if(coll_kind==H_SE_COH) then
   db_name = "COLLISIONS_COH"
 elseif(coll_kind==H_SE_HARTREE.or.coll_kind==H_SE_FOCK.or.coll_kind==H_SE_SEX.or.coll_kind==GS_xc_kind.or.&
&       coll_kind>=H_SE_HARTREE) then
   db_name = "COLLISIONS_HXC"
 elseif(coll_kind==QP_SE_GW_NEQ) then
   db_name = "COLLISIONS_GW_NEQ"
 elseif(coll_kind==QP_SE_GW_QED) then
   db_name = "COLLISIONS_P"
 else
   call error("COLLISIONS I/O : Unknown database type ")
 endif
 !
end subroutine COLLISIONS_dbname
