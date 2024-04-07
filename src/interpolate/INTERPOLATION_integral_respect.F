!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!                  
subroutine INTERPOLATION_integral_respect(ID,OUT_k,OUT,NK)
 !
 use pars,         ONLY:SP
 use R_lattice,    ONLY:bz_samp
 use interpolate,  ONLY:INTERP_obj,INTERP_FineGd
 !
 implicit none
 !
 integer      , intent(in)    :: ID,NK
 type(bz_samp), intent(in)    :: OUT_k
 real(SP)     , intent(inout) :: OUT(INTERP_obj(ID)%nb(1):INTERP_obj(ID)%nb(2),NK,INTERP_obj(ID)%D)
 !
 ! Work Space
 !
 integer  :: i1,ik,i3
 real(SP) :: INTERP_int(10),ORI_int(10)
 !
 INTERP_int=0._SP
 ORI_int   =0._SP
 !
 do i1=INTERP_obj(ID)%nb(1),INTERP_obj(ID)%nb(2)
   do ik=1,INTERP_obj(ID)%nk
     do i3=1,INTERP_obj(ID)%D
       ORI_int(i3)=ORI_int(i3)+INTERP_obj(ID)%SEED(i1,ik,i3)*INTERP_obj(ID)%weights(ik)
     enddo
   enddo
 enddo
 !
 do i1=INTERP_obj(ID)%nb(1),INTERP_obj(ID)%nb(2)
   do ik=1,NK
     do i3=1,INTERP_obj(ID)%D
       if (trim(INTERP_FineGd)=="NONE") INTERP_int(i3)=INTERP_int(i3)+OUT(i1,ik,i3)*OUT_k%weights(ik)
       if (trim(INTERP_FineGd)=="IBZ")  INTERP_int(i3)=INTERP_int(i3)+OUT(i1,ik,i3)*OUT_k%FGibz%weights(ik)
       if (trim(INTERP_FineGd)=="BZ")   INTERP_int(i3)=INTERP_int(i3)+OUT(i1,ik,i3)*OUT_k%FGbz%weights(ik)
       if (trim(INTERP_FineGd)=="BARE") INTERP_int(i3)=INTERP_int(i3)+OUT(i1,ik,i3)*OUT_k%FGbare%weights(ik)
     enddo
   enddo
 enddo
 !
 do i1=INTERP_obj(ID)%nb(1),INTERP_obj(ID)%nb(2)
   do ik=1,NK
     do i3=1,INTERP_obj(ID)%D
       if (abs(INTERP_int(i3))>0._SP) OUT(i1,ik,i3)=OUT(i1,ik,i3)*ORI_int(i3)/INTERP_int(i3)
     enddo
   enddo
 enddo
 !
end subroutine INTERPOLATION_integral_respect
