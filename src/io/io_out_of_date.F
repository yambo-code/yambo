!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function io_out_of_date(ID)
 !
 use IO_m,          ONLY:IO_OUTDATED_DB,io_file
 use IO_int,        ONLY:ver_is_gt_or_eq
 !
 implicit none
 integer               :: ID
 !
 ! Work Space
 !
 integer :: REF_rev,REF_ver(3)
 !
 io_out_of_date=IO_OUTDATED_DB
 !
 ! Global revision check 
 !=======================
 !
 ! 4.2.0: global change of IO bulk due to the introduction of the COMPLEX interface.
 !        affects only some ndb files
 !
 if (.not.ver_is_gt_or_eq(ID,(/4,2,0/))) then
   if (index(io_file(ID),'Vnl')==0.and.&
&      index(io_file(ID),'Double_Grid')==0.and.&
&      index(io_file(ID),'RIM_E_and_k')==0.and.&
&      index(io_file(ID),'E_RIM')==0.and.&
&      index(io_file(ID),'elph_')==0.and.&
&      index(io_file(ID),'kindx')==0.and.&
&      index(io_file(ID),'RIM')==0.and.&
&      index(io_file(ID),'Overlaps')==0.and.&
&      index(io_file(ID),'gops')==0.and.&
&      index(io_file(ID),'db1')==0.and.&
&      index(io_file(ID),'kb_pp')==0.and.&
&      index(io_file(ID),'wf')==0.and.&
&      index(io_file(ID),'QP')==0.and.&
&      index(io_file(ID),'E_SOC_map')==0&
&     ) return
 endif
 !
 ! 3.0.15: mostly based on io_header changes
 !
 if (.not.ver_is_gt_or_eq(ID,(/3,0,15/))) return
 !
 ! DB specific
 !=============
 !
 REF_rev=17876   ! X descriptors changed
 if (index(io_file(ID),'em1')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 !
 REF_rev=16788   ! tables are now defined as integers
 if (index(io_file(ID),'QINDX')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 !
 REF_rev=16664   ! tables are defined as character
 REF_rev=17876   ! X descriptors changed
 if (index(io_file(ID),'COLLISIONS')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 !
 REF_rev=5380
 if (index(io_file(ID),'RT_carriers')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 if (index(io_file(ID),'RT_OBSERVERVABLES')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 if (index(io_file(ID),'RT_REF')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 if (index(io_file(ID),'RT_G')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 !
 REF_ver=(/3,2,1/)
 if (index(io_file(ID),'elph_gkkp')/=0.and..not.ver_is_gt_or_eq(ID,REF_ver)) return
 if (index(io_file(ID),'elph_gFsq')/=0.and..not.ver_is_gt_or_eq(ID,REF_ver)) return
 !
 REF_ver=(/3,0,15/)
 if (index(io_file(ID),'BS_diago_Q')/=0.and..not.ver_is_gt_or_eq(ID,REF_ver)) return
 !
 ! No error => Clean return
 !==========================
 !
 io_out_of_date=0
 !
end function
