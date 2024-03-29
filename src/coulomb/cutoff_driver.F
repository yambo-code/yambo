!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV
!
subroutine cutoff_driver(q)
 !     
 ! In this subroutine I calculate the FT of a cutoffed coulomb
 ! interaction, zeroed outside a given cylinder/box/sphere geometry.
 !
 ! The cutoffed potential in G-space is given by
 !
 ! CYLINDER:
 ! ---------
 ! 
 ! Vc(q)=4pi*Int[0,Rc]Int[0,zc]drdz cos(q_z*z)*J0(q_r*r)/sqrt(r**2+z**2)
 ! q_r=sqrt(qx**2+qy**2)
 !
 ! BOX:
 ! ----
 !
 ! Vc(q,G)=1/(Vdl Nq)\sum_{G'} V(q'+G') F(q'+G',q+G)
 !
 ! Note that q\in BZ and
 !
 ! F(v,w)=8 \prod_i sin[(v_i-w_i)L_i/2]/(v_i-w_i)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use vec_operate,   ONLY:v_is_zero
 use com,           ONLY:msg
 use R_lattice,     ONLY:cyl_ph_radius,box_length,cyl_length,cut_geometry,bz_samp,cut_description,ws_cutoff,&
                         cut_is_slab
 use stderr,        ONLY:STRING_split,STRING_pack
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,OP_WR_CL,REP,VERIFY,NONE
 use parser_m,      ONLY:parser
 use drivers,       ONLY:l_col_cut
 use timing_m,      ONLY:timing
 implicit none
 type(bz_samp) :: q
 !
 ! Work Space
 !
 logical ::cut_is_sphere,cut_is_box,cut_is_cyl,cut_is_ws,is_cut(3)
 integer ::i1,i2,i3,i4
 real(SP)::length(3)
 logical ::l_col_test
 character(schlen)::str_piece(5)
 character(schlen)::dir
 character(lchlen)::ch1,ch2
 !
 ! Potential symmetry
 !
 real(SP)::sym_points(8,3)
 !
 ! I/O
 !
 integer ::ID,io_err
 integer, external :: io_COL_CUT
 !
 ! First check if the cutoff written on disk is consistent
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=io_COL_CUT(ID)
 !
 ! Parsing the cut_geometry string
 !
 call STRING_split(cut_geometry,str_piece)
 !
 cut_is_sphere=trim(str_piece(1))=='sphere'
 cut_is_box=trim(str_piece(1))   =='box'
 cut_is_cyl=trim(str_piece(1))   =='cylinder'
 cut_is_ws=trim(str_piece(1))    =='ws'
 cut_is_slab=trim(str_piece(1))  =='slab'
 !
 ! Cutoffed directions
 !
 is_cut(1)=index(str_piece(2),'x')/=0.or.index(str_piece(2),'X')/=0
 is_cut(2)=index(str_piece(2),'y')/=0.or.index(str_piece(2),'Y')/=0
 is_cut(3)=index(str_piece(2),'z')/=0.or.index(str_piece(2),'Z')/=0
 !
 if (cut_is_sphere) is_cut=.true.
 !
 if(any(is_cut)) call check_periodic_directions(is_cut)
 !
 if (io_err==0) then
   call section('*','Coloumb potential CutOff '//trim(cut_geometry))
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
   io_err=io_COL_CUT(ID)
   return
 endif
 !
 if (.not.cut_is_box)                        box_length=0.
 if (.not.cut_is_sphere.and..not.cut_is_cyl) cyl_ph_radius=0.
 if (.not.cut_is_cyl)                        cyl_length=0.
 if (.not.is_cut(1))                         box_length(1)=0.
 if (.not.is_cut(2))                         box_length(2)=0.
 if (.not.is_cut(3))                         box_length(3)=0.
 if (.not.cut_is_ws)                         ws_cutoff=0.
 !
 ! Return conditions
 !
 if (.not.any((/cut_is_sphere,cut_is_box,cut_is_cyl,cut_is_ws,cut_is_slab/))) then
   l_col_cut=.false.
   return
 endif
 if (.not.any(is_cut)) then
   l_col_cut=.false.
   return
 endif
 if (cut_is_sphere.and.cyl_ph_radius==0.) then
   l_col_cut=.false.
   return
 endif
 if (cut_is_box.and.all(box_length==0.)) then
   l_col_cut=.false.
   return
 endif
 if (cut_is_cyl.and.cyl_ph_radius==0.) then
   l_col_cut=.false.
   return
 endif
 if (cut_is_ws.and.ws_cutoff==0.) then
   l_col_cut=.false.
   return
 endif
 !
 ch1=STRING_pack('Coloumb potential CutOff ',str_piece(1))
 call section('*',trim(ch1))
 !
 ch1=           'Cut directions       '
 ch2=' '
 if (is_cut(1)) ch1=STRING_pack(ch1,'X')
 if (is_cut(1)) ch2=STRING_pack(ch2,'x')
 if (is_cut(2)) ch1=STRING_pack(ch1,'Y')
 if (is_cut(2)) ch2=STRING_pack(ch2,'y')
 if (is_cut(3)) ch1=STRING_pack(ch1,'Z')
 if (is_cut(3)) ch2=STRING_pack(ch2,'z')
 call msg('r',trim(ch1))
 write (cut_geometry,'(3a)') trim(str_piece(1)),' ',trim(ch2)
 !
 !lengths
 !
 if (cut_is_sphere) then 
   call msg('r','Sphere radius       ',cyl_ph_radius,"[a.u.]")
   write (cut_description,'(a,f6.3)') trim(cut_geometry)//' ', cyl_ph_radius
 endif
 if (cut_is_ws) then
   call msg('r','WS Cutoff   [units to be defined]',ws_cutoff)
   write (cut_description,'(a,f6.3)') trim(cut_geometry)//' ', ws_cutoff
 endif
 if (cut_is_slab) then
   dir=""
   if (is_cut(1)) dir="X"
   if (is_cut(2)) dir="Y"
   if (is_cut(3)) dir="Z"
   call msg('r','Slab Cutoff:',dir)
   write (cut_description,'(a)') trim(cut_geometry)
 endif
 if (cut_is_box) then
   i1=0
   if (is_cut(1)) i1=i1+1
   if (is_cut(1)) length(i1)=box_length(1)
   if (is_cut(2)) i1=i1+1
   if (is_cut(2)) length(i1)=box_length(2)
   if (is_cut(3)) i1=i1+1
   if (is_cut(3)) length(i1)=box_length(3)
   call msg('r','Box sides            ',length(:i1),"[a.u.]")
   write (cut_description,'(a,3f6.3)') trim(cut_geometry)//' ', length(:i1)
 else if (cut_is_cyl) then
   if (cyl_length > 0.) then
     call msg('r','Cyl. length          ',cyl_length,"[a.u.]")
   else
     call msg('r','Cyl. length          : infinite')
   endif
   call msg('r',  'Cyl. radius          ',cyl_ph_radius,"[a.u.]")
   if (cyl_length > 0.) then
     write (cut_description,'(a,f6.3,a,f6.3)') trim(cut_geometry)//' ',cyl_length,' ',cyl_ph_radius 
   else
     write (cut_description,'(a,a,a,f6.3)') trim(cut_geometry)//' ','infinite',' ',cyl_ph_radius
   endif
 endif
 !
 ! Checking geometry symmetry
 !
 ! We generate max 8 points corresponding to
 ! the corners of the given geometry.
 ! These points must be left unchanged by the
 ! symmetry operations.
 !
 sym_points=0.
 if (cut_is_cyl) then
   do i1=1,3
     if (is_cut(i1)) then
       sym_points(1,i1)=-cyl_length/2.
       sym_points(2,i1)= cyl_length/2.
     endif
   enddo
 else if (cut_is_box) then
   i4=0
   do i1=-1,1,2
     do i2=-1,1,2
       do i3=-1,1,2
         i4=i4+1
         sym_points(i4,:)=(/i1*box_length(1),i2*box_length(2),i3*box_length(3)/)/2.
       enddo
     enddo
   enddo
   if (.not.is_cut(1)) sym_points(:,1)=0.
   if (.not.is_cut(2)) sym_points(:,2)=0.
   if (.not.is_cut(3)) sym_points(:,3)=0.
 endif
 !
 call msg('rn','Symmetry test passed ',geometry_sym_is_ok())
 if (.not.geometry_sym_is_ok()) return
 !
 ! timing is started here after all the return conditions
 ! since it should be both started and stopped in order not
 ! to break the clock
 !
 call timing('Coulomb Cutoff',OPR='start')
 !
 ! I/O + Specific geometry calls
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_COL_CUT(ID)
 !
 if (io_err/=0) then
   !
   if (cut_is_sphere) call cutoff_sphere()
   if (cut_is_box)    call cutoff_box(q,is_cut)
   if (cut_is_cyl)    call cutoff_cylinder(q,is_cut)
   if (cut_is_ws)     call cutoff_ws(q)
   if (cut_is_slab)   call cutoff_slab(q,is_cut)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_COL_CUT(ID)
   !
   ! Real-Space Test
   !
   call parser('CUTCol_test',l_col_test)
   if (l_col_test) call cutoff_test(cut_is_sphere,cut_is_box,cut_is_cyl,cut_is_ws,cut_is_slab,is_cut,q)
   !
 endif
 call timing('Coulomb Cutoff',OPR='stop')
 !
 contains
   !
   logical function geometry_sym_is_ok()
     !
     use D_lattice,     ONLY:dl_sop,nsym
     integer    :: is,syms_ok
     real(SP)   :: v_rot(3)
     !
     geometry_sym_is_ok=.true.
     do i1=1,8
       syms_ok=0
       do is=1,nsym
         v_rot=matmul(dl_sop(:,:,is),sym_points(i1,:))
         do i2=1,8
           if (v_is_zero(v_rot-sym_points(i2,:))) then
             syms_ok=syms_ok+1
             exit
           endif
         enddo
       enddo
       if (syms_ok/=nsym) geometry_sym_is_ok=.false.
     enddo
     !
   end function
   !
end subroutine
