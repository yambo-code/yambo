!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine ypp_init(instr,lnstr,FINALIZE)
 !
 use pars,           ONLY:HARTREE
 use wave_func,      ONLY:wf_ng 
 use R_lattice,      ONLY:ng_vec
 use it_m,           ONLY:initdefs,initmode,ofiles_append,&
&                         initinfio,infile,infile_dump,initactivate,&
&                         nrnlvls,rnlvls,runlevel_is_on,switch_off_runlevel
 use drivers,        ONLY:infile_editing
 use com,            ONLY:file_exists
 use stderr,         ONLY:string_split
 use YPP
 use electrons,      ONLY:n_spin
 use par_proc_m,     ONLY:master_cpu,pp_redux_wait
 use timing,         ONLY:what_is_running
#if defined _YPP_RAS
 use bulkeps,        only : bshift, bbroad, bfile, btype 
 use convolute,      only : gbroad
#endif
 implicit none
 integer          ::lnstr
 character(lnstr) ::instr
 logical          ::FINALIZE
 !
 ! Work Space
 !
 type(initdefs)   ::defs
 integer          ::i1,i2
 character(schlen)::rstr_piece(50) 
 !
 !
 ! What is running ?
 !
 what_is_running='YPP'
#if defined _ELPH
 what_is_running='YPP_PH'
#elif defined _RAS
 what_is_running='YPP_RAS'
#endif
 !
 if (FINALIZE) then
   call call_init_load('GameOver')
   if (master_cpu) call ofiles_append(defs=defs)
   return
 else
   !
   ! DEFAULTS
   !
   ! EXC WF
   !
   l_wf=.false.
   l_spin=.false.
   l_sort=.false.
   ncell = (/1, 1, 1/) 
   r_hole= (/0.,0.,0./)
   p_dir = '1'
   lambda = 1          
   state_ctl = '1 - 1'
   deg_energy =0.01/HARTREE
   R_normalize="yes"
   p_format   ='g'
   wf_ng      =ng_vec
   !
#if defined _YPP_ELPH
   !
   ! ELPH
   !
   elph_steps=200
   ph_broad=0.01/HARTREE
   elph_gamma_broad=0.0
   elph_Ef=0.
   elph_dbs_path='.'
   ph_freqs_file='none'
   !
#endif
   !
#if defined _YPP_RAS
   !
   ! RAS
   !
   xdata = 'o.eps-rpa_00'
   ydata = 'o.eps-rpa_00'
   bshift  = 0.0_SP       ! bulk shift
   bbroad  = 0.0_SP       ! bulk broadening
   bfile = "bulk.dat"     ! file of bulk
   btype = "3ir"
   gbroad  = 0.0          ! gaussian convolution, FWHM
   sshift = 0.0           ! surface shift
   datatype = 'eps23'
   d_cellin = 1.0
   !
   ! Localization
   !
   normdir = 3
   lowerlim = 0
   upperlim = 1
   ngloc = 0
   ! 
   ! Transition analysis
   ! 
   Ecv_min = 0.0
   Ecv_max = -1.0/HARTREE
   idir = 1
   !
#endif
   !
   ! K analyze
   !
   coo_in="iku"
   coo_out="rlu"
   Kgw_1=0.
   qlong= 0.0_SP
   !
   ! GPL_EXCLUDE_START
   !
   !BZ RIM
   !
   BZ_RIM_path="."
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! Defaults->Defs + Initialization
 !
 call call_init_load('load')
 !
 ! Dump internally the input file
 !
 if (file_exists(trim(infile))) call infile_dump()
 !
 !RunLevels on
 !
 call string_split(instr,rstr_piece)
 do i1=1,50
   if ( trim(rstr_piece(i1))=="jobstr" ) cycle
   if ( i1>1) then
     if (trim(rstr_piece(i1-1))=="jobstr" ) cycle
   endif
   do i2=1,nrnlvls
     if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i1)) ) then
       infile_editing=.true.
       call initactivate(1,trim(rnlvls(i2,1)))
     endif
     if ( trim(rstr_piece(i1)) == 'excp') then
       l_wf    =trim(rstr_piece(i1+1))=='p'
       !
       ! GPL_EXCLUDE_START
       !
       l_spin  =trim(rstr_piece(i1+1))=='sp'
       !
       ! GPL_EXCLUDE_END
       !
       l_sort  =trim(rstr_piece(i1+1))=='s'
       if  (l_wf)   call initactivate(1,'exc_wf')
       !
       ! GPL_EXCLUDE_START
       !
       if  (l_spin) call initactivate(1,'exc_spin')
       !
       ! GPL_EXCLUDE_END
       !
       if  (l_sort) infile_editing=.false.
       !
     endif
   enddo
 enddo
 !
 if (.not.l_sort) call call_init_load('parserload')
 !
 lkanalyze  =runlevel_is_on('kpt')
 l_exc_props=runlevel_is_on('excp')
 l_wf       =runlevel_is_on('exc_wf')
 !
 ! GPL_EXCLUDE_START
 !
 l_spin     =runlevel_is_on('exc_spin')
 lbzrim     =runlevel_is_on('bzrim')
 !
 ! GPL_EXCLUDE_END
 !
 if (lkanalyze) call initactivate(1,"cooIn cooOut GWKpts Qpts KShift1 KShift2 KShift3")
 !
 ! GPL_EXCLUDE_START
 !
 if (lbzrim) call initactivate(1,"BZ_RIM_path BZ_RIM_Nk cooOut")
 !
 ! GPL_EXCLUDE_END
 !
 if (l_exc_props) then
   if (l_wf) call initactivate(1,"States Format Direction FFTGvecs Degen_Step RNorm &
&                                 Cells Hole Dimension")
   if (l_spin) call initactivate(1,"States Degen_Step")
 endif
 !
#if defined _YPP_ELPH
 !
 lelph=runlevel_is_on('elph')
 if (lelph) call initactivate(1,"DBsPATH PHfreqF EEfermi EfGbroad PhBroad PhStps")
 !
#endif

#if defined _YPP_RAS
 !
 lsurf =runlevel_is_on('surf')
 lras  =runlevel_is_on('ras')
 lloc  =runlevel_is_on('loc')
 ltrans=runlevel_is_on('trans')
 if(lsurf) then
   if (lras) call initactivate(1,"XData YData "//&
&       " BulkFile BulkForm BlkShift BlkBroad "//&
&       " DataType SrfShift d_cell GausConv")
   if(lloc) call initactivate(1,"NormDir LowerLim UpperLim NGLoc")
   if(ltrans) call initactivate(1,"EcvMin EcvMax PolDir")
 endif
 !
#endif
 !
 !
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call pp_redux_wait
 endif
 !
 contains
 !
 subroutine call_init_load(mode) 
   character(*)::mode
   !
   if (mode=='load') initmode=0
   if (mode=='todef') initmode=1
   if (mode=='Gclose') initmode=2
   if (mode=='GameOver') initmode=3
   if (mode=='parserload') initmode=4
   call ypp_init_load(defs)
   !
 end subroutine
 !
end subroutine
