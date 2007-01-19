!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine spp_init(instr,lnstr,FINALIZE)
 !----------------------------------------
 use it_m,           ONLY:rstatus,initdefs,initmode,ofiles_append,&
&                         initinfio,infile,infile_dump,initactivate,&
&                         nrnlvls,rnlvls
 use drivers,        ONLY:infile_editing
 use com,            ONLY:file_exists
 use stderr,         ONLY:string_split
 use SPP
 use par_proc_m,     ONLY:master_node,pp_redux_wait
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
 if (FINALIZE) then
   call call_init_load('GameOver')
   if (master_node) call ofiles_append(defs=defs)
   return
 else
   !
   ! DEFAULTS
   !
   ! EXC WF
   !
   ncell = (/1, 1, 1/) 
   r_hole= (/0.,0.,0./)
   p_dir = 'x'
   lambda = 1          
   window = 1.
   sort_mode = "i"
   action_mode = "s"
   p_format = 'g'
   max_energy = 1.
   p_dim = 3
   !
#if defined SPP_PJ_PH
   !
   ! ELPH
   !
   elph_steps=200
   elph_Emax=1./HARTREE
   elph_broad=0.1/HARTREE
   elph_gamma_broad=0.0
   elph_Ef=0.
   elph_dbs_path='.'
   !
#endif
   !
   ! K analyze
   !
   Kcoo_in="iku"
   Kcoo_out="rlu"
   Kgw_1=0.
   qlong= (/0.,0.,0./)
   !
   !BZ RIM
   !
   RIM_G_shells=1
   BZ_RIM_path="."
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
   enddo
 enddo
 !
 !
 call call_init_load('parserload')
 !
 lkanalyze=rstatus(1)/=0
 lexcwf=rstatus(2)/=0
 lbzrim=rstatus(3)/=0
 !
 if (lkanalyze) call initactivate(1,"KcooIn KcooOut GWKpts KGrdShft")
 if (lbzrim) call initactivate(1,"BZ_RIM_path Srch_G_sh BZ_RIM_Nk KcooOut")
 if (lexcwf) call initactivate(1,"Action Max_Energy Order State Window Cells &
&                              Hole Dimension Direction Format")
#if defined SPP_PJ_PH
 !
 lelph=rstatus(4)/=0
 if (lelph) call initactivate(1,"DBsPATH EEfermi EfGbroad ElPhBroad ElPhStps EliEmax")
 !
 if (lelph) call initactivate(1,"UseDV")
 !
#endif
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
 call spp_init_load(defs)
 !
 end subroutine
 !
 end subroutine
