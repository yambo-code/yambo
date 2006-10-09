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
 subroutine spp_init_load(defs)
!==============================
 use it_m,          ONLY:it,initdefs,E_unit
 use SPP
 implicit none
 type(initdefs)::defs
!
!RunLevels
 call it('r',defs,'kpt',  '[R] K-grid analysis')  ! [1]
 call it('r',defs,'excwf','[R] Excitonic Wavefunction plot')  ! [2]
 call it('r',defs,'elph','[R] Electron-Phonon DBs preprocessing')  ! [3]
 call it('r',defs,'bzrim','[R] BZ energy RIM analyzer')  ! [4]
!
!Kpt analysis
 call it(defs,'KcooIn','K-pts coordinates (in) cc/rlu/iku',Kcoo_in)
 call it(defs,'KcooOut','K-pts coordinates (out) cc/rlu/iku',Kcoo_out)
 call it(defs,'GWKpts','GW K-point grid',Kgw_1)
!
!EXC WF
 call it(defs,'Order','Energies order `mode` [(e)nergy/(i)ntensity]', sort_mode)
 call it(defs,'State','Ordered Index of to BS state',lambda)
 call it(defs,'Window','[o/o] BS states window',window)
 call it(defs,'Cells', 'Number of BS WF cell repetitions',ncell)
 call it(defs,'Hole', '[cc] Hole position',r_hole)
!
!EL-PH
 call it(defs,'DBsPATH', 'Path to the PW el-ph databases',elph_dbs_path)
 call it(defs,'EfGbroad', 'Fermi surface broadening (Gamma)',elph_gamma_broad,E_unit)
 call it(defs,'EfEbroad', 'Fermi surface broadening (Eliashberg)',elph_eli_broad,E_unit)
 call it(defs,'EEfermi', 'External Fermi Level (eliashberg routine)',elph_Ef,E_unit)
 call it(defs,'EliEmax', 'Max energy in Eliashberg function',elph_Emax,E_unit)
 call it(defs,'EliStps', 'Energy steps in Eliashberg function',elph_steps)
!
!BZ E RIM
 call it(defs,'BZ_RIM_Nk','BZ Random Integration points',BZ_RIM_nkpts)
 call it(defs,'Srch_G_sh','Number of RL shells used in the search',RIM_G_shells)
 call it(defs,'BZ_RIM_path','Path to the SAVE folder with the RIM DB1',BZ_RIM_path)

 end subroutine
