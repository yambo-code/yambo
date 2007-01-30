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
 !
 call it('r',defs,'kpt',  '[R] K-grid analysis')  ! [1]
 call it('r',defs,'excwf','[R] Excitonic Wavefunction plot')  ! [2]
 call it('r',defs,'bzrim','[R] BZ energy RIM analyzer')  ! [3]
 call it('r',defs,'elph','[R] Electron-Phonon DBs preprocessing')  ! [4]
 !
 !Kpt analysis
 !
 call it(defs,'KcooIn','K-pts coordinates (in) cc/rlu/iku',Kcoo_in)
 call it(defs,'KcooOut','K-pts coordinates (out) cc/rlu/iku',Kcoo_out)
 call it(defs,'GWKpts','GW K-point grid',Kgw_1)
 call it(defs,'KGrdShft','K-grid shifts',qlong)
 !
 !EXC WF
 !
 call it(defs,'Action','Sort energies or plot WF [(s)ort/(p)lot]', action_mode)
 call it(defs,'Max_Energy','Max value energy to be shown', max_energy,E_unit)
 call it(defs,'Order','Energies order `mode` [(e)nergy/(i)ntensity]', sort_mode)
 call it(defs,'State','Ordered Index of the BS state',lambda)
 call it(defs,'Window','[o/o] BS states window (not implemented)',window)
 call it(defs,'Cells', 'Number of cell repetitions (even)',ncell)
 call it(defs,'Hole', '[cc] Hole position in unit cell',r_hole)
 !
 ! p_dim:    puo' assumere i valori 1/2/3  controlla se dare in output la
 ! funzione eccitonica in 3 dimensioni, oppure in 2 dimensioni (integrata
 ! nella direzione perpendicolare al piano, oppure 1 dimenzione (integrata
 ! nel piano perpendicolare alla retta.
 ! 
 ! p_dir:   puo' assumere i valori x/y/z/  /xy/xz/yz   controlla la
 ! direzione per il plot 1d oppure il piano per la 2d
 ! 
 ! p_format: controlla il formato dell'ouptut (g)nuplot/(x)crysden,
 ! il plot 1d viene comunque fatto per gnuplot e il 3d per xcrysden
 !
 call it(defs,'Dimension', 'Dimension of the plot [3, integrated /1/2]',p_dim)
 call it(defs,'Format', 'Format of the output[(g)nuplot/(x)crysden]',p_format)
 call it(defs,'Direction', '[x/y/z] for 1d or [xy/xz/yz] for 2d',p_dir)
 !
#if defined SPP_PJ_PH
 !
 !EL-PH
 !
 call it(defs,'DBsPATH',  'Path to the PW el-ph databases',elph_dbs_path)
 call it(defs,'EfGbroad', 'Fermi surface broadening (Gamma)',elph_gamma_broad,E_unit)
 call it(defs,'ElPhBroad','Broadening (Eliashberg)',elph_broad,E_unit)
 call it(defs,'EEfermi',  'External Fermi Level (Eliashberg)',elph_Ef,E_unit)
 call it(defs,'ElPhStps', 'Energy steps',elph_steps)
 !
#endif
 !
 !BZ E RIM
 !
 call it(defs,'BZ_RIM_Nk','BZ Random Integration points',BZ_RIM_nkpts)
 call it(defs,'Srch_G_sh','Number of RL shells used in the search',RIM_G_shells)
 call it(defs,'BZ_RIM_path','Path to the SAVE folder with the RIM DB1',BZ_RIM_path)

 end subroutine
