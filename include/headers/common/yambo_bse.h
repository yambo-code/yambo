/*
        Copyright (C) 2000-2018 the YAMBO team
              http://www.yambo-code.org

 Authors (see AUTHORS file for details): DS
 
 This file is distributed under the terms of the GNU 
 General Public License. You can redistribute it and/or 
 modify it under the terms of the GNU General Public 
 License as published by the Free Software Foundation; 
 either version 2, or (at your option) any later version.

 This program is distributed in the hope that it will 
 be useful, but WITHOUT ANY WARRANTY; without even the 
 implied warranty of MERCHANTABILITY or FITNESS FOR A 
 PARTICULAR PURPOSE.  See the GNU General Public License 
 for more details.

 You should have received a copy of the GNU General Public 
 License along with this program; if not, write to the Free 
 Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
 MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
 
*/
#define DEFINE_BSK_COMMON_INDEXES \
integer ::     i_k_s,i_k_s_m1,i_p_s,i_k_bz,i_p_bz,i_k,i_p,i_kp_s,& NEWLINE \
&               i_kmq_s,i_kmq_s_m1,i_pmq_s,i_kmq_bz,i_pmq_bz,i_kmq,i_pmq,i_kp_mq_s,& NEWLINE \
&               i_Tk,i_Tp,i_Tgrp_k,i_Tgrp_p,H_pos(2),& NEWLINE \
&               i_v_k,i_v_p,i_c_k,i_c_p,i_k_sp_pol,i_p_sp_pol,iq_W,iq_W_bz,iq_W_s,ig_W,& NEWLINE \
&               i_kmq_t,i_pmq_t

#define FILL_BSK_COMMON_INDEXES \
     NEWLINE \
     H_pos(1) = sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk NEWLINE \
     H_pos(2) = sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp NEWLINE \
     NEWLINE \
     if (H_pos(1)>H_pos(2)) cycle NEWLINE \
     NEWLINE \
     i_k_bz  = BS_T_grp(i_Tgrp_k)%table(i_Tk,1) NEWLINE \
     i_p_bz  = BS_T_grp(i_Tgrp_p)%table(i_Tp,1) NEWLINE \
     NEWLINE \
     i_k_s =Xk%sstar(i_k_bz,2) NEWLINE \
     i_p_s =Xk%sstar(i_p_bz,2) NEWLINE \
     NEWLINE \
     i_k_s_m1= sop_inv(i_k_s) NEWLINE \
     i_kp_s  = sop_tab(i_k_s_m1,i_p_s) NEWLINE \
     NEWLINE \
     i_kmq_bz=qindx_X(iq,i_k_bz,1) NEWLINE \
     i_pmq_bz=qindx_X(iq,i_p_bz,1) NEWLINE \
     NEWLINE \
     i_kmq   =Xk%sstar(i_kmq_bz,1) NEWLINE \
     i_pmq   =Xk%sstar(i_pmq_bz,1) NEWLINE \
     NEWLINE \
     i_kmq_s =Xk%sstar(i_kmq_bz,2) NEWLINE \
     i_pmq_s =Xk%sstar(i_pmq_bz,2) NEWLINE \
     NEWLINE \
     if (BS_res_K_corr) then NEWLINE \
       i_kmq_t=BS_blk(i_block)%kp_table(1,i_kmq) NEWLINE \
       i_pmq_t=BS_blk(i_block)%kp_table(2,i_pmq) NEWLINE \
     endif NEWLINE \
     NEWLINE \
     i_kmq_s_m1 = sop_inv(i_kmq_s) NEWLINE \
     i_kp_mq_s  = sop_tab(i_kmq_s_m1,i_pmq_s) NEWLINE \
     NEWLINE \
     if (BS_res_K_corr.or.BS_cpl_K_corr) then NEWLINE \
       iq_W_bz=qindx_B(i_k_bz,i_p_bz,1) NEWLINE \
       ig_W   =qindx_B(i_k_bz,i_p_bz,2) NEWLINE \
       iq_W   =q%sstar( iq_W_bz ,1) NEWLINE \
       iq_W_s =q%sstar( iq_W_bz ,2) NEWLINE \
     endif NEWLINE \
     NEWLINE \
     i_v_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,2) NEWLINE \
     i_c_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,3) NEWLINE \
     NEWLINE \
     i_v_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,2) NEWLINE \
     i_c_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,3) NEWLINE \
     NEWLINE \
     i_k_sp_pol=BS_T_grp(i_Tgrp_k)%table(i_Tk,4) NEWLINE \
     i_p_sp_pol=BS_T_grp(i_Tgrp_p)%table(i_Tp,4) NEWLINE \

#define FILL_BSK_KERNEL_INDEXES \
     NEWLINE \
     ig_kmq    = qindx_X(iq,i_k_bz,2) NEWLINE \
     ig_pmq    = qindx_X(iq,i_p_bz,2) NEWLINE \
     NEWLINE \
     if (BS_res_K_corr.or.BS_cpl_K_corr) then NEWLINE \
       iq_W_bz_mq=qindx_B(i_kmq_bz,i_pmq_bz,1) NEWLINE \
       ig_W_mq   =qindx_B(i_kmq_bz,i_pmq_bz,2) NEWLINE \
       iq_W_mq   =q%sstar( iq_W_bz_mq,1) NEWLINE \
       iq_W_s_mq =q%sstar( iq_W_bz_mq,2) NEWLINE \
     endif NEWLINE \
     NEWLINE \
     if (BS_K_is_ALDA) then NEWLINE \
       is_k = (/i_c_k,i_k,i_k_s,i_k_sp_pol/) NEWLINE \
       os_k = (/i_v_k,i_kmq,i_kmq_s,i_k_sp_pol/) NEWLINE \
       is_p = (/i_c_p,i_p,i_p_s,i_p_sp_pol/) NEWLINE \
       os_p = (/i_v_p,i_pmq,i_pmq_s,i_p_sp_pol/) NEWLINE \
     endif NEWLINE \

#define FILL_BSK_CORR_INDEXES \
     NEWLINE \
     i_s_star=Xk%s_table(i_p,i_kp_s) NEWLINE \
     i_s_mq_star=Xk%s_table(i_pmq,i_kp_mq_s) NEWLINE \

