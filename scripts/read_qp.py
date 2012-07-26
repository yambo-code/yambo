#! /usr/bin/python
"""      AUTHOR: Claudio Attaccalite
         CREATED: 16/04/2012
         LAST MODIFIED: 16/06/2012"""

import sys,argparse
import Scientific.IO.NetCDF
from Scientific_netcdf import NetCDFFile

def Open_DB(path,dbname):
    infile = path + '/'+dbname
    db  = NetCDFFile(infile, 'r')
    return db

def read_QP_from_DB(db):
    key="PARS"
    QP_n_states=int(db.variables[key][2])
    
    key_table="QP_table" 
    QP_table=[]

    for i_qp in range(0,QP_n_states):
        ib =int(db.variables[key_table][0,i_qp])
        ibp=int(db.variables[key_table][1,i_qp])
        ik =int(db.variables[key_table][2,i_qp])
        QP_table.append([ib,ibp,ik])
    
    key_qp   ="QP_E_Eo_Z"
    QP_E_Eo_Z=[]

    for i_qp in range(0,QP_n_states):
        E =complex(db.variables[key_qp][0,i_qp,0],db.variables[key_qp][1,i_qp,0])
        Eo=complex(db.variables[key_qp][0,i_qp,1],db.variables[key_qp][1,i_qp,1])
        Z =complex(db.variables[key_qp][0,i_qp,2],db.variables[key_qp][1,i_qp,2])
        QP_E_Eo_Z.append([E.real,Eo.real,Z.real])

    return QP_table, QP_E_Eo_Z

if __name__ == "__main__":
#
# parse command line
#
    parser = argparse.ArgumentParser(prog='read_qp',description='Read QP_table and QP_E_Eo_Z from \
            the ndb.dipoles netcdf file (yambo)',epilog="Copyright Claudio Attaccalite")
    parser.add_argument('-J','--path', help='path for the ndb.QP file',type=str,default="./SAVE")
    args = parser.parse_args()
    #
    HA2EV=27.2113834
    #
    db_qp = Open_DB(args.path,'ndb.QP')
    QP_table, QP_E_Eo_Z = read_QP_from_DB(db_qp)
        
    print "\n\n * * * Quasi-particles found in  %s * * * \n " % db_qp
    for qp_idx,qp in zip(QP_table, QP_E_Eo_Z):
        print "QP (b= %3d, k=%2d ): Eo = %10.6f eV ---> E = %10.6f eV" % (qp_idx[0],qp_idx[2],qp[1]*HA2EV,qp[0]*HA2EV)

