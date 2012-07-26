#! /usr/bin/python
"""      AUTHOR: Myrta Gruening
         CREATED: 17/11/2011
         LAST MODIFIED: 17/11/2011"""
import sys,argparse
import Scientific.IO.NetCDF
from Scientific_netcdf import NetCDFFile
#
XYZ = {'x':1,'y':2,'z':3}
def Open_DBdipoles(path):
    infile = path + '/ndb.dipoles'
    db  = NetCDFFile(infile, 'r')
    return db
def Read_dipole_from_DB(db,nmk):
    kpt = '00'+str(nmk[2])
    if (nmk[2] < 10):
        kpt = '0' + kpt
    key = 'DIP_iR_k_'+ kpt + '_xyz' + '_000'+str(XYZ[nmk[3]]) + '_spin_0001' #for the moment only without spin
    dip_R = db.variables[key][0,nmk[0]-1,nmk[1]-1]
    dip_I = db.variables[key][1,nmk[0]-1,nmk[1]-1]
    return complex(dip_R,dip_I)
#
#
#
if __name__ == "__main__":
#
# parse command line
#
    parser = argparse.ArgumentParser(prog='read_dipoles',description='Read one specific element of the ndb.dipoles netcdf file (yambo)',epilog="Copyright Myrta Gruning 2011")
    parser.add_argument('B1', help='first band',type=int)
    parser.add_argument('B2', help='second band',type=int)
    parser.add_argument('K', help='k point',type=int)
    parser.add_argument('xyz', help='direction',type=str,choices=["x","y","z"])
    parser.add_argument('-J','--path', help='path for the ndb.dipoles file',type=str,default="./SAVE")
    args = parser.parse_args()
    #
    #
    #
    transition = [args.B1,args.B2,args.K,args.xyz]
    db_dip = Open_DBdipoles(args.path)
    Dipole = Read_dipole_from_DB(db_dip,transition)
    print "Dipole element <n{0:}|{3:}|n{1:}> at k{2:}: {4:}, module: {5:}".format(args.B1,args.B2,args.K,args.xyz,Dipole,abs(Dipole))
