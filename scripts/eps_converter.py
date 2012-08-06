#! /usr/bin/python
"""      AUTHOR: Claudio Attaccalite
         CREATED: 27/07/2012
         LAST MODIFIED: 27/07/2012 """

""" 
    This program takes as input a dielectric constant and produces: 
    1) conductivity
    2) refractive index and extinction coefficient
    3) reflectivity and transmissivity

    reference:
    ELECTRODYNAMICS OF SOLIDS
    OPTICAL PROPERTIES OF ELECTRONS IN MATTER
    Martin_Dressel and George Gruner

"""

import argparse                                                           
import string
import os
import re
import math
from sys import exit

def Read_Epsilon(path,filename):
    infile = os.path.join(path,filename)
    try:
        f=open(infile,'r')
    except:
        print " Error opening: %s " % infile
        exit(0)

    lines=f.readlines()
    
    pattern1=r'\s*ETStpsXd\s*=\s*(\d*)'
    pattern2=r'\s*BEnSteps\s*=\s*(\d*)'
    pattern=pattern1 + '|' + pattern2
    match = re.search(pattern, string.join(lines), re.MULTILINE)

    if match: 
        if match.group(1): nsteps=int(match.group(1))
        else: nsteps=int(match.group(2))
        print " Number of points: %d " % nsteps
    else:
        print " Wrong dielectric constant file! "
        exit(0)

    comments =[]
    energy   =[]
    eps_im   =[]
    eps_re   =[]
    eps0_im  =[]
    eps0_re  =[]

    for line in lines:
        if re.search(r'\s*#', line):
            comments.append(line)
            continue
        parts=re.split(r'\s+',line.strip())
        #
        # To be changed in case of exponential notation
        #
        energy.append(float(parts[0]))
        eps_im.append(float(parts[1]))
        eps_re.append(float(parts[2]))
        eps0_im.append(float(parts[3]))
        eps0_re.append(float(parts[4]))

    return comments,energy,eps_im,eps_re,eps0_im,eps0_re

def eval_conductivity(energy, eps_im, eps_re, eps0_im, eps0_re):
    #
    # Re(Sigma) = \omega * Im(epsilon) / ( 4 pi)
    # Im(Simga) = ( 1 - Re(epsilon)) * \omega / (4 *pi ) 
    #
    sigma_im  =[]
    sigma_re  =[]
    sigma0_im =[]
    sigma0_re =[]
    #
    for en,e_im,e_re,e0_im,e0_re in zip(energy, eps_im, eps_re, eps0_im, eps0_re):
        sigma_im.append((1.0 - e_re)*en/(4.0 * math.pi))
        sigma0_im.append((1.0 - e0_re)*en/(4.0 * math.pi))

        sigma_re.append(en * e_im/ (4.0 * math.pi))
        sigma0_re.append(en * e0_im/ (4.0 * math.pi))

    fname="conductivity"
    headers=['E/ev[1]','cond /Im[2]','cond /Re[3]','cond0/Im[4]','cond0/Re[5]']
    return fname,headers,sigma_im,sigma_re,sigma0_im,sigma0_re

def eval_refra_and_extin(energy, eps_im,eps_re,eps0_im,eps0_re):
    #
    # n = refractive index
    # k = extinction coefficient
    # 
    # n = ( \mu_1/2 ( e_im^2 + e_re^2) ^(1/2) + e_re * \mu_1 /2 )^(1/2)
    # k = ( \mu_1/2 ( e_im^2 + e_re^2) ^(1/2) - e_re * \mu_1 /2 )^(1/2)
    #
    #  We consider \mu_1 = 1. this approximation can fail for magnetic materials!
    #
    n =[]
    k =[]
    n0 =[]
    k0 =[]
    #
    for en,e_im,e_re,e0_im,e0_re in zip(energy, eps_im, eps_re, eps0_im, eps0_re):
        n.append( (1./2.*(e_im**2.  + e_re**2. )**(1./2.) + e_re *  1./2. )**(1./2.))
        k.append( (1./2.*(e_im**2.  + e_re**2. )**(1./2.) - e_re *  1./2. )**(1./2.))
        n0.append((1./2.*(e0_im**2. + e0_re**2.)**(1./2.) + e0_re * 1./2. )**(1./2.))
        k0.append((1./2.*(e0_im**2. + e0_re**2.)**(1./2.) - e0_re * 1./2. )**(1./2.))

    fname="refraction_and_extinction"
    headers=['E/ev[1]','n','k','n0','k0']
    return fname,headers,n,k,n0,k0

def eval_trasmissivity(energy, eps_im,eps_re,eps0_im,eps0_re):
    #
    # Reflectivity and trasmissivity for normal incidence 
    #
    # R = Reflecivity
    # T = Trasmissivity
    # 
    # R = ((1 - n)^2 + k^2)/((1 + n)^2 +k^2)
    # T = 1 - R 
    #
    R =[]
    T =[]
    R0 =[]
    T0 =[]
    #
    fname,headers,n_l,k_l,n0_l,k0_l=eval_refra_and_extin(energy, eps_im,eps_re,eps0_im,eps0_re)
    #
    for en,n,k,n0,k0 in zip(energy, n_l, k_l, n0_l, k0_l):
        refle =((1 - n )**2. + k**2. )/((1. + n)**2.  + k**2.)
        refle0=((1 - n0)**2. + k0**2.)/((1. + n0)**2. +k0**2.)
        R.append(refle)
        T.append(1. - refle)
        R0.append(refle0)
        T0.append(1. - refle0)

    fname="reflecivity_and_trasmissivity"
    headers=['E/ev[1]','R','T','R0','T0']
    return fname,headers,R,T,R0,T0
#
#
#
if __name__ == "__main__":
#
# parse command line
#
    parser = argparse.ArgumentParser(prog='eps_coonverter',description='Read and convert dielectric constant (yambo)',epilog="Copyright Claudio Attaccalite 2012",formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('-J','--path', help='path for the o.eps_... file',type=str,default="./",dest="path")
    parser.add_argument('-F', help='dielectric constant file name',type=str,default=None,dest="filename")
    
    options={'c': eval_conductivity,
             'r': eval_refra_and_extin,
             't': eval_trasmissivity}

    parser.add_argument('-o', choices=options.keys(),help="output: \n (c)onductivity \n (r)efractive index and extinction coefficient \n (t)ransmissivity and reflectivity",type=str,default=None,dest="output")
    args = parser.parse_args()

    if(args.output==None or args.filename==None): 
        parser.print_help()
        exit(0)
    else:
        print '\n\n Read and convert dielectric constant (yambo) \n'

    comments,energy,eps_im,eps_re,eps0_im,eps0_re=Read_Epsilon(args.path,args.filename)
    
    function = options.get(args.output)
    
    fname,headers,out_im,out_re,out0_im,out0_re=function(energy, eps_im,eps_re,eps0_im,eps0_re)

    print " output : %s \n" % ("o."+fname)

    outfile=open("o."+fname,'w')
    outfile.write("#\n#\n# "+fname+" from "+args.path+"/"+args.filename+"#\n#\n#\n")

    outfile.write("# %s   %s    %s    %s    %s  \n" % (headers[0],headers[1],headers[2],headers[3],headers[4]))

    for en,im,re,im0,re0 in zip(energy, out_im, out_re, out0_im, out0_re):  
        outfile.write(" %f  %f  %f  %f  %f \n" % (en,im,re,im0,re0))

    for comment in comments:
        outfile.write(comment)

    outfile.close()
