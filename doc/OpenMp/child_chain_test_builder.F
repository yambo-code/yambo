
 n_chains=4
 father_chain=3
 child_chain=2
 N=2**n_chains
 n_f=2**father_chain
 n_c=2**(father_chain-child_chain)
 
 write (*,*) 
 write (*,*) 'N ',N
 write (*,*) 'N_father ',n_f
 write (*,*) 'N_child ',n_c
 write (*,*) 

 do i=0,N-1
   write (*,*) i,mod(i,n_f),(i/n_f)*n_c,(i/n_f)*n_c+mod(i,n_f/n_c)
 enddo

 end program
