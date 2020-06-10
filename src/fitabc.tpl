DATA_SECTION

 int nsd 
 !! nsd=5;
 init_int nnodes
 init_number samplesize
 init_number pen_spp
 init_number pen_nodes
 init_int nspp
 init_int nyrs
 int nobs
 !! nobs = nspp*nyrs*nspp;
 init_matrix data(1,nobs,1,5)
 3darray obs_tac(1,nyrs,1,nspp,1,nspp)
 3darray obs_abc(1,nyrs,1,nspp,1,nspp)
 3darray rescaled_abc(1,nyrs,1,nspp,1,nspp)
 vector maxabc(1,nspp)
 number offset;
 imatrix inode(1,nyrs,1,nspp)
 // matrix obs_tac(1,nyrs,1,nspp)
 // matrix obs_abc(1,nyrs,1,nspp)
 LOCAL_CALCS
   maxabc.initialize();
  for (int i=1;i<=nobs;i++)
  {    
 // TAC_Spp ABC_Spp Yr  ABC TAC
    obs_abc(data(i,3),data(i,1),data(i,2)) = data(i,4);
    obs_tac(data(i,3),data(i,2),data(i,1)) = data(i,5);
    if (obs_abc(data(i,3),data(i,1),data(i,2)) >  maxabc(data(i,2)))
      maxabc(data(i,2)) = obs_abc(data(i,3),data(i,1),data(i,2)) ;
  }
  int ijunk;
  offset = 0.;
  for (int i=1;i<=nyrs;i++) 
  {
    for (int j=1;j<=nspp;j++) 
    {
      for (int k=1;k<=nspp;k++) 
        rescaled_abc(i,j,k) = obs_abc(i,j,k)/maxabc(k);

      for (int k=1;k<=nspp;k++) 
      {
        ijunk = int(rescaled_abc(i,j,k)*nnodes) ;
        inode(i,k) = ijunk;
      }
      obs_tac(i,j) /= sum(obs_tac(i,j));
    }
    offset -= samplesize * obs_tac(i,1) * log(obs_tac(i,1));
  }
   // cout <<obs_tac<<endl;cout<<endl<<obs_abc<<endl<<endl<<"inode"<<endl<<inode<<endl;exit(1);
 END_CALCS

PARAMETER_SECTION
  number tac_pen;
  init_bounded_matrix theta(0,nnodes,1,nspp,-8,8,1)
  matrix pred_tac(1,nyrs,1,nspp)
  matrix abc_tac(1,nyrs,1,nspp)
  sdreport_matrix sdTAC(1,nsd,1,nspp)
  vector like(1,4);
  objective_function_value obj_fun   

PROCEDURE_SECTION
  like.initialize();
  pred_tac.initialize();
  tac_pen=0.0;
  for (int i=1;i<=nyrs;i++) 
  {
    for (int j=1;j<=nspp;j++) 
    {
      // pred_tac(i)  += elem_prod(obs_abc(i,j),mfexp(theta(i,j))) ;
      pred_tac(i,j)  += rescaled_abc(i,1,j) * mfexp(theta(inode(i,j),j)) ;
    }
    pred_tac(i) /= sum(pred_tac(i));
  }

  int ijunk;
  for (int i=1;i<=nsd;i++)
  {
    for (int j=1;j<=nspp;j++) 
    {
      ijunk = int( double(i)/nsd * nnodes) ;
      sdTAC(i,j)  = double(i)/double(nsd) * mfexp( theta(ijunk,j) ) ;
    }
  }

  // Fit the TAC portions
  for (int i=1;i<=nyrs;i++) 
    like(1) -= samplesize * obs_tac(i,1) * log(pred_tac(i)) ;
  like(1) -= offset;

  // Penalize differences in theta over species...
  for (int i=0;i<=nnodes;i++) 
    like(2) += pen_spp * norm2(first_difference(first_difference(theta(i))));

  // Penalize differences in theta over nodes...
  if (nnodes >1)
    for (int j=1;j<=nspp;j++) 
      like(3) += pen_nodes*norm2(first_difference(first_difference(trans(theta)(j))));

  // Penalize TAC less than ABC's...
  dvariable xtmp;
  for (int i=1;i<=nyrs;i++) 
  {
    for (int j=1;j<=nspp;j++) 
    {
      abc_tac(i,j) = obs_abc(i,1,j)-(2.0*pred_tac(i,j));
      xtmp = posfun( (abc_tac(i,j) ), 0.2 , tac_pen );
      // cout<<(1.-abc_tac(i,j))<<" "<< tac_pen<<endl;
    }
  }
  like(4) = 20.*tac_pen;
  obj_fun = sum(like);

REPORT_SECTION
  cout <<endl<< "Report section, phase: "<<current_phase()<<endl;
  cout <<"===========================================  "<<endl;

  report<<"Obs_TAC " <<endl;
    for (int i=1;i<=nyrs;i++) 
      report<<2.*obs_tac(i,1)<< " Pred: "<<i<<" "<<2.*pred_tac(i)<<endl;
  report<<"Pred_TAC " <<endl;
    for (int i=1;i<=nyrs;i++) 
      report<<2.*pred_tac(i)<<endl;
  report<<"Obs_ABC " <<endl;
    for (int i=1;i<=nyrs;i++) 
      report<<rescaled_abc(i,1)<<endl;
  report<<"Obs_ABC/Pred_TAC " <<endl;
    for (int i=1;i<=nyrs;i++) 
      report<<abc_tac(i)<<endl;
  report<<"#_Nodes,_samsize,_penaltyspp,_pennode,_Likelihoods"<<endl;
  report<<nnodes<<" "<<samplesize<<" "<<pen_spp<<" "<<pen_nodes<<" "<<like<<endl;
  report<<"TAC_by_nodes"<<endl;
    for (int i=1;i<=nsd;i++) 
      report<<sdTAC(i)<<endl;
  ofstream ofs("tacpar.dat");
  ofs << nspp  <<endl;
  ofs << nnodes <<endl;
  ofs << maxabc <<endl;
  ofs << theta  <<endl;
  ofs.close();
  // report<<pred_tac<<endl;

  // report<<pred_tac<<endl;

TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(800); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);

