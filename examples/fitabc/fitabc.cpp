#ifdef DEBUG
  #ifndef __SUNPRO_C
    #include <cfenv>
    #include <cstdlib>
  #endif
#endif
#ifdef DEBUG
  #include <chrono>
#endif
#include <admodel.h>
#ifdef USE_ADMB_CONTRIBS
#include <contrib.h>

#endif
  extern "C"  {
    void ad_boundf(int i);
  }
#include <fitabc.htp>

  df1b2_parameters * df1b2_parameters::df1b2_parameters_ptr=0;
  model_parameters * model_parameters::model_parameters_ptr=0;
model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  adstring tmpstring;
  tmpstring=adprogram_name + adstring(".dat");
  if (argc > 1)
  {
    int on=0;
    if ( (on=option_match(argc,argv,"-ind"))>-1)
    {
      if (on>argc-2 || argv[on+1][0] == '-')
      {
        cerr << "Invalid input data command line option"
                " -- ignored" << endl;
      }
      else
      {
        tmpstring = adstring(argv[on+1]);
      }
    }
  }
  global_datafile = new cifstream(tmpstring);
  if (!global_datafile)
  {
    cerr << "Error: Unable to allocate global_datafile in model_data constructor.";
    ad_exit(1);
  }
  if (!(*global_datafile))
  {
    delete global_datafile;
    global_datafile=NULL;
  }
 nsd=5;
  nnodes.allocate("nnodes");
  samplesize.allocate("samplesize");
  pen_spp.allocate("pen_spp");
  pen_nodes.allocate("pen_nodes");
  nspp.allocate("nspp");
  nyrs.allocate("nyrs");
 nobs = nspp*nyrs*nspp;
  data.allocate(1,nobs,1,5,"data");
  obs_tac.allocate(1,nyrs,1,nspp,1,nspp);
  obs_abc.allocate(1,nyrs,1,nspp,1,nspp);
  rescaled_abc.allocate(1,nyrs,1,nspp,1,nspp);
  maxabc.allocate(1,nspp);
  inode.allocate(1,nyrs,1,nspp);
   maxabc.initialize();
  for (int i=1;i<=nobs;i++)
  {    
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
  if (global_datafile)
  {
    delete global_datafile;
    global_datafile = NULL;
  }
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  model_parameters_ptr=this;
  initializationfunction();
  tac_pen.allocate("tac_pen");
  #ifndef NO_AD_INITIALIZE
  tac_pen.initialize();
  #endif
  theta.allocate(0,nnodes,1,nspp,-8,8,1,"theta");
  pred_tac.allocate(1,nyrs,1,nspp,"pred_tac");
  #ifndef NO_AD_INITIALIZE
    pred_tac.initialize();
  #endif
  abc_tac.allocate(1,nyrs,1,nspp,"abc_tac");
  #ifndef NO_AD_INITIALIZE
    abc_tac.initialize();
  #endif
  sdTAC.allocate(1,nsd,1,nspp,"sdTAC");
  like.allocate(1,4,"like");
  #ifndef NO_AD_INITIALIZE
    like.initialize();
  #endif
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  obj_fun.allocate("obj_fun");  /* ADOBJECTIVEFUNCTION */
}
void model_parameters::userfunction(void)
{
  obj_fun =0.0;
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
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
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
}
  long int arrmblsize=0;

int main(int argc,char * argv[])
{
#ifdef DEBUG
  auto start = std::chrono::high_resolution_clock::now();
  #ifndef __SUNPRO_C
std::feclearexcept(FE_ALL_EXCEPT);
  #endif
#endif
  ad_set_new_handler();
  ad_exit=&ad_boundf;
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(800); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
      if (!arrmblsize) arrmblsize=150000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
#ifdef DEBUG
  std::cout << endl << argv[0] << " elapsed time is " << std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - start).count() << " microseconds." << endl;
  #ifndef __SUNPRO_C
bool failedtest = false;
if (std::fetestexcept(FE_DIVBYZERO))
  { cerr << "Error: Detected division by zero." << endl; failedtest = true; }
if (std::fetestexcept(FE_INVALID))
  { cerr << "Error: Detected invalid argument." << endl; failedtest = true; }
if (std::fetestexcept(FE_OVERFLOW))
  { cerr << "Error: Detected overflow." << endl; failedtest = true; }
if (std::fetestexcept(FE_UNDERFLOW))
  { cerr << "Error: Detected underflow." << endl; }
if (failedtest) { std::abort(); } 
  #endif
#endif
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}

void model_parameters::preliminary_calculations(void){
  #if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

  #endif

}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

void df1b2_parameters::user_function(void)
{
  obj_fun =0.0;
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
  df1b2variable xtmp;
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
}

void df1b2_pre_parameters::setup_quadprior_calcs(void) 
{ 
df1b2_gradlist::set_no_derivatives(); 
quadratic_prior::in_qp_calculations=1; 
}  

void df1b2_pre_parameters::begin_df1b2_funnel(void) 
{ 
(*re_objective_function_value::pobjfun)=0; 
other_separable_stuff_begin(); 
f1b2gradlist->reset();  
if (!quadratic_prior::in_qp_calculations) 
{ 
df1b2_gradlist::set_yes_derivatives();  
} 
funnel_init_var::allocate_all();  
}  

void df1b2_pre_parameters::end_df1b2_funnel(void) 
{  
lapprox->do_separable_stuff(); 
other_separable_stuff_end(); 
funnel_init_var::deallocate_all(); 
} 

void model_parameters::begin_df1b2_funnel(void) 
{ 
if (lapprox)  
{  
{  
begin_funnel_stuff();  
}  
}  
}  

void model_parameters::end_df1b2_funnel(void) 
{  
if (lapprox)  
{  
end_df1b2_funnel_stuff();  
}  
} 
void df1b2_parameters::deallocate() 
{
  tac_pen.deallocate();
  theta.deallocate();
  pred_tac.deallocate();
  abc_tac.deallocate();
  sdTAC.deallocate();
  like.deallocate();
  prior_function_value.deallocate();
  likelihood_function_value.deallocate();
  obj_fun.deallocate();
} 
void df1b2_parameters::allocate(void) 
{
  tac_pen.allocate("tac_pen");
  #ifndef NO_AD_INITIALIZE
  tac_pen.initialize();
  #endif
  theta.allocate(0,nnodes,1,nspp,-8,8,1,"theta");
  pred_tac.allocate(1,nyrs,1,nspp,"pred_tac");
  #ifndef NO_AD_INITIALIZE
    pred_tac.initialize();
  #endif
  abc_tac.allocate(1,nyrs,1,nspp,"abc_tac");
  #ifndef NO_AD_INITIALIZE
    abc_tac.initialize();
  #endif
  sdTAC.allocate(1,nsd,1,nspp,"sdTAC");
  like.allocate(1,4,"like");
  #ifndef NO_AD_INITIALIZE
    like.initialize();
  #endif
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  obj_fun.allocate("obj_fun");  /* ADOBJECTIVEFUNCTION */
}
