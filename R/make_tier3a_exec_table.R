#' Takes spm results and produces the values for the Tier3a executive summary table
#'
#' @param run_dir the directory where the spm output is located
#' @param endyr the current year
#' @param the_scalar the scalar used in spm.dat for outputting results
#'
#' @returns Values for Tier 3a executive summary table
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @examples
#' make_tier3a_exec_table(run_dir = "C:/GitProjects/goa_deepwater/2025/harvest_projections",endyr = 2025,the_scalar = 1000)
make_tier3a_exec_table<-function(run_dir,endyr,the_scalar) {

  bdf <- spmR::runSPM(run_dir,run=FALSE)
  df <- readr::read_csv(file.path(run_dir,"spm_summary.csv"))
  inp <- spmR::dat2list(file.path(run_dir,"spm.dat"))
  fc <- data.frame(Year=inp$fixed_catch[,1], mean=inp$fixed_catch[,2], type='C')# |>
  names(fc) <- c("Year", "mean", "type")
  fc <- fc |> expand_grid(Alt=c(1,3,5,7)) |> mutate(Alt=as.factor(Alt), ub=mean,lb=mean)
  proj_plot<-plotSPM(df) + geom_point(data=fc,aes(x=Year,y=mean)) + theme_classic()
  ggsave(proj_plot,file = file.path(run_dir,"spm_plot.png"),device = "png",height= 6, width = 8)


  #Build executive summary table:
  stuff<-bdf %>% select(c(Year,Alt,Sim,SSB,F,Tot_biom,OFL,ABC)) %>% filter(Year == endyr+1 | Year==endyr+2,Alt==2) %>%
    group_by(Year,Alt) %>%
    summarize(meanTot_biom = mean(Tot_biom),meanSSB = mean(SSB),meanABC = mean(ABC),meanOFL=mean(OFL))

  stuff2<-as_tibble(cbind(variable = names(stuff),round(t(stuff)*the_scalar))) %>% filter(variable!="Year",variable!="Alt")
  names(stuff2)<-c("variable",endyr+1,endyr+2)


  ssb_ref_pts<-df %>% filter(variable=="SSB_100" | variable=="SSB_40" | variable == "SSB_ofl") %>%
    select(-c(spp_file,Alt,Year)) %>%
    mutate(value = round(value*the_scalar,0)) %>%
    mutate(value2 = value)

  F_ref_pts<-df %>% filter(variable == "F_abc" | variable == "F_ofl") %>%
    mutate(value = round(value,2)) %>%
    select(-c(spp_file,Alt,Year)) %>%
    mutate(value2 = value)

  names(ssb_ref_pts)<-c("variable",endyr+1,endyr+2)
  names(F_ref_pts)<-c("variable",endyr+1,endyr+2)

  exec_table<-rbind(stuff2[1:2,],
                    ssb_ref_pts[1:3,],
                    F_ref_pts[2,],
                    F_ref_pts[1,],
                    F_ref_pts[1,],
                    stuff2[4,],
                    stuff2[3,],
                    stuff2[3,])

  write.csv(exec_table,file.path(run_dir,"exec_table.csv"))
  return(exec_table)
}
