#' Plot Results from SPM Analysis from a Tibble
#'
#' This function creates plots from the results of a Stock Production Model (SPM) analysis contained in a tibble.
#' It generates three separate plots for different metrics and combines them using the `patchwork` package.
#'
#' @param df A tibble containing the results from `spm_detail.csv`.
#' @param thisyr The current year for the projection model.
#' @param alt  The alternative to be plotted.
#' @param mytitle  Title for the plot
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth
#' @importFrom dplyr filter
#' @importFrom patchwork
#' @return A combined plot of the results from the SPM analysis.
#' @export
plotSPM <- function(df, alt=2,thisyr=2022,mytitle=NULL) {
  dfs <- df %>% filter(Sim<=30, Alternative==alt) %>% select(Alternative,Yr,Catch,SSB,Sim)
  # create a tibble from df that has the quantiles over Sim
  pf <-  df |> select(Sim,Alternative,Yr,Catch,SSB,ABC,OFL) |>
                     pivot_longer(cols =  4:7,names_to = "variable", values_to = "value") |>
      group_by(Yr,Alternative,variable) |> summarise(median=median(value),mean=mean(value),lb=quantile(value,.1),ub=quantile(value,.9))

  names(pf) <- c("Yr","Alt","variable","median","mean","lb","ub")

  #p1 <-
  head(pf)
  head(pf)
  Cofl <- as.numeric(pf |> ungroup() |> filter(Alt==alt,variable=="OFL") |> summarise(max(mean)))
  Cabc <- as.numeric(pf |> ungroup() |> filter(Alt==alt,variable=="ABC") |> summarise(max(mean)))
  p1 <- pf %>% filter(Alt==alt,variable=="Catch") |>
    ggplot(aes(x=Yr,y=mean),width=1.2) + geom_ribbon(aes(ymax=ub,ymin=lb),fill="goldenrod",alpha=.5) +
    ggthemes::theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") +
    coord_cartesian(ylim=c(0,NA)) + ylab("Tier 3 ABC") + geom_point() +
    geom_line(data=dfs,aes(x=Yr,y=Catch,col=as.factor(Sim)))+
    geom_hline(yintercept=Cabc) +
    geom_hline(yintercept=Cofl, linetype="dashed") +
    guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE)

  p2 <- pf %>% filter(Alt==alt,variable=="SSB") |>
    ggplot(aes(x=Yr,y=mean),width=1.2) + geom_ribbon(aes(ymax=ub,ymin=lb),fill="goldenrod",alpha=.5) +
    ggthemes::theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") +
    coord_cartesian(ylim=c(0,NA)) +
    ylab("Spawning biomass") + geom_point() +
    geom_line(data=dfs,aes(x=Yr,y=SSB,col=as.factor(Sim)))+
    #geom_hline(yintercept=Cabc) +
    #geom_hline(yintercept=Cofl, linetype="dashed") +
    guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE)

  t3 <- p1/ p2 + plot_annotation(title = mytitle )
  return(t3)
}
# plot_res <- function(df, thisyr) {
#   p1 <- df %>%
#     filter(Alternative == 2) %>%
#     ggplot(aes(x = SSB, y = F)) +
#     geom_point(color = "salmon", alpha = .2) +
#     theme_few()
#
#   p2 <- df %>%
#     filter(Alternative == 2) %>%
#     ggplot(aes(x = SSB, y = ABC)) +
#     geom_point(alpha = .2, color = "blue") +
#     theme_few()
#
#   p3 <- df %>%
#     filter(Alternative == 2) %>%
#     ggplot(aes(x = log(SSB), y = log(Rec) / log(SSB))) +
#     geom_point(alpha = .2, color = "green", size = .5) +
#     geom_smooth() +
#     theme_few()
#
#   return(p1 / p2 / p3)
# }
# p1/p2/p3
