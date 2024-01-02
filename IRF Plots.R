################################################################################
#                       PLOT SVAR IRFS WITH GGPLOT                             # 
################################################################################

# Call libraries
library(devtools)
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")
library(tidyverse)
library(vars)
library(gridExtra)
library(backports)

################################################################################
# LEVEL IRF WITH GGPLOT
# Inputs
# SVAR_e: SVAR ESTIMATION using SVAR funtion from "vars" package
################################################################################


plot_irf <- function(SVAR_e, inpulse, response) {
  
  irf_p <- irf(SVAR_e, impulse = inpulse, response = response,
               n.ahead = 26, ortho = TRUE, boot = TRUE, cumulative = FALSE,
               ci = 0.95, runs = 500, model = "svarest")
  
  varirf <- extract_varirf(irf_p)
  # get col order
  
    
  irf_plot <- varirf %>% ggplot(aes(x = round(period), y = !!sym(paste0("irf_",tolower(inpulse),"_",tolower(response))),
               ymin = !!sym(paste0("lower_",tolower(inpulse),"_", tolower(response))),
               ymax = !!sym(paste0("upper_",tolower(inpulse),"_", tolower(response))))) +
             geom_hline(yintercept = 0, color = "#254431", linetype = "dashed") +
             geom_ribbon(fill = "#22AABE", alpha = 0.2, color = "grey50", linetype = "dotted") +
             geom_line(size = 0.8) +
             theme_light() +
             ylab(paste(gsub("\\.", " ", inpulse),"=> \n",  gsub("\\.", " ", response))) +
             xlab("Month") +
             theme(plot.title = element_text(size = 8, family = "Times"),
                   axis.title.y = element_text(size = 8, family = "Times")) + scale_x_continuous(breaks = 1:25)
           
  return(irf_plot)         
}



################################################################################
# CUMULATIVE IRF WITH GGPLOT
# Inputs
# SVAR_e: SVAR ESTIMATION using SVAR funtion from "vars" package
# Inpulse: Inpusle Variable Name
# Response: Response Varaible name
################################################################################

plot_irf_cum <- function(SVAR_e, inpulse, response) {
  
  irf_p <- irf(SVAR_e, impulse = inpulse, response = response,
               n.ahead = 26, ortho = TRUE, boot = TRUE, cumulative = TRUE,
               ci = 0.95, runs = 500, model = "svarest")

  
  varirf <- extract_varirf(irf_p)

  # get col order
  

  irf_plot <- varirf %>% ggplot(aes(x = round(period), y = !!sym(paste0("irf_",tolower(inpulse),"_",tolower(response))),
                                    ymin = !!sym(paste0("lower_",tolower(inpulse),"_",tolower(response))),
                                    ymax = !!sym(paste0("upper_",tolower(inpulse),"_",tolower(response))))) +
    geom_hline(yintercept = 0, color = "#254431", linetype = "dashed") +
    geom_ribbon(fill = "#22AABE", alpha = 0.2, color = "grey50", linetype = "dotted") +
    geom_line(size = 0.8) +
    theme_light() +
    ylab(paste("Cumulative Shock\n",gsub("\\.", " ", inpulse),"=> \n",  gsub("\\.", " ", response))) +
    xlab("Month")  +
    theme(plot.title = element_text(size = 8, family = "Times"),
          axis.title.y = element_text(size = 8, family = "Times")) + scale_x_continuous(breaks = 1:25)
  
  return(irf_plot)         
}

