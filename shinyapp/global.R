library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(hrbrthemes)
library(sf)


if (Sys.info()[['sysname']] =='Linux'){
  dir.create('~/.fonts')
  file.copy("www/ARIALN.TTF", "~/.fonts")
  system('fc-cache -f ~/.fonts')
}

# membaca data set Rds
ipm <- readRDS("data/ipm.Rds")
mapProv <- readRDS("data/mapProv.Rds")
mapKab <- readRDS("data/mapKab.Rds")

# list provisi untuk drop-down select
provList <- unique(ipm$Reg2)
provList <- c("NASIONAL", provList[provList != "NASIONAL" & !is.na(provList)])

# tahun terakhir yang tersedia
latestYear = max(as.character(ipm$Tahun))

# ggplot wrapper untuk generate chart
generateChart <- function(data, 
                          chart = "bar",
                          xvar, yvar, fillvar, 
                          llegend = NULL,
                          refline = NULL,
                          gtitle = NULL,
                          xtitle = NULL,
                          ytitle = NULL){
  
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  fillvar <- enquo(fillvar)
  
  if (chart == "bar"){
    p <- data %>% 
      ggplot(aes(x = reorder(!!xvar, !!yvar), 
                 y = !!yvar,
                 fill = !!fillvar)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = refline, color = "blue", linetype = "dashed") +
      coord_flip() +
      scale_fill_ipsum(labels = llegend)
    
  } else if (chart == "line"){
    p <- data %>%
      ggplot(aes(x = !!xvar, y = !!yvar, group = !!fillvar)) +
      geom_line(aes(color = !!fillvar), size = 1)+
      geom_point(aes(color = !!fillvar), size = 3) +
      scale_color_ipsum(labels = llegend)
  }
  
  p + 
  ggtitle(gtitle) +
  xlab(xtitle) + ylab(ytitle) +
  theme_ipsum() +
  theme(legend.title = element_blank(), 
        legend.position="bottom", 
        legend.box = "horizontal") 
}





