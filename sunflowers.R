## Sunflowers activity, The Practice of Statistics and Doug Tyson
## R code by Neema Salimi
## some edits inc. population plot, plot annotations, 
## and heterogeneous/homogenous clusters by Amy Hogan 10/09/2017

#install.packages("tidyverse") !if not already
library(tidyverse)

setwd("~/Google Drive/AP Stats (personal)/Sunflowers")
sunflowers <- read_csv(file = "Sunflowers.csv")

#evaluates population SD
pop.var <- function(x) var(x) * (length(x)-1) / length(x)
pop.sd <- function(x) sqrt(pop.var(x))

#plot sunflower population
dot <- ggplot(data = sunflowers, aes(x = Healthy)) +
     geom_dotplot(binaxis = "x", stackgroups = TRUE,
                  color = "black", method = "histodot",
                  binpositions = "all",
                  stackratio = 1.2, binwidth = 0.1, dotsize = 0.9) +
     scale_x_continuous(breaks = seq(96, 110, 2), limits = c(96, 110)) +
     scale_y_continuous(NULL, breaks = NULL) +
     theme_bw() +
     labs(title = "Sunflowers", x = "# of Healthy Plants in Each Sq. Plot") +
     annotate("text",x=106,y=200,label=paste0("Mean=",
          paste(round(mean(sunflowers$Healthy),2)))) +
     annotate("text",x=106,y=190,label=paste0("SD=",
          paste(round(pop.sd(sunflowers$Healthy),2))))     

plot(dot)

srs.num <- 250
samp.size <- 10

# SRS sample
srs.means <- tibble(SRS.Mean = replicate(srs.num, {
  srs.sunflowers <- sunflowers[sample(nrow(sunflowers), samp.size, 
                                      replace = FALSE), 3]
  mean(srs.sunflowers$Healthy)
}))

# SRS plot
dot1 <- ggplot(data = srs.means, aes(x = SRS.Mean)) +
  geom_dotplot(binaxis = "x", stackgroups = TRUE,
               color = "black", method = "histodot",
               binpositions = "all",
               stackratio = 1.2, binwidth = 0.1, dotsize = 0.9) +
     scale_x_continuous(breaks = seq(98, 108, 2), limits = c(98, 108)) +
     scale_y_continuous(NULL, breaks = NULL) +
  theme_bw() +
  labs(title = "SRS", x = "Mean # of Healthy Plants") +
     annotate("text",x=106,y=200,label=paste0("Mean=",
          paste(round(mean(srs.means$SRS.Mean),2)))) +
     annotate("text",x=106,y=190,label=paste0("SD=",
          paste(round(sd(srs.means$SRS.Mean),2))))     

plot(dot1)

# Stratified by Row
strr.means <- tibble(STRR.Mean = replicate(srs.num, {
  strat.row <- sunflowers %>% group_by(Row) %>% sample_n(1)
  mean(strat.row$Healthy)
}))

# Stratified by Row plot
dot2 <- ggplot(data = strr.means, aes(x = STRR.Mean)) +
  geom_dotplot(binaxis = "x", stackgroups = TRUE,
               color = "black", method = "histodot",
               binpositions = "all",
               stackratio = 1.2, binwidth = 0.1, dotsize = 0.9) +
     scale_x_continuous(breaks = seq(98, 108, 2), limits = c(98, 108)) +
     scale_y_continuous(NULL, breaks = NULL) +
  theme_bw() +
  labs(title = "Stratified by Row", x = "Mean # of Healthy Plants") +
     annotate("text",x=106,y=200,label=paste0("Mean=",
          paste(round(mean(strr.means$STRR.Mean),2)))) +
     annotate("text",x=106,y=190,label=paste0("SD=",
          paste(round(sd(strr.means$STRR.Mean),2))))     

plot(dot2)

# Stratified by Column
strc.means <- tibble(STRC.Mean = replicate(srs.num, {
  strat.col <- sunflowers %>% group_by(Column) %>% sample_n(1)
  mean(strat.col$Healthy)
}))

# Stratified by Column plot
dot3 <- ggplot(data = strc.means, aes(x = STRC.Mean)) +
  geom_dotplot(binaxis = "x", stackgroups = TRUE,
               color = "black", method = "histodot",
               binpositions = "all",
               stackratio = 1.2, binwidth = 0.1, dotsize = 0.9) +
     scale_x_continuous(breaks = seq(98, 108, 2), limits = c(98, 108)) +
     scale_y_continuous(NULL, breaks = NULL) +
  theme_bw() +
  labs(title = "Stratified by Column", x = "Mean # of Healthy Plants") +
     annotate("text",x=106,y=200,label=paste0("Mean=",
          paste(round(mean(strc.means$STRC.Mean),2)))) +
     annotate("text",x=106,y=190,label=paste0("SD=",
          paste(round(sd(strc.means$STRC.Mean),2))))     

plot(dot3)

# Cluster Sample, vertical
clusv.means <- tibble(CLUSV.Mean = replicate(srs.num, {
  clus.vertical <- sunflowers %>% filter(ClusterV == sample(1:10, 1))
  mean(clus.vertical$Healthy)
}))

# Cluster plot, vertical
dot4 <- ggplot(data = clusv.means, aes(x = CLUSV.Mean)) +
  geom_dotplot(binaxis = "x", stackgroups = TRUE,
               color = "black", method = "histodot",
               binpositions = "all",
               stackratio = 1.2, binwidth = 0.1, dotsize = 0.9) +
     scale_x_continuous(breaks = seq(98, 108, 2), limits = c(98, 108)) +
     scale_y_continuous(NULL, breaks = NULL) +
  theme_bw() +
  labs(title = "Vertical Clusters", x = "Mean # of Healthy Plants") +
     annotate("text",x=106,y=200,label=paste0("Mean=",
          paste(round(mean(clusv.means$CLUSV.Mean),2)))) +
     annotate("text",x=106,y=190,label=paste0("SD=",
          paste(round(sd(clusv.means$CLUSV.Mean),2))))     

plot(dot4)

# Cluster sample, horizontal 
clush.means <- tibble(CLUSH.Mean = replicate(srs.num, {
     clus.horiz <- sunflowers %>% filter(ClusterH == sample(1:10, 1))
     mean(clus.horiz$Healthy)
}))

# Cluster plot, horizontal
dot5 <- ggplot(data = clush.means, aes(x = CLUSH.Mean)) +
     geom_dotplot(binaxis = "x", stackgroups = TRUE,
                  color = "black", method = "histodot",
                  binpositions = "all",
                  stackratio = 1.2, binwidth = 0.1, dotsize = 0.9) +
     scale_x_continuous(breaks = seq(98, 108, 2), limits = c(98, 108)) +
     scale_y_continuous(NULL, breaks = NULL) +
     theme_bw() +
     labs(title = "Horizontal Clusters", x = "Mean # of Healthy Plants") +
     annotate("text",x=106,y=200,label=paste0("Mean=",
          paste(round(mean(clush.means$CLUSH.Mean),2)))) +
     annotate("text",x=106,y=190,label=paste0("SD=",
          paste(round(sd(clush.means$CLUSH.Mean),2))))     

plot(dot5)