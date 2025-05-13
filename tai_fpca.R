# load data

nonlong <- read.csv('data_emphasisaccentmaybe_TAI.csv')
long <- read.csv('data_emphaticaccentmaybe_TAI_long.csv')

# load libraries

library(fdapace)
library(tidyverse)

# make FPCA inputs

long_input <- MakeFPCAInputs(
  IDs = long$filename,
  tVec = long$stepnumber,
  yVec = long$f0
)

# run FPCA 

fpca_accent <- FPCA(long_input$Ly, long_input$Lt,
                    list(plot = TRUE,
                         FVEthreshold = 0.95))

# construct data frame with PC scores

fpca_scores <- data.frame(
  filename = unlist(long_input$Lid),
  pc1 = fpca_accent$xiEst[,1],
  pc2 = fpca_accent$xiEst[,2],
  pc3 = fpca_accent$xiEst[,3]
)

# join PC scores to nonlong data frame

nonlong <- nonlong %>% left_join(fpca_scores)

# plot PC scores corresponding to clusters

par(mfrow = c(1, 2))

nonlong$cluster[which(nonlong$cluster == 1)] <- '1 (LH)'
nonlong$cluster[which(nonlong$cluster == 2)] <- '2 (HL)'
boxplot(nonlong$pc1 ~ as.factor(nonlong$cluster), col = c('red', 'blue'),
        ylab = 'PC1', xlab = 'Cluster')
plot(nonlong$pc1, nonlong$pc2, pch=20, xlab = 'PC1', ylab = 'PC2')
points(nonlong[nonlong$cluster=='1 (LH)','pc1'], 
       nonlong[nonlong$cluster=='1 (LH)','pc2'], 
       pch=20, col = 'red')
points(nonlong[nonlong$cluster=='2 (HL)','pc1'], 
       nonlong[nonlong$cluster=='2 (HL)','pc2'], 
       pch=20, col = 'blue')
text(-0.2, 0.47, lab = '1 (LH)', col = 'red', font = 2, pos = 1)
text(-0.2, 0.4, lab = '2 (HL)', col = 'blue', font = 2, pos = 1)

# make data frame for ggplot

forPlot <- data.frame(
  t = rep(1:20, 9),
  sd = rep(seq(-1, 1, by = 0.25), each = 20), 
  pc1 = rep(fpca_accent$phi[,1], 9)
)
forPlot$weighted <- fpca_accent$mu + sd(fpca_accent$xiEst[,1]) * forPlot$sd *
  fpca_accent$phi[,1]

# set theme

myTheme <- theme_minimal()
myTheme$legend.position <- 'bottom'
myTheme$text$size <- 16

# plot results

forPlot %>% ggplot + 
  aes(x = t, y = weighted, color = sd, group = sd) +
  scale_color_gradient2(low = 'blue', mid = 'darkgrey', high = 'red',
                        name = 'PC1 std.dev.') + 
  geom_line() +
  xlab('Measurement number') +
  ylab('Octave-median scaled f0') + 
  myTheme