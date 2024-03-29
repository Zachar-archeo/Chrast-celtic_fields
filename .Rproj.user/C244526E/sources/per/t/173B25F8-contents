# Load required libraries
library(tidyverse)
library(corrplot)
library(psych)
library(readxl)

# CPrepare data 
data3 <- xrfData %>% 
          select (c(ID_Label,Ca,K,Mg,Mn,P,S))%>% 
          drop_na()

# Calculate correlation matrix for specific variables  
cor_matrix <- cor(data3[, 2:7])

# Calculate p-values for the correlation matrix
# List variables of the data you want to compute correlation on if necessary
p_values <- corr.test(data3[, 2:7])$p

# Create a correlation plot
# Possible methods:"color", "square", "circle", "shade", "number" 
corrplot(cor_matrix, 
         method = "circle", 
         type = "upper", 
         addCoef.col = 'black', 
         tl.cex = 1.0, 
         p.mat = p_values,sig.level = 0.05, insig = "pch",
         col =  COL2('RdYlBu', 6))

# Add color legend
corrplot.mixed(cor_matrix, 
               tl.cex = 1.0, 
               col = c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200)

# Optional: Save the correlation plot as an image file
# png("correlation_plot.png", width = 800, height = 800)
# corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 1.0, p.mat = p_values)
# corrplot.mixed(cor_matrix, tl.cex = 1.0, col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))
# dev.off()