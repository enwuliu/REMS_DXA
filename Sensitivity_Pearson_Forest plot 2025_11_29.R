library(meta)
library(tidyverse)
library(ggplotify)
library(ggpubr)
library(readr)

dat<-read.csv("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\correlation coefficients_2025_11_29.csv",header=T)
dat$author <- gsub("\\?", "≥", dat$author)
dat<-dat %>% filter(Correlation_type=='Pearson')


fnd<-dat %>% filter(grepl('Femoral', site))

study<-paste(fnd$author,fnd$year)


# Perform meta-analysis
r.meta1 <- metacor(cor = fnd$r,
                   n = fnd$n,
                   studlab = study,
                   data = fnd,  # Added data frame name
                   common = FALSE,
                   random = TRUE,
                   method.tau = "DL",
                   method.random.ci = TRUE,
                   prediction = TRUE,
                   title = "Femoral neck BMD")

# With more precise column adjustments
#meta::forest(r.meta1)



tiff("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\forest_plot_pearson_fnd.tiff", width = 9, height = 6, units = "in", res = 600)
meta::forest(
  r.meta1,
  digits = 3,
  overall = TRUE,
  method.random.ci = TRUE,
    leftlabs = c("Study", "Sample size", "Weight", "Correlation (r)(95% CI)"),
  rightlabs = c("Weight", "Correlation (95% CI)"),
  
  # --- Aesthetic customizations ---
  col.square = "skyblue",         # light blue squares
  col.square.lines = "steelblue", # square borders
  col.diamond = "steelblue",      # diamond color
  col.diamond.lines = "steelblue",
  col.study = "black",
  col.inside = "black",
  colgap = "8mm",
  colgap.forest.left = "2mm",
  colgap.forest.right = "15mm",
  just = "left",
  
  # --- Font sizes and layout ---
  fs.heading = 11,
  fs.study = 10,
  fs.axis = 10,
  fs.smlab = 11,
  fs.common = 10,
  fs.random = 10,
  layout = "RevMan",
  xlab = "Correlation Coefficient (r)",
  xlim = c(-0.5, 1),
  fontsize = 10,
  
  # --- Statistical info ---
  print.tau2 = TRUE,
  print.I2 = TRUE,
  print.p = TRUE,
  
  # --- Make symbols smaller ---
  cex = 0.8                    # <1 makes everything smaller, including squares
)
dev.off()



lumbar<-dat %>% filter(grepl('Lumbar', site))

study2<-paste(lumbar$author,lumbar$year)

r.meta2 <- metacor(cor = lumbar$r,
                   n = lumbar$n,
                   studlab = study2,
                   data = lumbar,  # Added data frame name
                   common = FALSE,
                   random = TRUE,
                   method.tau = "DL",
                   method.random.ci = TRUE,
                   prediction = TRUE,
                   title = "Lumbar spine BMD")

tiff("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\forest_plot_pearson_lumbar.tiff", width = 9, height = 8, units = "in", res = 600)
meta::forest(
  r.meta2,
  digits = 3,
  overall = TRUE,
  prediction = TRUE,
  leftlabs = c("Study", "Sample size", "Weight", "Correlation (r)(95% CI)"),
  rightlabs = c("Weight", "Correlation (95% CI)"),
  
  # --- Aesthetic customizations ---
  col.square = "skyblue",         # light blue squares
  col.square.lines = "steelblue", # square borders
  col.diamond = "steelblue",      # diamond color
  col.diamond.lines = "steelblue",
  col.study = "black",
  col.inside = "black",
  colgap = "8mm",
  colgap.forest.left = "2mm",
  colgap.forest.right = "15mm",
  just = "left",
  
  # --- Font sizes and layout ---
  fs.heading = 11,
  fs.study = 10,
  fs.axis = 10,
  fs.smlab = 11,
  fs.common = 10,
  fs.random = 10,
  layout = "RevMan",
  xlab = "Correlation Coefficient (r)",
  xlim = c(-0.5, 1),
  fontsize = 10,
  
  # --- Statistical info ---
  print.tau2 = TRUE,
  print.I2 = TRUE,
  print.p = TRUE,
  
  # --- Make symbols smaller ---
  cex = 0.8                    # <1 makes everything smaller, including squares
)
dev.off()

