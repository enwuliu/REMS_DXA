library(meta)
library(tidyverse)
library(ggplotify)
library(ggpubr)
library(metafor)

dat<-read.csv("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\correlation coefficients_2025_11_29.csv",header=T)
dat$author <- gsub("\\?", "≥", dat$author)


create_meta_analysis <- function(is_specific, bone_site, data = dat, output_path = "D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\") {
  
  # Filter data based on country and bone site
  filtered_data <- data %>% 
    filter(grepl(bone_site, site), grepl(is_specific, Disease_specific))
  
  # Create study labels
  study <- paste(filtered_data$author, filtered_data$year)
  
  # Perform meta-analysis
  meta_result <- metacor(cor = filtered_data$r,
                         n = filtered_data$n,
                         studlab = study,
                         data = filtered_data,
                         common = FALSE,
                         random = TRUE,
                         prediction=TRUE,
                         method.tau = "DL",
                         method.random.ci = TRUE,
                         title = paste(bone_site, "BMD - Disease specific", is_specific))
  
  # Create output filename
  filename <- paste0("forest_plot_", gsub(" ", "_", tolower(bone_site)), "_", 
                     gsub(" ", "_", tolower(is_specific)), "_disease_sepcific.tiff")
  full_path <- paste0(output_path, filename)
  
  # Generate forest plot
  tiff(full_path, width = 9, height = 6, units = "in", res = 600)
  meta::forest(
    meta_result,
    digits = 3,
    overall = TRUE,
    prediction=TRUE,
    leftlabs = c("Study", "Sample size", "Weight", "Correlation (r) (95% CI)"),
    rightlabs = c("Weight", "Correlation (95% CI)"),
    
    # Aesthetic customizations
    col.square = "skyblue",
    col.square.lines = "steelblue",
    col.diamond = "steelblue",
    col.diamond.lines = "steelblue",
    col.study = "black",
    col.inside = "black",
    colgap = "8mm",
    colgap.forest.left = "2mm",
    colgap.forest.right = "15mm",
    just = "left",
    
    # Font sizes and layout
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
    
    # Statistical info
    print.tau2 = TRUE,
    print.I2 = TRUE,
    print.p = TRUE,
    
    # Make symbols smaller
    cex = 0.8
  )
  dev.off()
  
  # Return the meta-analysis result
  return(meta_result)
}



yes_femoral <- create_meta_analysis("Yes", "Femoral neck")
summary(yes_femoral)

no_femoral <- create_meta_analysis("No", "Femoral neck")
summary(no_femoral)



dat$z <- 0.5 * log((1 + dat$r) / (1 - dat$r))
dat$vz <- 1 / (dat$n - 3)

# Meta regression
femoral_data <- dat %>% filter(site == "Femoral neck")

model_femoral <- rma.mv(yi = z, V = vz, 
                        mods = ~ Disease_specific,  #predictor
                        random = ~ 1 | study_id,
                        data = femoral_data,
                        method = "REML")


summary(model_femoral)





yes_lumbar <- create_meta_analysis("Yes", "Lumbar spine")
summary(yes_lumbar)

no_lumbar <- create_meta_analysis("No", "Lumbar spine")

summary(no_lumbar)



# Meta regression
lumbar_data <- dat %>% filter(site == "Lumbar spine")

model_lumbar <- rma.mv(yi = z, V = vz, 
                        mods = ~ Disease_specific,  #predictor
                        random = ~ 1 | study_id,
                        data = lumbar_data,
                        method = "REML")


summary(model_lumbar)



