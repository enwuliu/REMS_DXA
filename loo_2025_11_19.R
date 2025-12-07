###L#################Leave one out

library(metafor)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggpubr)

dat <- read.csv("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\correlation coefficients_2025_11_29.csv", header = TRUE)
dat$author <- gsub("\\?", "≥", dat$author)

fnd <- dat %>% filter(grepl('Femoral', site))
lumbar <- dat %>% filter(grepl('Lumbar', site))
#########Femoral neck


dat_fnd_loo <- data.frame(
  author=fnd$author,
  year=fnd$year,
  r = fnd$r,
  n = fnd$n
)

# Step 1: Fisher's z transformation
dat_fnd_loo$z <- atanh(dat_fnd_loo$r)  # Fisher's z-transform
dat_fnd_loo$se <- 1 / sqrt(dat_fnd_loo$n - 3)  # Standard error of z

# Step 2: Random-effects meta-analysis (Fisher’s z scale)
res <- rma(yi = z, sei = se, data = dat_fnd_loo, method = "REML")

# Perform leave-one-out analysis
loo_analysis <- leave1out(res)
print(loo_analysis)

# Create a more comprehensive results table
comprehensive_loo <- data.frame(
  Study = paste(dat_fnd_loo$author,dat_fnd_loo$year),
  Estimate = loo_analysis$estimate,
  SE = loo_analysis$se,
  CI_lower = loo_analysis$ci.lb,
  CI_upper = loo_analysis$ci.ub,
  I2 = loo_analysis$I2,
  Tau2 = loo_analysis$tau2
)

print(comprehensive_loo)

comprehensive_loo <- comprehensive_loo %>%
  arrange(desc(grepl("Casciaro", Study)))



############

tiff("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\figure 10 loo_fnd.tiff", width = 12.5, height = 10, units = "in", res = 600)


# Create a customized leave-one-out forest plot
par(mar = c(3, 1, 1, 1),mgp = c(2, 0.7, 0))


# Calculate range for x-axis limits
all_estimates <- c(comprehensive_loo$CI_lower, comprehensive_loo$CI_upper, res$ci.lb, res$ci.ub)
x_limits <- c(min(all_estimates, na.rm = TRUE)-0.5, 
              max(all_estimates, na.rm = TRUE) + 0.4)

# Calculate metrics for summary box
effect_differences <- abs(comprehensive_loo$Estimate - res$b)
max_diff <- max(effect_differences)
max_diff_study <- comprehensive_loo$Study[which.max(effect_differences)]
range_estimates <- range(comprehensive_loo$Estimate)
mean_abs_diff <- mean(effect_differences)
influential_threshold <- 0.1
influential_count <- sum(effect_differences > influential_threshold)

# Create the plot
plot(NA, NA, xlim = x_limits, ylim = c(0.5, nrow(comprehensive_loo) + 1.0),
     xlab = "Effect Size (Fisher's z)",cex.lab=1.2, ylab = "", yaxt = "n",
     main = "Leave-One-Out Sensitivity Analysis-Correlation at Femoral Neck")

# Add reference line at zero
abline(v = 0, lty = 2, col = "gray")

# Add reference line for original estimate
abline(v = res$b, lty = 1, col = "red", lwd = 2)

# Add results for each leave-one-out analysis
for(i in 1:nrow(comprehensive_loo)) {
  y_pos <- nrow(comprehensive_loo) - i + 1
  # Add point estimate
  points(comprehensive_loo$Estimate[i], y_pos, pch = 16, cex = 1.2)
  # Add confidence interval
  lines(c(comprehensive_loo$CI_lower[i], comprehensive_loo$CI_upper[i]), 
        c(y_pos, y_pos), lwd = 2)
  # Add study label
  text(x_limits[1] + 0.1, y_pos, comprehensive_loo$Study[i], pos = 4, cex = 1.5)
}

# ---------------------------------------------------------------
# Add Summary Box
# ---------------------------------------------------------------
summary_x <- x_limits[2] - 0.3
box_width <- 0.35

# Create summary box (no border)
rect(summary_x - 0.1, 2,
     summary_x + box_width, nrow(comprehensive_loo) + 1.5,
     col = "white", border = NA)

# Summary content
robustness_assessment <- ifelse(max_diff < 0.1, "HIGH", "MODERATE")
robustness_color <- ifelse(max_diff < 0.1, "darkgreen", "orange")

summary_lines <- c(
  "SENSITIVITY SUMMARY",
  "-------------------",
  paste("Robustness:", robustness_assessment),
  paste("Orig ES:", sprintf("%.3f", res$b)),
  paste("LOO Range:", sprintf("%.3f-%.3f", range_estimates[1], range_estimates[2])),
  paste("Mean |Δ|:", sprintf("%.3f", mean_abs_diff)),
  paste("Max |Δ|:", sprintf("%.3f", max_diff)),
  paste("Influential studies:", influential_count),
  paste("Most influential:"),
  paste(substr(max_diff_study, 1, 12), "")
)

# Display summary
y_start <- nrow(comprehensive_loo)
line_height <- 0.4

for (i in 1:length(summary_lines)) {
  line <- summary_lines[i]
  
  font_type <- 1
  text_color <- "black"
  text_size <- 1.2
  
  if (i == 1) {
    font_type <- 2
    text_color <- "darkblue"
    text_size <- 1.2
  } else if (i == 3) {
    font_type <- 2
    text_color <- robustness_color
  } else if (i == 8) {
    font_type <- 2
  }
  
  text(summary_x, y_start - (i-1) * line_height, line,
       pos = 4, cex = text_size, font = font_type, col = text_color)
}

# Add legend without border, aligned to summary box
legend(x = summary_x-0.07, y = 1,  # Position aligned with summary box
       legend = c("Leave-One-Out Estimates", "Original Pooled Estimate"), 
       pch = c(16, NA), 
       lty = c(1, 1), 
       col = c("black", "red"),
       lwd = c(2, 2),
       bty = "n",  # Removes the border
       xjust = 0,  # Left-align to summary box position
       yjust = 0,  # Bottom-align
       cex = 1.2)  # Slightly smaller text

# Identify influential studies
influential_studies <- comprehensive_loo[effect_differences > influential_threshold, ]

cat("\n=== POTENTIALLY INFLUENTIAL STUDIES ===\n")
if(nrow(influential_studies) > 0) {
  print(influential_studies)
} else {
  cat("No studies exceeded the influence threshold of", influential_threshold, "\n")
}

# Create summary statistics
cat("\n=== SUMMARY OF LEAVE-ONE-OUT ANALYSIS ===\n")
cat("Original pooled estimate:", round(res$b, 3), "\n")
cat("Range of leave-one-out estimates:", 
    round(min(comprehensive_loo$Estimate), 3), "to", 
    round(max(comprehensive_loo$Estimate), 3), "\n")
cat("Mean absolute difference:", round(mean(effect_differences), 3), "\n")
cat("Maximum absolute difference:", round(max(effect_differences), 3), "\n")

dev.off()




########## Lumbar spine


dat_lumbar_loo <- data.frame(
  author=lumbar$author,
  year=lumbar$year,
  r = lumbar$r,
  n = lumbar$n
)

# Step 1: Fisher's z transformation
dat_lumbar_loo$z <- atanh(dat_lumbar_loo$r)  # Fisher's z-transform
dat_lumbar_loo$se <- 1 / sqrt(dat_lumbar_loo$n - 3)  # Standard error of z

# Step 2: Random-effects meta-analysis (Fisher’s z scale)
res <- rma(yi = z, sei = se, data = dat_lumbar_loo, method = "REML")

# Perform leave-one-out analysis
loo_analysis <- leave1out(res)
print(loo_analysis)

# Create a more comprehensive results table
comprehensive_loo <- data.frame(
  Study = paste(dat_lumbar_loo$author,dat_lumbar_loo$year),
  Estimate = loo_analysis$estimate,
  SE = loo_analysis$se,
  CI_lower = loo_analysis$ci.lb,
  CI_upper = loo_analysis$ci.ub,
  I2 = loo_analysis$I2,
  Tau2 = loo_analysis$tau2
)

print(comprehensive_loo)


tiff("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\figure 11 loo_lumbar.tiff", width = 12.6, height = 12, units = "in", res = 600)


# Create a customized leave-one-out forest plot
par(mar = c(3, 1, 1, 1),mgp = c(2, 0.7, 0))


# Calculate range for x-axis limits
all_estimates <- c(comprehensive_loo$CI_lower, comprehensive_loo$CI_upper, res$ci.lb, res$ci.ub)
x_limits <- c(min(all_estimates, na.rm = TRUE)-0.6, 
              max(all_estimates, na.rm = TRUE) + 0.4)

# Calculate metrics for summary box
effect_differences <- abs(comprehensive_loo$Estimate - res$b)
max_diff <- max(effect_differences)
max_diff_study <- comprehensive_loo$Study[which.max(effect_differences)]
range_estimates <- range(comprehensive_loo$Estimate)
mean_abs_diff <- mean(effect_differences)
influential_threshold <- 0.1
influential_count <- sum(effect_differences > influential_threshold)

# Create the plot
plot(NA, NA, xlim = x_limits, ylim = c(0.5, nrow(comprehensive_loo) + 1.0),
     xlab = "Effect Size (Fisher's z)",cex.lab=1.2, ylab = "", yaxt = "n",
     main = "Leave-One-Out Sensitivity Analysis-Correlation at Lumbar Spine")

# Add reference line at zero
abline(v = 0, lty = 2, col = "gray")

# Add reference line for original estimate
abline(v = res$b, lty = 1, col = "red", lwd = 2)

# Add results for each leave-one-out analysis
for(i in 1:nrow(comprehensive_loo)) {
  y_pos <- nrow(comprehensive_loo) - i + 1
  # Add point estimate
  points(comprehensive_loo$Estimate[i], y_pos, pch = 16, cex = 1.2)
  # Add confidence interval
  lines(c(comprehensive_loo$CI_lower[i], comprehensive_loo$CI_upper[i]), 
        c(y_pos, y_pos), lwd = 2)
  # Add study label
  text(x_limits[1] + 0.1, y_pos, comprehensive_loo$Study[i], pos = 4, cex = 1.5)
}

# ---------------------------------------------------------------
# Add Summary Box
# ---------------------------------------------------------------
summary_x <- x_limits[2] - 0.3
box_width <- 0.35

# Create summary box (no border)
rect(summary_x - 0.15, 1.0,
     summary_x + box_width, nrow(comprehensive_loo) + 1.0,
     col = "white", border = NA)

# Summary content
robustness_assessment <- ifelse(max_diff < 0.1, "HIGH", "MODERATE")
robustness_color <- ifelse(max_diff < 0.1, "darkgreen", "orange")

summary_lines <- c(
  "SENSITIVITY SUMMARY",
  "-------------------",
  paste("Robustness:", robustness_assessment),
  paste("Orig ES:", sprintf("%.3f", res$b)),
  paste("LOO Range:", sprintf("%.3f-%.3f", range_estimates[1], range_estimates[2])),
  paste("Mean |Δ|:", sprintf("%.3f", mean_abs_diff)),
  paste("Max |Δ|:", sprintf("%.3f", max_diff)),
  paste("Influential studies:", influential_count),
  paste("Most influential:"),
  paste(substr(max_diff_study, 1, 31),"")
)

# Display summary
y_start <- nrow(comprehensive_loo)
line_height <- 0.8

for (i in 1:length(summary_lines)) {
  line <- summary_lines[i]
  
  font_type <- 1
  text_color <- "black"
  text_size <- 1.2
  
  if (i == 1) {
    font_type <- 2
    text_color <- "darkblue"
    text_size <- 1.2
  } else if (i == 3) {
    font_type <- 2
    text_color <- robustness_color
  } else if (i == 8) {
    font_type <- 2
  }
  
  text(summary_x-0.1, y_start - (i-1) * line_height, line,
       pos = 4, cex = text_size, font = font_type, col = text_color)
}

# Add legend without border, aligned to summary box
legend(x = summary_x-0.09, y = 2,  # Position aligned with summary box
       legend = c("Leave-One-Out Estimates", "Original Pooled Estimate"), 
       pch = c(16, NA), 
       lty = c(1, 1), 
       col = c("black", "red"),
       lwd = c(2, 2),
       bty = "n",  # Removes the border
       xjust = 0,  # Left-align to summary box position
       yjust = 0,  # Bottom-align
       cex = 1.2)  # Slightly smaller text

# Identify influential studies
influential_studies <- comprehensive_loo[effect_differences > influential_threshold, ]

cat("\n=== POTENTIALLY INFLUENTIAL STUDIES ===\n")
if(nrow(influential_studies) > 0) {
  print(influential_studies)
} else {
  cat("No studies exceeded the influence threshold of", influential_threshold, "\n")
}

# Create summary statistics
cat("\n=== SUMMARY OF LEAVE-ONE-OUT ANALYSIS ===\n")
cat("Original pooled estimate:", round(res$b, 3), "\n")
cat("Range of leave-one-out estimates:", 
    round(min(comprehensive_loo$Estimate), 3), "to", 
    round(max(comprehensive_loo$Estimate), 3), "\n")
cat("Mean absolute difference:", round(mean(effect_differences), 3), "\n")
cat("Maximum absolute difference:", round(max(effect_differences), 3), "\n")


dev.off()



