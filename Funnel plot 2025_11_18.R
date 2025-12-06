
###funnel plot

library(metafor)
library(ggplot2)
library(tidyverse)


dat<-read.csv("D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\correlation coefficients_2025_11_29.csv",header=T)

fnd<-dat %>% filter(grepl('Femoral', site))

lumbar<-dat %>% filter(grepl('Lumbar', site))

####Femoral neck

dat <- data.frame(
  r = fnd$r,
  n = fnd$n
)

# Step 1: Fisher's z transformation
dat$z <- atanh(dat$r)  # Fisher's z-transform
dat$se <- 1 / sqrt(dat$n - 3)  # Standard error of z

# Step 2: Random-effects meta-analysis (Fisher’s z scale)
res <- rma(yi = z, sei = se, data = dat, method = "REML")

# funnel(res,
#        xlab = "Fisher’s z transformed correlation",
#        ylab = "Standard Error",
#        refline = res$b, # Adds vertical line at pooled effect
#        level = c(95),
#        main = "Begg's Funnel Plot of Meta-Analysis on Correlation Coefficients",
#        bg="white"
#        )

begg_test <- ranktest(res)
print(begg_test)

egger_test <- regtest(res)
print(egger_test)


# Prepare data
funnel_data <- data.frame(
  yi = dat$z,
  sei = dat$se
)


# Pooled (overall) estimate
b <- as.numeric(res$b)

# SE range (slightly extended for display)
max_se <- max(funnel_data$sei, na.rm = TRUE) * 1.1

# Coordinates for 95% CI triangle (no bottom line)
triangle <- data.frame(
  x = c(b - qnorm(0.975) * max_se, b, b + qnorm(0.975) * max_se),
  y = c(max_se, 0, max_se)
)

# Funnel plot
funnel_fnd<-ggplot() +
  # dashed 95% CI triangle outline (no bottom edge)
  geom_path(data = triangle,
            aes(x = x, y = y),
            color = "black", linewidth = 1.1, linetype = "dashed") +
  # solid black data points
  geom_point(data = funnel_data,
             aes(x = yi, y = sei),
             color = "black", size = 5) +
  # vertical reference line
  geom_vline(xintercept = b, linetype = "dashed", color = "black", linewidth=1.1) +
  # reverse y-axis (smaller SE on top)
  scale_y_reverse(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Fisher’s z transformed correlation",
    y = "Standard Error",
    title = "Begg's Rank Correlation Test for Funnel Plot Asymmetry (P=0.0602)\n Egger's Regression Based Test for Funnel Plot Asymmetry (P<0.0001)"
  ) +
  theme_classic(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 20),
    axis.title.x=element_text(size=30),
    panel.border = element_blank()
  )

funnel_fnd


ggsave(
  filename = "D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\funnel_fnd.tiff",
  plot = funnel_fnd,
  device = "tiff",
  units = "mm",
  width = 300,      # Fits within A4 margins
  height = 240,     # Adjust based on  needs (e.g., half-page)
  dpi = 600,        # High resolution for print
  bg = "white",     # Ensures white background (not transparent)
  compression = "lzw"  # Reduces file size without quality loss
)

###funnel Lumbar spine


dat2 <- data.frame(
  r = lumbar$r,
  n = lumbar$n
)

# Step 1: Fisher's z transformation
dat2$z <- atanh(dat2$r)  # Fisher's z-transform
dat2$se <- 1 / sqrt(dat2$n - 3)  # Standard error of z

# Step 2: Random-effects meta-analysis (Fisher’s z scale)
res <- rma(yi = z, sei = se, data = dat2, method = "REML")

# funnel(res,
#        xlab = "Fisher’s z transformed correlation",
#        ylab = "Standard Error",
#        refline = res$b, # Adds vertical line at pooled effect
#        level = c(95),
#        main = "Funnel Plot of Meta-Analysis on Correlation Coefficients",
#        bg="white"
#        )

begg_test <- ranktest(res)
print(begg_test)


egger_test <- regtest(res)
print(egger_test)



# Prepare data
funnel_data <- data.frame(
  yi = dat2$z,
  sei = dat2$se
)


# Pooled (overall) estimate
b <- as.numeric(res$b)

# SE range (slightly extended for display)
max_se <- max(funnel_data$sei, na.rm = TRUE) * 1.1

# Coordinates for 95% CI triangle (no bottom line)
triangle <- data.frame(
  x = c(b - qnorm(0.975) * max_se, b, b + qnorm(0.975) * max_se),
  y = c(max_se, 0, max_se)
)

# Funnel plot
funnel_lumbar<-ggplot() +
  # dashed 95% CI triangle outline (no bottom edge)
  geom_path(data = triangle,
            aes(x = x, y = y),
            color = "black", linewidth = 1.1, linetype = "dashed") +
  # solid black data points
  geom_point(data = funnel_data,
             aes(x = yi, y = sei),
             color = "black", size = 5) +
  # vertical reference line
  geom_vline(xintercept = b, linetype = "dashed", color = "black",linewidth = 1.1) +
  # reverse y-axis (smaller SE on top)
  scale_y_reverse(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Fisher’s z transformed correlation",
    y = "Standard Error",
    title = "Begg's Rank Correlation Test for Funnel Plot Asymmetry (P=0.2598)\n Egger's Regression Based Test for Funnel Plot Asymmetry (P=0.1398)"
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 22),
    axis.title.x=element_text(size=30),
    panel.border = element_blank()
  )

funnel_lumbar


ggsave(
  filename = "D:\\flinders\\REMS the accuracy of diagnostic tests for Osteoporosis\\funnel_lumbar.tiff",
  plot = funnel_lumbar,
  device = "tiff",
  units = "mm",
  width = 350,      # Fits within A4 margins
  height = 380,     # Adjust based on  needs (e.g., half-page)
  dpi = 600,        # High resolution for print
  bg = "white",     # Ensures white background (not transparent)
  compression = "lzw"  # Reduces file size without quality loss
)