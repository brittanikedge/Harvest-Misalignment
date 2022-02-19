library(here)
library(sf)
library(ggplot2)
library(tmap)
library(ggcorrplot)
library(patchwork)
library(flextable)
library(officer)
library(parallel)
library(tidyverse)
library(data.table)
library(dplyr)

##### Jensen's inequality figure #####
# Jensen's Inequality Figures #

soy_function <- function(x){
  yield <- 250 * (1 - exp(-.035 * (20 + x)))
  return(yield)
}

r1 <- 30
r2 <- 100

r3 <- 120
r4 <- 190

gap_data <- data.frame(X = c(.5*r1 + .5*r2, 
                             .5*r1 + .5*r2,
                             .3*r1 + .7*r2,
                             .3*r1 + .7*r2,
                             .5*r3 + .5*r4,
                             .5*r3 + .5*r4),
                       Y = c(.5*soy_function(r1) + .5*soy_function(r2),
                             soy_function(.5*r1 + .5*r2),
                             .3*soy_function(r1) + .7*soy_function(r2),
                             soy_function(.3*r1 + .7*r2),
                             .5*soy_function(r3) + .5*soy_function(r4),
                             soy_function(.5*r3 + .5*r4)),
                       pair = c(1, 1, 2, 2, 3, 3))

length_data <- data.frame(length = c(soy_function(.5*r1 + .5*r2) - (.5*soy_function(r1) + .5*soy_function(r2)),
                                     soy_function(.3*r1 + .7*r2) - (.3*soy_function(r1) + .7*soy_function(r2)),
                                     soy_function(.5*r3 + .5*r4) - (.5*soy_function(r3) + .5*soy_function(r4))),
                          X = c(.5*r1 + .5*r2,
                                .3*r1 + .7*r2,
                                .5*r3 + .5*r4),
                          Y = c(255,
                                255,
                                255),
                          pair = c(1, 2, 3))

label_data <- data.frame(label = c("A",
                                   "B",
                                   "C"),
                         X = c(.5*r1 + .5*r2,
                               .3*r1 + .7*r2,
                               .5*r3 + .5*r4),
                         Y = c(max(soy_function(r1), soy_function(r2)) + 5,
                               max(soy_function(r1), soy_function(r2)) + 5,
                               max(soy_function(r3), soy_function(r4)) + 5))

new_data <- data.frame(X = c(r1, r2, r1, r2),
                       Y = c(soy_function(r1), soy_function(r2),  soy_function(r1),  soy_function(r2)),
                       pair = c(1, 1, 2, 2))

base <- ggplot(new_data, aes(X, Y)) 

weighting1 <- base + geom_function(fun = soy_function) +
  geom_point(aes(x = r1, y = soy_function(r1))) +
  geom_point(aes(x = r2, y = soy_function(r2))) +
  geom_point(aes(x = .5*r1 + .5*r2, y = .5*soy_function(r1) + .5*soy_function(r2)), shape = 0) +
  geom_point(aes(x = .5*r1 + .5*r2, y = soy_function(.5*r1 + .5*r2)), shape = 0) +
  geom_line(linetype = "dashed", aes (group = pair)) +
  geom_line(data = subset(gap_data, pair == 1), linetype = "dotted", aes (group = pair)) +
  geom_label(data = subset(length_data, pair == 1),
             aes(label = round(length, 2)),
             size = 8) +
  xlim(0, 200) +
  ylim(175, 260) +
  labs( y="Yield", x = "Seeding Rate (thousands/acre)")

weighting2 <- base + geom_function(fun = soy_function) +
  geom_point(aes(x = r1, y = soy_function(r1))) +
  geom_point(aes(x = r2, y = soy_function(r2))) +
  geom_point(aes(x = .3*r1 + .7*r2, y = .3*soy_function(r1) + .7*soy_function(r2)), shape = 5) +
  geom_point(aes(x = .3*r1 + .7*r2, y = soy_function(.3*r1 + .7*r2)), shape = 5) +
  geom_line(linetype = "dashed", aes (group = pair)) +
  geom_line(data = subset(gap_data, pair == 2), linetype = "dotted", aes (group = pair), linetype = "dashed") +
  geom_label(data = subset(length_data, pair == 2),
             aes(label = round(length, 2)),
             size = 8) +
  xlim(0, 200) +
  ylim(175, 260) +
  labs( y="Yield", x = "Seeding Rate (thousands/acre)")

new_data <- data.frame(X = c(r3, r4),
                       Y = c(soy_function(r3), soy_function(r4)),
                       pair = c(3, 3))
base <- ggplot(new_data, aes(X, Y)) 

weighting3 <- base + geom_function(fun = soy_function) +
  geom_point(aes(x = r3, y = soy_function(r3))) +
  geom_point(aes(x = r4, y = soy_function(r4))) +
  geom_point(aes(x = .5*r3 + .5*r4, y = .5*soy_function(r3) + .5*soy_function(r4)), shape = 15) +
  geom_point(aes(x = .5*r3 + .5*r4, y = soy_function(.5*r3 + .5*r4)), shape = 15) +
  geom_line(linetype = "dashed", aes (group = pair)) +
  geom_line(data = subset(gap_data, pair == 3), linetype = "dotted", aes (group = pair)) +
  geom_label(data = subset(length_data, pair == 3),
             aes(label = round(length, 2)),
             size = 8) +
  xlim(0, 200) +
  ylim(175, 260) +
  labs( y="Yield", x = "Seeding Rate (thousands/acre)")

jensens <- (weighting1)
saveRDS(jensens, here("Results/jensens_inequality_figure.rds"))

##### uploading the results #####
results_mismatch <- readRDS(file = here("Results", paste0("results_", paste0(c("mismatch"), collapse = "_"), "20sd", ".rds")))
results_mismatch <- na.omit(results_mismatch)
results_mismatch$profit_diff <- as.numeric(unlist(results_mismatch$profit_diff))
results_mismatch$profit_diff_clean <- as.numeric(unlist(results_mismatch$profit_diff_clean))
results_mismatch$no_obs_clean <- as.numeric(unlist(results_mismatch$no_obs_clean))
results_mismatch$no_obs_all <- as.numeric(unlist(results_mismatch$no_obs_all))
results_mismatch$rate_clean <- as.numeric(unlist(results_mismatch$rate_clean))
results_mismatch$rate <- as.numeric(unlist(results_mismatch$rate))
results_mismatch$diff <- as.numeric(unlist(results_mismatch$diff))
results_mismatch$rate_diff <- results_mismatch$rate_clean - results_mismatch$opt_n
results_mismatch$rate_diff_raw <- results_mismatch$rate - results_mismatch$opt_n
results_mismatch$rate_diff_clean <- results_mismatch$rate_clean - results_mismatch$rate 
results_mismatch$rate_diff_cleaning <- results_mismatch$rate_clean - results_mismatch$rate

levels(results_mismatch$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")
levels(results_mismatch$ys_type) <- c("Low Curvature", "Middle Curvature", "High Curvature")

results_others <- readRDS(file = here("Results", paste0("results_", paste0(c("angle", "mis-alignment"), collapse = "_"), "20sd", ".rds")))
results_others <- na.omit(results_others)
results_others$profit_diff <- as.numeric(unlist(results_others$profit_diff))
results_others$profit_diff_clean <- as.numeric(unlist(results_others$profit_diff_clean))
results_others$no_obs_clean <- as.numeric(unlist(results_others$no_obs_clean))
results_others$no_obs_all <- as.numeric(unlist(results_others$no_obs_all))
results_others$rate_clean <- as.numeric(unlist(results_others$rate_clean))
results_others$rate <- as.numeric(unlist(results_others$rate))
results_others$rate_diff_raw <- results_others$rate - results_others$opt_n
results_others$rate_diff_cleaning <- results_others$rate_clean - results_others$rate

results_others$rate_diff_cleaning <- results_others$rate_clean - results_others$rate

levels(results_others$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")
levels(results_others$ys_type) <- c("Low Curvature", "Middle Curvature", "High Curvature")


##### Profit figures without cleaning ####
results_others_means_raw <- results_others[ ,list(mean_diff = mean(profit_diff)), by = list(error_degree, ys_type, alignment_case, rate_types)]

results_mismatch_means_raw <- results_mismatch[ ,list(mean_diff = mean(profit_diff)), by = list(error_degree, ys_type, alignment_case, rate_types)]

profitmismatch <- ggplot(data = subset(results_mismatch_means_raw, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ rate_types) +
  ylim(0, 12) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Yield Response")) + 
  theme(axis.title.y=element_blank(),
        legend.position = "none")

profitangle <- ggplot(data = subset(results_others_means_raw, alignment_case == "angle" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ rate_types) +
  ylim(0, 12) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Yield Response")) + 
  theme(axis.title.x=element_blank())

profitshift <- ggplot(data = subset(results_others_means_raw, alignment_case == "mis-alignment" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ rate_types) +
  ylim(0, 12) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Yield Response")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

profit_no_cleaning <- (profitshift / profitangle / profitmismatch) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))
 
saveRDS(profit_no_cleaning, here("Results/profit_no_cleaning.rds"))

##### Profit density without cleaning ####
profit_density_angle <- ggplot(data = subset(results_others, alignment_case == "angle" & error_degree != 0)) +
  geom_density(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Profit Difference from Optimal") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
saveRDS(profit_density_angle, here("Results/profit_density_angle.rds"))

profit_density_shift <- ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Profit Difference from Optimal") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
saveRDS(profit_density_shift, here("Results/profit_density_shift.rds"))

profit_density_mismatch <- ggplot(data = subset(results_mismatch, error_degree != 1)) +
  geom_density(aes(x = profit_diff, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Profit Difference from Optimal") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
saveRDS(profit_density_mismatch, here("Results/profit_density_mismatch.rds"))

##### Profit figures with cleaning #####
results_others_means <- results_others[ ,list(mean_diff = mean(diff)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

results_mismatch_means <- results_mismatch[ ,list(mean_diff = mean(diff)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

profit_clean_mismatch <- ggplot(data = subset(results_mismatch_means, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) + 
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter")) + 
  theme(axis.title.y=element_blank(),
        legend.position = "none")

profit_clean_shift <- ggplot(data = subset(results_others_means, error_degree != 0 & alignment_case == "mis-alignment"), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter")) + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

profit_clean_angle <- ggplot(data = subset(results_others_means, error_degree != 0 & alignment_case == "angle"), aes(x = as.factor(error_degree), y = as.numeric(mean_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rate_types ~ ys_type) +
  xlab("Error Level") +
  ylab("Profit Difference from Optimal") + 
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter")) + 
  theme(axis.title.x=element_blank())

profit_clean_shift
saveRDS(profit_clean_shift, here("Results/profit_clean_shift.rds"))

profit_clean_angle 
saveRDS(profit_clean_angle, here("Results/profit_clean_angle.rds"))

profit_clean_mismatch
saveRDS(profit_clean_mismatch, here("Results/profit_clean_mismatch.rds"))


##### Profit density with cleaning #####
profit_density_mismatch_clean_75 <- ggplot(data = subset(results_mismatch, error_degree == 0.75)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Profit Difference from Cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(profit_density_mismatch_clean_75, here("Results/profit_density_mismatch_clean_75.rds"))

profit_density_mismatch_clean_125 <- ggplot(data = subset(results_mismatch, error_degree == 1.25)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Profit Difference from Cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(profit_density_mismatch_clean_125, here("Results/profit_density_mismatch_clean_125.rds"))

profit_density_shift_clean_30 <- ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree == 30)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Profit Difference from Cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(profit_density_shift_clean_30, here("Results/profit_density_shift_clean_30.rds"))

profit_density_shift_clean_10 <- ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree == 10)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Profit Difference from Cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(profit_density_shift_clean_10, here("Results/profit_density_shift_clean_10.rds"))

profit_density_angle_clean_30 <- ggplot(data = subset(results_others, alignment_case == "angle" & error_degree == 30)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Profit Difference from Cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(profit_density_angle_clean_30, here("Results/profit_density_angle_clean_30.rds"))

profit_density_angle_clean_10 <- ggplot(data = subset(results_others, alignment_case == "angle" & error_degree == 10)) +
  geom_density(aes(x = diff, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Profit Difference from Cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(profit_density_angle_clean_10, here("Results/profit_density_angle_clean_10.rds"))


##### Rate without cleaning #####
results_mismatch_means_rate_raw <- results_mismatch[ ,list(mean_rate_diff = mean(rate_diff_raw)), by = list(error_degree, ys_type, alignment_case, rate_types, max_dev)]

results_others_means_rate_raw <- results_others[ ,list(mean_rate_diff = mean(rate_diff_raw)), by = list(error_degree, ys_type, alignment_case, rate_types)]

rateangle <- ggplot(data = subset(results_others_means_rate_raw, alignment_case == "angle" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(-10, 45) +
  xlab("Error Level") +
  ylab("Difference between \nEstimated and True Optimal Rate") + 
  facet_grid(~ rate_types) +
  guides(fill = guide_legend(title = "Yield Response")) + 
  theme(axis.title.x=element_blank())

rateshift <- ggplot(data = subset(results_others_means_rate_raw, alignment_case == "mis-alignment" & error_degree != 0), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(-10, 45) +
  xlab("Error Level") +
  ylab("Difference between \nEstimated and True Optimal Rate") + 
  facet_grid(~ rate_types) +
  guides(fill = guide_legend(title = "Yield Response")) + 
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ratemismatch <- ggplot(data = subset(results_mismatch_means_rate_raw, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(ys_type))) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(-10, 45) +
  xlab("Error Level") +
  ylab("Difference between \nEstimated and True Optimal Rate") + 
  facet_grid(~ rate_types) +
  guides(fill = guide_legend(title = "Yield Response")) + 
  theme(axis.title.y=element_blank(),
        legend.position = "none")

rate_no_cleaning <- (rateshift / rateangle / ratemismatch)  +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))
saveRDS(rate_no_cleaning, here("Results/rate_no_cleaning.rds"))

##### Rate with cleaning #####
results_mismatch_means_rate <- results_mismatch[ ,list(mean_rate_diff = mean(rate_diff_clean)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

results_others_means_rate <- results_others[ ,list(mean_rate_diff = mean(rate_diff_cleaning)), by = list(error_degree, ys_type, alignment_case, max_dev, rate_types)]

rate_clean_mismatch <- ggplot(data = subset(results_mismatch_means_rate, error_degree != 1), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Clean and \nRaw Estimated Optimal Input") + 
  facet_grid(rate_types ~ ys_type) +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

rate_clean_shift <- ggplot(data = subset(results_others_means_rate, error_degree != 0 & alignment_case == "mis-alignment"), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Clean and \nRaw Estimated Optimal Input") + 
  facet_grid(rate_types ~ ys_type) +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

rate_clean_angle <- ggplot(data = subset(results_others_means_rate, error_degree != 0 & alignment_case == "angle"), aes(x = as.factor(error_degree), y = as.numeric(mean_rate_diff), fill = as.factor(max_dev))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Error Level") +
  ylab("Difference between Clean and \nRaw Estimated Optimal Input") + 
  facet_grid(rate_types ~ ys_type) +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))

rate_clean_shift  
saveRDS(rate_clean_shift, here("Results/rate_clean_shift.rds"))

rate_clean_angle  
saveRDS(rate_clean_angle, here("Results/rate_clean_angle.rds"))

rate_clean_mismatch
saveRDS(rate_clean_mismatch, here("Results/rate_clean_mismatch.rds"))


##### Rate density without cleaning #####
rate_density_mismatch <- ggplot(data = subset(results_mismatch, error_degree != 1)) +
  geom_density(aes(x = rate_diff_raw, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Difference between Estimated and True Optimal Rate") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
saveRDS(rate_density_mismatch, here("Results/rate_density_mismatch.rds"))

rate_density_angle <- ggplot(data = subset(results_others,  alignment_case == "angle" & error_degree != 0)) +
  geom_density(aes(x = rate_diff_raw, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Difference between Estimated and True Optimal Rate") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
saveRDS(rate_density_angle, here("Results/rate_density_angle.rds"))

rate_density_shift <- ggplot(data = subset(results_others,  alignment_case == "mis-alignment" & error_degree != 0)) +
  geom_density(aes(x = rate_diff_raw, fill = factor(error_degree)), alpha = 0.7, binwidth = 1) +
  facet_grid(rate_types ~ ys_type, scale = "free") +
  xlab("Difference between Estimated and True Optimal Rate") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Error Level"))
saveRDS(rate_density_shift, here("Results/rate_density_shift.rds"))

##### Rate density with cleaning #####
rate_density_mismatch_clean_75 <- ggplot(data = subset(results_mismatch, error_degree == 0.75)) +
  geom_density(aes(x = rate_diff_cleaning, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Difference between estimated rate after cleaning and before cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(rate_density_mismatch_clean_75, here("Results/rate_density_mismatch_clean_75.rds"))

rate_density_mismatch_clean_125 <- ggplot(data = subset(results_mismatch, error_degree == 1.25)) +
  geom_density(aes(x = rate_diff_cleaning, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Difference between estimated rate after cleaning and before cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(rate_density_mismatch_clean_125, here("Results/rate_density_mismatch_clean_125.rds"))

rate_density_shift_clean_30 <- ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree == 30)) +
  geom_density(aes(x = rate_diff_cleaning, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Difference between estimated rate after cleaning and before cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(rate_density_shift_clean_30, here("Results/rate_density_shift_clean_30.rds"))

rate_density_shift_clean_10 <- ggplot(data = subset(results_others, alignment_case == "mis-alignment" & error_degree == 10)) +
  geom_density(aes(x = rate_diff_cleaning, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Difference between estimated rate after cleaning and before cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(rate_density_shift_clean_10, here("Results/rate_density_shift_clean_10.rds"))

rate_density_angle_clean_30 <- ggplot(data = subset(results_others, alignment_case == "angle" & error_degree == 30)) +
  geom_density(aes(x = rate_diff_cleaning, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Difference between estimated rate after cleaning and before cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(rate_density_angle_clean_30, here("Results/rate_density_angle_clean_30.rds"))

rate_density_angle_clean_10 <- ggplot(data = subset(results_others, alignment_case == "angle" & error_degree == 10)) +
  geom_density(aes(x = rate_diff_cleaning, fill = factor(max_dev)), alpha = 0.7, binwidth = 1) +
  xlab("Difference between estimated rate after cleaning and before cleaning") +
  ylab("Density") + 
  facet_grid(ys_type ~ rate_types, scale = "free") +
  guides(fill = guide_legend(title = "Maximum Deviation \nParameter"))
saveRDS(rate_density_angle_clean_10, here("Results/rate_density_angle_clean_10.rds"))

#### Factor figures #####
f_high <- function(x) {
  y = 250 * (1 - exp(-.035 * (20 + x)))
  return(y)
}

f_low <- function(x){
  y = 250 * (1 - exp(-.009 * (160 + x)))
  return(y)
}

f_mid <- function(x){
  y = 250 * (1 - exp(-.02 * (45 + x)))
  return(y)
}

final_results_mismatch_nitrogen <- readRDS(here("Results", "deterministic_clean_data_mismatch_nitrogen.rds"))
det_est_data_mismatch_nitrogen <- readRDS(here("Results", "deterministic_clean_est_mismatch_nitrogen.rds"))

final_results_mismatch_nitrogen <- subset(final_results_mismatch_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 0.75)
det_est_data_mismatch_nitrogen <- subset(det_est_data_mismatch_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 0.75)
final_results_mismatch_nitrogen <- final_results_mismatch_nitrogen[,1:11]
det_est_data_mismatch_nitrogen <- det_est_data_mismatch_nitrogen[,1:11]

final_results_others_nitrogen <- readRDS(here("Results", "deterministic_clean_data_others_nitrogen.rds"))
det_est_data_others_nitrogen <- readRDS(here("Results", "deterministic_clean_est_others_nitrogen.rds"))

# changing yield response types
high_data <- ggplot(data = subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_high) +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "high_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  xlab("Input Rate") +
  ylab("Yield") +
  xlim(60, 260) +
  theme(legend.position="none") +
  ylim(230, 252)

mid_data <- ggplot(data = subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "middle_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_mid) +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "middle_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  xlab("Input Rate") +
  ylab("Yield") +
  xlim(60, 260) +
  theme(legend.position="none") +
  ylim(230, 252)

low_data <- ggplot(data = subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "low_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate)) +
  geom_count() +
  geom_function(fun = f_low) +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "low_response" & alignment_case == "mis-alignment" & error_degree == 30 & rate_types == "rates_center"), aes(y = yield, x = rate), color = "Red") +
  xlab("Input Rate") +
  ylab("Yield") +
  xlim(60, 260) +
  ylim(230, 252) +
  guides(size = guide_legend(title = "Count"))

factor_response <- (high_data | mid_data | low_data)
factor_response_vertical <- (high_data / mid_data / low_data)
saveRDS(factor_response, here("Results/factor_response.rds"))

# change misalignment type
# need to add misalignment types together 
results_others <- subset(final_results_others_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 30)
results_others <- results_others %>%
  dplyr::select(colnames(final_results_mismatch_nitrogen))
data_others <- subset(det_est_data_others_nitrogen, max_dev == 100 & rate_types == "rates_center" & ys_type == "high_response" & c_price == 5.62 & error_degree == 30)

data_all <- rbind(data_others, det_est_data_mismatch_nitrogen)
results_all <- rbind(results_others, final_results_mismatch_nitrogen)

data_all$alignment_case <- as.factor(data_all$alignment_case)
levels(data_all$alignment_case) <- c("Heading Difference", "Parallel Shift", "Incompatible Machinery")

results_all$alignment_case <- as.factor(results_all$alignment_case)
levels(results_all$alignment_case) <- c("Heading Difference", "Parallel Shift", "Incompatible Machinery")

factor_type <- ggplot(results_all, aes(rate, yield)) +
  geom_count() +
  geom_line(data = data_all, aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(alignment_case)) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

saveRDS(factor_type, here("Results/factor_type.rds"))

# just change rates
# use mis-alignment 30-foot high-response and change rates
# final_results_others_nitrogen$rate_types <- as.factor(final_results_others_nitrogen$alignment_case)
levels(final_results_others_nitrogen$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")
levels(det_est_data_others_nitrogen$rate_types) <- c("Low Rates", "Centered Rates", "High Rates")

factor_rates <- ggplot(subset(final_results_others_nitrogen, max_dev == 100 & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment" & error_degree == 30), aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(rate_types)) +
  # ylim(230, 250) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

saveRDS(factor_rates, here("Results/factor_rates.rds"))

# change error levels
final_results_others_nitrogen$error_degree <- as.factor(final_results_others_nitrogen$error_degree)
det_est_data_others_nitrogen$error_degree <- as.factor(det_est_data_others_nitrogen$error_degree)

levels(final_results_others_nitrogen$error_degree) <- c("0-foot shift", "10-foot shift", "30-foot shift")
levels(det_est_data_others_nitrogen$error_degree) <- c("0-foot shift", "10-foot shift", "30-foot shift")

factor_error <- ggplot(subset(final_results_others_nitrogen, max_dev == 100 & rate_types == "Centered Rates" & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others_nitrogen, max_dev == 100 & rate_types == "Centered Rates" & ys_type == "high_response" & c_price == 5.62 & alignment_case == "mis-alignment"), aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(error_degree)) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

saveRDS(factor_error, here("Results/factor_error.rds"))
# change max_dev 

factor_maxdev <- ggplot(subset(final_results_others_nitrogen, error_degree == "30-foot" & rate_types == "Centered Rates" & ys_type == "high_response"  & alignment_case == "mis-alignment"), aes(rate, yield)) +
  geom_count() +
  geom_line(data = subset(det_est_data_others_nitrogen, error_degree == "30-foot" & rate_types == "Centered Rates" & ys_type == "high_response" & alignment_case == "mis-alignment"), aes(rate, yield), color = "Red") +
  geom_function(fun = f_high) +
  facet_grid(cols = vars(max_dev)) +
  xlab("Input Rate") +
  ylab("Yield") +
  guides(size = guide_legend(title = "Count"))

saveRDS(factor_maxdev, here("Results/factor_maxdev.rds"))


#### Trial design example ####
trial_grids <- readRDS("Data/Wendte_LaueLib80_2020/Raw/trial-grids-mismatch.rds")
trial_grids <- dplyr::rename(trial_grids, "td_grid_id" = "td_grd_", "strip_id" = "strip_d")
harvester_polygons <- readRDS("Data/Wendte_LaueLib80_2020/Raw/harvester-polygons-mismatch.rds")

rates_ls <- c(41, 69, 97, 125, 153, 181)

exp_design <- assign_rates(
  filter(trial_grids, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

trial_design <- merge(trial_grids, exp_design, by = "td_grid_id")

trial_design$rate <- as.factor(trial_design$rate)

trial_design_map <- ggplot(trial_design) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  guides(fill = guide_legend(title = "Target Rate")) +
  scale_fill_brewer(palette="Blues")
saveRDS(trial_design_map, file = here("Results", "trial_design_map.rds"))