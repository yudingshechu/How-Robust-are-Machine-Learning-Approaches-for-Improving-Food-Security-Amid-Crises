library(haven)
library(tidyr)
library('ggplot2')
library(dplyr)
library(terra)
library(stringr)
library(tidyverse)
library(ggpubr)
library(ggridges)
library(viridis)
library(reshape2)
library(PRROC)
library(Metrics)
library(plotROC)
library(ggrepel)
###################################################################################
###################################################################################
AUC <- readxl::read_xlsx('bld/AUCs.xlsx')
districtAUC <- AUC[, grepl("district|mode", colnames(AUC))]
countyAUC <- AUC[, grepl("_county|mode", colnames(AUC))]
subcountyAUC <- AUC[, grepl("subcounty|mode", colnames(AUC))]

districtAUC <- melt(districtAUC, id.vars = 'model')
countyAUC <- melt(countyAUC, id.vars = 'model')
subcountyAUC <- melt(subcountyAUC, id.vars = 'model')
r4_colors <- c("LR during" = "#DCE319FF", "LR before" = "#FDE725FF", 
               "RF during" = "#1F968BFF", "RF before" = "#55C667FF", 
               "XGB during" = "#440154FF", "XGB before" = "#404788FF" )
districtAUC['Resample'] <- rep(c('balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 'SMOTE Tomek', 'origin'), each = 6)
districtAUC$model <- gsub('_', ' ', districtAUC$model)
# Add a column for the model prefix
df <- districtAUC %>%
  mutate(model_prefix = sub(" .*", "", model))

# Filter and pair "before" and "during" models, then calculate the difference
result <- df %>%
  filter(grepl("before|during", model)) %>%
  group_by(model_prefix, variable, Resample) %>%
  summarise(
    before_value = value[model == paste0(model_prefix, " before")],
    during_value = value[model == paste0(model_prefix, " during")],
    difference = during_value - before_value,
    .groups = "drop"
  )

# Display the result
result <- as.data.frame(result)
result$difference <- round(result$difference, 3)
result <- result[, c(1, 3, 4, 5, 6)]
colnames(result) <- c('Model', 'Resample method', 'AUC before', 'AUC during', 'change')

result['Method'] <- paste(result$Model, result$`Resample method`, sep = ' ')
r4_colors2 <- c("LR" = "#7D0112",
               "RF" = "#1F968BFF", 
               "XGB" = "#440154FF" )
result_district <- result
result$change <- abs(result$change)
result_district_abs <- result
library(ggrepel)
result['percent'] <- 100*result$change/result$`AUC before`
result['inverse_change'] <- 1/result$change
result['inverse_perc'] <-  1 / result$percent
p1 <- ggplot(result, aes(x = change, y = `AUC during`, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "robustness: absolute change in AUC",
    y = "performance: AUC during COVID"
  ) 
p1
ggsave('bld/figures/AUC_change_district.jpg', p1, width = 8,
       height = 5, dpi = 800)
p1 <- ggplot(result, aes(x = percent, y = `AUC during`, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "absolute difference in AUC (%)",
    y = "AUC during COVID"
  ) + scale_x_reverse()
p1
ggsave('bld/figures/AUC_change_district_percent.jpg', p1, width = 8,
       height = 5, dpi = 800)

# county
countyAUC['Resample'] <- rep(c('balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 'SMOTE Tomek', 'origin'), each = 6)
countyAUC$model <- gsub('_', ' ', countyAUC$model)
# Add a column for the model prefix
df <- countyAUC %>%
  mutate(model_prefix = sub(" .*", "", model))

# Filter and pair "before" and "during" models, then calculate the difference
result <- df %>%
  filter(grepl("before|during", model)) %>%
  group_by(model_prefix, variable, Resample) %>%
  summarise(
    before_value = value[model == paste0(model_prefix, " before")],
    during_value = value[model == paste0(model_prefix, " during")],
    difference = during_value - before_value,
    .groups = "drop"
  )

# Display the result
result <- as.data.frame(result)
result$difference <- round(result$difference, 3)
result <- result[, c(1, 3, 4, 5, 6)]
colnames(result) <- c('Model', 'Resample method', 'AUC before', 'AUC during', 'change')
result$`AUC before` <- round(result$`AUC before`, 3)
result$`AUC during` <- round(result$`AUC during`, 3)
# write.csv(result, 'bld/countyAUC.csv')
result['Method'] <- paste(result$Model, result$`Resample method`, sep = ' ')
result_county <- result
result$change <- abs(result$change)
result_county_abs <- result
library(ggrepel)
result['percent'] <- 100*result$change/result$`AUC before`
p1 <- ggplot(result, aes(x = percent, y = `AUC during`, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "absolute difference in AUC (%)",
    y = "AUC during COVID"
  ) + scale_x_reverse()
p1
ggsave('bld/figures/AUC_change_county_percent.jpg', p1, width = 8,
       height = 4, dpi = 800)
p2 <- ggplot(result, aes(x = change, y = `AUC during`, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "robustness: absolute change in AUC",
    y = "performance: during COVID AUC"
  ) 
p2
ggsave('bld/figures/AUC_change_county.jpg', p2, width = 8,
       height = 4, dpi = 800)

# subcounty
subcountyAUC['Resample'] <- rep(c('balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 'SMOTE Tomek', 'origin'), each = 6)
subcountyAUC$model <- gsub('_', ' ', subcountyAUC$model)
# Add a column for the model prefix
df <- subcountyAUC %>%
  mutate(model_prefix = sub(" .*", "", model))

# Filter and pair "before" and "during" models, then calculate the difference
result <- df %>%
  filter(grepl("before|during", model)) %>%
  group_by(model_prefix, variable, Resample) %>%
  summarise(
    before_value = value[model == paste0(model_prefix, " before")],
    during_value = value[model == paste0(model_prefix, " during")],
    difference = during_value - before_value,
    .groups = "drop"
  )

# Display the result
result <- as.data.frame(result)
result$difference <- round(result$difference, 3)
result <- result[, c(1, 3, 4, 5, 6)]
colnames(result) <- c('Model', 'Resample method', 'AUC before', 'AUC during', 'change')
result$`AUC before` <- round(result$`AUC before`, 3)
result$`AUC during` <- round(result$`AUC during`, 3)
# write.csv(result, 'bld/subcountyAUC.csv')
result['Method'] <- paste(result$Model, result$`Resample method`, sep = ' ')
result_subcounty <- result
result$change <- abs(result$change)
result_subcounty_abs <- result
result['percent'] <- 100*result$change/result$`AUC before`
p1 <- ggplot(result, aes(x = percent, y = `AUC during`, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "absolute difference in AUC (%)",
    y = "AUC during COVID"
  ) + scale_x_reverse()
p1
ggsave('bld/figures/AUC_change_subcounty_percent.jpg', p1, width = 8,
       height = 4, dpi = 800)
p3 <- ggplot(result, aes(x = change, y = `AUC during`, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "robustness: absolute change in AUC",
    y = "perfromance: during COVID AUC"
  ) + scale_x_reverse()
p3
ggsave('bld/figures/AUC_change_subcounty.jpg', p3, width = 8,
       height = 4, dpi = 800)

result_district['Region'] <- 'district'
result_district_abs['Region'] <- 'district'
result_county['Region'] <- 'county'
result_county_abs['Region'] <- 'county'
result_subcounty['Region'] <- 'subcounty'
result_subcounty_abs['Region'] <- 'subcounty'

together_result <- rbind(result_district, result_county, result_subcounty)
# together_result$MR <- paste(together_result$Model, together_result$region, sep = ' ')
together_result_abs <- rbind(result_district_abs, result_county_abs, result_subcounty_abs)
r4_colors3 <- c("LR district" = "#7D0112", 'LR county' = '#A24E27', 'LR subcounty' = '#DCB371', 
                "RF district" = "#315D00", 'RF county' = '#6CAE25', 'RF subcounty' = '#97DDC1', 
                "XGB district" = "#440154FF", 'XGB county' =  '#B4219C', 'XGB subcounty' = '#F382AB')
r4_colors2 <- c("district" = "#DF536B",
                "county" = "#2297E6", 
                "subcounty" = "#61D04F" )
together_result['percent'] <- 100*together_result$change/together_result$`AUC before`
p0 <- ggplot(together_result, aes(x = percent, y = `AUC during`, color = Region, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "difference in AUC (%)",
    y = "AUC during COVID"
  ) + 
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'red')
p0
ggsave('bld/figures/AUC_change_all_precentage.jpg', p0, width = 8,
       height = 5, dpi = 800)
p0 <- ggplot(together_result, aes(x = change, y = `AUC during`, color = Region, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "difference in AUC",
    y = "AUC during COVID"
  ) + 
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'red')
p0
ggsave('bld/figures/AUC_change_all.jpg', p0, width = 8,
       height = 5, dpi = 800)
p0_abs <- ggplot(together_result_abs, aes(x = change, y = `AUC during`, color = Region, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "robustness: absolute change in AUC",
    y = "performance: AUC during COVID"
  ) 
p0_abs
ggsave('bld/figures/AUC_change_all_abs.jpg', p0_abs, width = 8,
       height = 5, dpi = 800)

together_result['percent_abs'] <- abs(together_result['percent'])
p0_abs <- ggplot(together_result, aes(x = percent_abs, y = `AUC during`, color = Region, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "absolute difference in AUC (%)",
    y = "AUC during COVID"
  ) + scale_x_reverse()
p0_abs
ggsave('bld/figures/AUC_change_all_abs_percent.jpg', p0_abs, width = 8,
       height = 5, dpi = 800)
