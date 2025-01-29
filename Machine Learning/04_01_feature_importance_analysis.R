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
library(grafify)

importance <- read_csv('bld/feature_importance_district.csv')

importance_abs <- importance
importance_abs$LR_district <- abs(importance_abs$LR_district)
importance_abs$LR_ada_district <- abs(importance_abs$LR_ada_district)
importance_abs$LR_district_noW <- abs(importance_abs$LR_district_noW)
importance_abs$LR_smote_district <- abs(importance_abs$LR_smote_district)
importance_abs$LR_smoteenn_district <- abs(importance_abs$LR_smoteenn_district)
importance_abs$LR_smotetom_district <- abs(importance_abs$LR_smotetom_district)

var_previous <- c('FCSStaus_lag', 'FoodInsecureMonthly_lag')
var_macro <- c('NL_District_log', 'precipitationMean', 'NDVI.Anomaly.Mean', 
               'Average..mm.', "X1.Month.Anomaly...." , "X3.Months.Anomaly...." ,
               "fatalitiesMean_log" , "temperatureMean_log" , "NDVIMean_log")
var_HHinfo <- c('urban', "MaleRatio" , 'AvgAge', "SelfArg" ,
                "SelfHerd" , 
                "FamilySize" , "SelfStapleTypes"    )
var_HHfin <- c("Kind.Income.Ratio" ,  "ShareToilet_Yes" , "Salt_Yes"  ,'WaterSource_feq', 
               "HouseType_feq" ,"RoofType_feq" ,   "IncomeStab_Somewhat stable" , 
               "IncomeStab_Very unstable"  , "SubjectivePoverty_Neither poor nor rich",
               "SubjectivePoverty_Poor" ,  "SubjectivePoverty_Very poor" , "DistDrinkingWaterBig3", 
               "RelLivStandard_Better off",   "RelLivStandard_Same" , "RelLivStandard_Worse off", 
               "LivStandChange_Decreased", "LivStandChange_Increased", 
               "LivStandChange_Stayed at the same" )
var_HHasset <- c("valueNow_MobilePhone"   , "valueNowTotal" ,                         
                 "valueNow_Furniture"  ,  "valueNow_MobilePhone_new"   ,            
                 "valueNow_ArgLand_new" ,  "valueNow_ArgLand"     ,                  
                 "valueNow_Livestock" , "valueNow_Furniture_new"  ,               
                 "valueNow_Livestock_new"  , "valueNowTotal_new",                     
                "Income_new" , "Income"   ,                              
                 "ValueAgoTotal_new"  ,   "valueNow_FixPhone" ,                     
                "valueNow_Refrigerator" )

importance_abs[importance_abs$feature_name %in% var_HHasset, ]

colSums(importance_abs[importance_abs$feature_name %in% var_HHasset, 2:19])
df_feature_importance <- data.frame(model = colnames(importance_abs)[2:19], 
                                    # Previous = NA, 
                                    Macro = NA, HHinfo = NA, 
                                    HHfinancial = NA, 
                                    HHassets = NA)
# df_feature_importance$Previous <- colSums(importance_abs[importance_abs$feature_name %in% var_previous, 2:19])
df_feature_importance$Macro <- colSums(importance_abs[importance_abs$feature_name %in% c(var_macro, var_previous), 2:19])
df_feature_importance$HHinfo <- colSums(importance_abs[importance_abs$feature_name %in% var_HHinfo, 2:19])
df_feature_importance$HHfinancial <- colSums(importance_abs[importance_abs$feature_name %in% var_HHfin, 2:19])
df_feature_importance$HHassets <- colSums(importance_abs[importance_abs$feature_name %in% var_HHasset, 2:19])

###############################################
importance_abs['vartype'] <- NA
importance_abs[importance_abs$feature_name %in% c(var_macro, var_previous), 20] <- 'Environmental'
importance_abs[importance_abs$feature_name %in% var_HHinfo, 20] <- 'Demographical'
importance_abs[importance_abs$feature_name %in% var_HHfin, 20] <- 'Living Condition'
importance_abs[importance_abs$feature_name %in% var_HHasset, 20] <- 'Income and Wealth'
write.csv(importance_abs, 'bld/importance_abs.csv')
###########################################
df_feature_importance['Sum'] <- rowSums(df_feature_importance[, 2:5])

# df_feature_importance['percent_Previous'] <- df_feature_importance$Previous/df_feature_importance$Sum * 100
df_feature_importance['percent_Macro'] <- df_feature_importance$Macro/df_feature_importance$Sum * 100
df_feature_importance['percent_HHinfo'] <- df_feature_importance$HHinfo/df_feature_importance$Sum * 100
df_feature_importance['percent_HHfinancial'] <- df_feature_importance$HHfinancial/df_feature_importance$Sum * 100
df_feature_importance['percent_HHassets'] <- df_feature_importance$HHassets/df_feature_importance$Sum * 100

df_feature_importance$alg <- sapply(strsplit(df_feature_importance$model, "_"), `[`, 1)

df_feature_importance$resample <- rep(c('balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 
                                       'SMOTE Tomek', 'origin'), 3)
df_feature_importance['minority'] <- rep(c('balanced weight', 'resampling', 'resampling', 
                                           'resampling', 'resampling', 'origin'), 3)
df_feature_importance['min_alg'] <- paste(df_feature_importance$alg, df_feature_importance$minority, sep = ' ')

result <- df_feature_importance %>%
  group_by(min_alg) %>%
  summarise(
    # Previous = mean(percent_Previous), 
    Macro = mean(percent_Macro), 
    Demographical = mean(percent_HHinfo), 
    `Living Condition` = mean(percent_HHfinancial), 
    `Income and Wealth` = mean(percent_HHassets), 
    .groups = "drop"
  )
result['alg'] <- rep(c('LR', 'RF', 'XGB'), each = 3)

result_plot <- melt(result)

r4_colors2 <- c("LR" = "#7D0112",
                "RF" = "#1F968BFF", 
                "XGB" = "#440154FF" )

result_plot <- result_plot %>%
  group_by(min_alg) %>%
  mutate(proportion = value / sum(value))
# Plot with proportional values displayed using geom_label
p1 <- ggplot(result_plot, aes(fill = variable, y = proportion, x = min_alg)) + 
  geom_bar(position = "fill", stat = "identity") + 
  geom_label(aes(label = scales::percent(proportion, accuracy = 0.1)), 
             position = position_fill(vjust = 0.5), 
             size = 3, label.padding = unit(0.2, "lines"), show.legend = FALSE) +
  scale_fill_grafify(palette = "r4")+
  labs(y = "Feature importance proportion", x = "Model", fill = "Variable type") +
  theme_minimal()
p1
ggsave('bld/figures/FeatureImportance_during_proportion.jpg', p1, width = 12,
       height = 6, dpi = 800)



# Rank each column (case) independently
ranked_df <- as.data.frame(apply(importance_abs[ , -1], 2, rank))

# Add the 'names' column back to the ranked dataframe
ranked_df$names <- importance_abs$feature_name

# Rearrange columns to have 'names' as the first column
ranked_df <- ranked_df[, c(ncol(ranked_df), 1:(ncol(ranked_df) - 1))]

#####################################################################################
importance <- read_csv('bld/feature_importance_district_shap_testingdata.csv')

importance_abs <- importance

df_feature_importance <- data.frame(model = colnames(importance_abs)[2:19], 
                                    # Previous = NA, 
                                    Macro = NA, HHinfo = NA, 
                                    HHfinancial = NA, 
                                    HHassets = NA)
# df_feature_importance$Previous <- colSums(importance_abs[importance_abs$feature_name %in% var_previous, 2:19])
df_feature_importance$Macro <- colSums(importance_abs[importance_abs$Feature %in% c(var_macro, var_previous), 2:19])
df_feature_importance$HHinfo <- colSums(importance_abs[importance_abs$Feature %in% var_HHinfo, 2:19])
df_feature_importance$HHfinancial <- colSums(importance_abs[importance_abs$Feature %in% var_HHfin, 2:19])
df_feature_importance$HHassets <- colSums(importance_abs[importance_abs$Feature %in% var_HHasset, 2:19])

df_feature_importance['Sum'] <- rowSums(df_feature_importance[, 2:5])

# df_feature_importance['percent_Previous'] <- df_feature_importance$Previous/df_feature_importance$Sum * 100
df_feature_importance['percent_Macro'] <- df_feature_importance$Macro/df_feature_importance$Sum * 100
df_feature_importance['percent_HHinfo'] <- df_feature_importance$HHinfo/df_feature_importance$Sum * 100
df_feature_importance['percent_HHfinancial'] <- df_feature_importance$HHfinancial/df_feature_importance$Sum * 100
df_feature_importance['percent_HHassets'] <- df_feature_importance$HHassets/df_feature_importance$Sum * 100

df_feature_importance$alg <- rep(c("XGB", "RF", 'LR'), each = 6)

df_feature_importance$resample <- rep(c('origin', 'balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 
                                        'SMOTE Tomek'), 3)
df_feature_importance['minority'] <- rep(c( 'origin', 'balanced weight', 'resampling', 'resampling', 
                                            'resampling', 'resampling'), 3)
df_feature_importance['min_alg'] <- paste(df_feature_importance$alg, df_feature_importance$minority, sep = ' ')

result <- df_feature_importance %>%
  group_by(min_alg) %>%
  summarise(
    # Previous = mean(percent_Previous), 
    Macro = mean(percent_Macro), 
    Demographical = mean(percent_HHinfo), 
    `Living Condition` = mean(percent_HHfinancial), 
    `Income and Wealth` = mean(percent_HHassets), 
    .groups = "drop"
  )
result['alg'] <- rep(c('LR', 'RF', 'XGB'), each = 3)

result_plot <- melt(result)

r4_colors2 <- c("LR" = "#7D0112",
                "RF" = "#1F968BFF", 
                "XGB" = "#440154FF" )

result_plot <- result_plot %>%
  group_by(min_alg) %>%
  mutate(proportion = value / sum(value))
# Plot with proportional values displayed using geom_label
p1 <- ggplot(result_plot, aes(fill = variable, y = proportion, x = min_alg)) + 
  geom_bar(position = "fill", stat = "identity") + 
  geom_label(aes(label = scales::percent(proportion, accuracy = 0.1)), 
             position = position_fill(vjust = 0.5), 
             size = 3, label.padding = unit(0.2, "lines"), show.legend = FALSE) +
  scale_fill_grafify(palette = "r4")+
  labs(y = "SHAP proportion", x = "Model", fill = "Variable type") +
  theme_minimal()
p1
ggsave('bld/figures/SHAP_during_proportion_test.jpg', p1, width = 12,
       height = 6, dpi = 800)

########################################################################################
importance <- read_csv('bld/feature_importance_district_before_shap_testingdata.csv')

importance_abs <- importance

var_previous <- c('FCSStaus_lag', 'FoodInsecureMonthly_lag')
var_macro <- c('NL_District_log', 'precipitationMean', 'NDVI.Anomaly.Mean', 
               'Average..mm.', "X1.Month.Anomaly...." , "X3.Months.Anomaly...." ,
               "fatalitiesMean_log" , "temperatureMean_log" , "NDVIMean_log")
var_HHinfo <- c('urban', "MaleRatio" , 'AvgAge', "SelfArg" ,
                "SelfHerd" , 
                "FamilySize" , "SelfStapleTypes"    )
var_HHfin <- c("Kind.Income.Ratio" ,  "ShareToilet_Yes" , "Salt_Yes"  ,'WaterSource_feq', 
               "HouseType_feq" ,"RoofType_feq" ,   "IncomeStab_Somewhat stable" , 
               "IncomeStab_Very unstable"  , "SubjectivePoverty_Neither poor nor rich",
               "SubjectivePoverty_Poor" ,  "SubjectivePoverty_Very poor" , "DistDrinkingWaterBig3", 
               "RelLivStandard_Better off",   "RelLivStandard_Same" , "RelLivStandard_Worse off", 
               "LivStandChange_Decreased", "LivStandChange_Increased", 
               "LivStandChange_Stayed at the same" )
var_HHasset <- c("valueNow_MobilePhone"   , "valueNowTotal" ,                         
                 "valueNow_Furniture"  ,  "valueNow_MobilePhone_new"   ,            
                 "valueNow_ArgLand_new" ,  "valueNow_ArgLand"     ,                  
                 "valueNow_Livestock" , "valueNow_Furniture_new"  ,               
                 "valueNow_Livestock_new"  , "valueNowTotal_new",                     
                 "Income_new" , "Income"   ,                              
                 "ValueAgoTotal_new"  ,   "valueNow_FixPhone" ,                     
                 "valueNow_Refrigerator" )

colSums(importance_abs[importance_abs$Feature %in% var_HHasset, 2:13])
df_feature_importance <- data.frame(model = colnames(importance_abs)[2:13], 
                                    # Previous = NA, 
                                    Macro = NA, HHinfo = NA, 
                                    HHfinancial = NA, 
                                    HHassets = NA)
# df_feature_importance$Previous <- colSums(importance_abs[importance_abs$feature_name %in% var_previous, 2:19])
df_feature_importance$Macro <- colSums(importance_abs[importance_abs$feature_name %in% c(var_macro, var_previous), 2:19])
df_feature_importance$HHinfo <- colSums(importance_abs[importance_abs$feature_name %in% var_HHinfo, 2:19])
df_feature_importance$HHfinancial <- colSums(importance_abs[importance_abs$feature_name %in% var_HHfin, 2:19])
df_feature_importance$HHassets <- colSums(importance_abs[importance_abs$feature_name %in% var_HHasset, 2:19])

###############################################
importance_abs['vartype'] <- NA
importance_abs[importance_abs$feature_name %in% c(var_macro, var_previous), 20] <- 'Environmental'
importance_abs[importance_abs$feature_name %in% var_HHinfo, 20] <- 'Demographical'
importance_abs[importance_abs$feature_name %in% var_HHfin, 20] <- 'Living Condition'
importance_abs[importance_abs$feature_name %in% var_HHasset, 20] <- 'Income and Wealth'
write.csv(importance_abs, 'bld/importance_abs.csv')
###########################################
df_feature_importance['Sum'] <- rowSums(df_feature_importance[, 2:5])

# df_feature_importance['percent_Previous'] <- df_feature_importance$Previous/df_feature_importance$Sum * 100
df_feature_importance['percent_Macro'] <- df_feature_importance$Macro/df_feature_importance$Sum * 100
df_feature_importance['percent_HHinfo'] <- df_feature_importance$HHinfo/df_feature_importance$Sum * 100
df_feature_importance['percent_HHfinancial'] <- df_feature_importance$HHfinancial/df_feature_importance$Sum * 100
df_feature_importance['percent_HHassets'] <- df_feature_importance$HHassets/df_feature_importance$Sum * 100

df_feature_importance$alg <- sapply(strsplit(df_feature_importance$model, "_"), `[`, 1)

df_feature_importance$resample <- rep(c('balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 
                                        'SMOTE Tomek', 'origin'), 3)
df_feature_importance['minority'] <- rep(c('balanced weight', 'resampling', 'resampling', 
                                           'resampling', 'resampling', 'origin'), 3)
df_feature_importance['min_alg'] <- paste(df_feature_importance$alg, df_feature_importance$minority, sep = ' ')

result <- df_feature_importance %>%
  group_by(min_alg) %>%
  summarise(
    # Previous = mean(percent_Previous), 
    Environmental = mean(percent_Macro), 
    Demographical = mean(percent_HHinfo), 
    `Living Condition` = mean(percent_HHfinancial), 
    `Income and Wealth` = mean(percent_HHassets), 
    .groups = "drop"
  )
result['alg'] <- rep(c('LR', 'RF', 'XGB'), each = 3)

result_plot <- melt(result)

result_plot <- result_plot %>%
  group_by(min_alg) %>%
  mutate(proportion = value / sum(value))
# Plot with proportional values displayed using geom_label
p1 <- ggplot(result_plot, aes(fill = variable, y = proportion, x = min_alg)) + 
  geom_bar(position = "fill", stat = "identity") + 
  geom_label(aes(label = scales::percent(proportion, accuracy = 0.1)), 
             position = position_fill(vjust = 0.5), 
             size = 3, label.padding = unit(0.2, "lines"), show.legend = FALSE) +
  scale_fill_grafify(palette = "r4")+
  labs(y = "Feature importance proportion", x = "Model", fill = "Variable type") +
  theme_minimal()
p1
ggsave('bld/figures/FeatureImportance_during_proportion.jpg', p1, width = 10,
       height = 6, dpi = 800)

# Rank each column (case) independently
ranked_df <- as.data.frame(apply(importance_abs[ , -1], 2, rank))

# Add the 'names' column back to the ranked dataframe
ranked_df$names <- importance_abs$feature_name

# Rearrange columns to have 'names' as the first column
ranked_df <- ranked_df[, c(ncol(ranked_df), 1:(ncol(ranked_df) - 1))]


#####################################################################################
importance <- read_csv('bld/feature_importance_district_shap.csv')

importance_abs <- importance

importance_abs[importance_abs$Feature %in% var_HHasset, ]

colSums(importance_abs[importance_abs$Feature %in% var_HHasset, 2:19])
df_feature_importance <- data.frame(model = colnames(importance_abs)[2:19], 
                                    # Previous = NA, 
                                    Macro = NA, HHinfo = NA, 
                                    HHfinancial = NA, 
                                    HHassets = NA)
# df_feature_importance$Previous <- colSums(importance_abs[importance_abs$feature_name %in% var_previous, 2:19])
df_feature_importance$Macro <- colSums(importance_abs[importance_abs$Feature %in% c(var_macro, var_previous), 2:19])
df_feature_importance$HHinfo <- colSums(importance_abs[importance_abs$Feature %in% var_HHinfo, 2:19])
df_feature_importance$HHfinancial <- colSums(importance_abs[importance_abs$Feature %in% var_HHfin, 2:19])
df_feature_importance$HHassets <- colSums(importance_abs[importance_abs$Feature %in% var_HHasset, 2:19])

df_feature_importance['Sum'] <- rowSums(df_feature_importance[, 2:5])

# df_feature_importance['percent_Previous'] <- df_feature_importance$Previous/df_feature_importance$Sum * 100
df_feature_importance['percent_Macro'] <- df_feature_importance$Macro/df_feature_importance$Sum * 100
df_feature_importance['percent_HHinfo'] <- df_feature_importance$HHinfo/df_feature_importance$Sum * 100
df_feature_importance['percent_HHfinancial'] <- df_feature_importance$HHfinancial/df_feature_importance$Sum * 100
df_feature_importance['percent_HHassets'] <- df_feature_importance$HHassets/df_feature_importance$Sum * 100

df_feature_importance$alg <- rep(c("XGB", "RF", 'LR'), each = 6)

df_feature_importance$resample <- rep(c('origin', 'balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 
                                        'SMOTE Tomek'), 3)
df_feature_importance['minority'] <- rep(c( 'origin', 'balanced weight', 'resampling', 'resampling', 
                                            'resampling', 'resampling'), 3)
df_feature_importance['min_alg'] <- paste(df_feature_importance$alg, df_feature_importance$minority, sep = ' ')

result <- df_feature_importance %>%
  group_by(min_alg) %>%
  summarise(
    # Previous = mean(percent_Previous), 
    Environmental = mean(percent_Macro), 
    Demographical = mean(percent_HHinfo), 
    `Living Condition` = mean(percent_HHfinancial), 
    `Income and Wealth` = mean(percent_HHassets), 
    .groups = "drop"
  )
result['alg'] <- rep(c('LR', 'RF', 'XGB'), each = 3)

result_plot <- melt(result)

result_plot <- result_plot %>%
  group_by(min_alg) %>%
  mutate(proportion = value / sum(value))
# Plot with proportional values displayed using geom_label
p1 <- ggplot(result_plot, aes(fill = variable, y = proportion, x = min_alg)) + 
  geom_bar(position = "fill", stat = "identity") + 
  geom_label(aes(label = scales::percent(proportion, accuracy = 0.1)), 
             position = position_fill(vjust = 0.5), 
             size = 3, label.padding = unit(0.2, "lines"), show.legend = FALSE) +
  scale_fill_grafify(palette = "r4")+
  labs(y = "SHAP proportion", x = "Model", fill = "Variable type") +
  theme_minimal()
p1
ggsave('bld/figures/SHAP_during_proportion.jpg', p1, width = 10,
       height = 6, dpi = 800)

# Rank each column (case) independently
ranked_df <- as.data.frame(apply(importance_abs[ , -1], 2, rank))

# Add the 'names' column back to the ranked dataframe
ranked_df$names <- importance_abs$Feature

# Rearrange columns to have 'names' as the first column
ranked_df <- ranked_df[, c(ncol(ranked_df), 1:(ncol(ranked_df) - 1))]

ranked_df_2 <- ranked_df[, c(1, 2, 3, 8, 9)]
ranked_df_2['shap_importance_XGB_smote'] <- rowMeans(ranked_df[, c(4:7)])
ranked_df_2['shap_importance_RF_smote'] <- rowMeans(ranked_df[, c(10:13)])

# Calculate the mean for each row across selected columns
df_means <- ranked_df %>%
  rowwise() %>%
  mutate(mean_value = mean(c_across(starts_with("_district")), na.rm = TRUE)) %>%
  ungroup()

#############################################################################
#############################################################################
#############################################################################
##### bld/AUC_district.csv needed to be created manually, looks like: 
# model	AUC_district	AUC_district_ada	AUC_district_smote	AUC_district_smoteenn	AUC_district_smotetom	AUC_district_noW
# LR_during	0.781031031	0.71021021	0.708708709	0.731481481	0.707957958	0.773523524
# RF_during	0.805805806	0.794044044	0.783033033	0.772772773	0.789039039	0.824574575
# XGB_during	0.8003003	0.797547548	0.791541542	0.791041041	0.794044044	0.839089089
# LR_before	0.781456954	0.79580574	0.78410596	0.761810155	0.78410596	0.781567329
# RF_before	0.753090508	0.794481236	0.787417219	0.786092715	0.787417219	0.753090508
# XGB_before	0.815452539	0.804966887	0.782891832	0.781898455	0.792273731	0.766887417
# LR_select	0.704204204	0.728228228	0.715965966	0.716716717	0.715965966	0.696696697
# RF_select	0.776776777	0.755755756	0.751751752	0.757757758	0.751751752	0.79954955
# XGB_select	0.771521522	0.722222222	0.682682683	0.749249249	0.671921922	0.786536537

AUC <- read_csv('bld/AUC_district.csv')

districtAUC <- melt(AUC, id.vars = 'model')

districtAUC['Resample'] <- rep(c('balanced weight', 'ADASYN', 'SMOTE', 'SMOTE ENN', 'SMOTE Tomek', 'origin'), each = 9)
districtAUC$model <- gsub('_', ' ', districtAUC$model)

# Add a column for the model prefix
df <- districtAUC %>%
  mutate(model_prefix = sub(" .*", "", model))

df['type'] <- rep(c('during', 'during', 'during', 
                    'before', 'before', 'before', 
                    'select', 'select', 'select'), 6)
# df['alg'] <- rep(c('LR', 'RF', 'XGB'), 18)
result <- df %>%
  filter(grepl("before|during|select", model)) %>%
  group_by(model_prefix, variable, Resample) %>%
  summarise(
    before_value = value[model == paste0(model_prefix, " before")],
    during_value = value[model == paste0(model_prefix, " during")],
    select_value = value[model == paste0(model_prefix, " select")], 
    difference = during_value - before_value,
    diff_select_before = select_value - before_value, 
    diff_select_during = select_value - during_value, 
    .groups = "drop"
  )

# Display the result
result <- as.data.frame(result)
result$difference <- round(result$difference, 3)
result$diff_select_before <- round(result$diff_select_before, 3)
result$diff_select_during <- round(result$diff_select_during, 3)

result['Method'] <- paste(result$model_prefix, result$Resample, sep = ' ')
r4_colors2 <- c("LR" = "#7D0112",
                "RF" = "#1F968BFF", 
                "XGB" = "#440154FF" )
result_district <- result
result$difference_abs <- abs(result$difference)
result$diff_select_before_abs <- abs(result$diff_select_before)
result$diff_select_during_abs <- abs(result$diff_select_during)
result_district_abs <- result

result['percent_change'] <- result$diff_select_during/result$during_value*100

result$during_value <- round(result$during_value, 2)
result$select_value <- round(result$select_value, 2)
result$percent_change <- round(result$percent_change, 2)
write.csv(result[,c(1, 3, 5, 6, 14)], 'bld/try.csv')

library(ggrepel)
colnames(result)[1] <- 'Model'
result_plot <- data.frame(Model = result$Model, Method = result$Method)
result_plot <- rbind(result_plot, result_plot, result_plot)
result_plot['Type'] <- rep(c('before', 'during', 'select'), each = 18)
result_plot['AUC'] <- c(result$before_value, result$during_value, result$select_value)
result_plot['AUC shift'] <- c(rep(0, 18), result$difference, result$diff_select_before)

p1 <- ggplot(result, aes(x = difference, y = diff_select_before, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "AUC shift with full feature",
    y = "AUC shift with selected feature"
  ) + geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed')
p1
ggsave('bld/figures/AUC_change_district_select.jpg', p1, width = 6,
       height = 5, dpi = 800)

p1 <- ggplot(result, aes(x = diff_select_before_abs, y = difference_abs, color = Model, shape = Model)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = r4_colors2) +
  geom_text_repel(aes(label = Method), size = 3, nudge_y = 0, direction = 'both', show.legend = FALSE) +
  theme_bw() + 
  labs(
    x = "absolute difference in AUC",
    y = "AUC during COVID"
  ) + geom_abline(intercept = 0, slope = 1, color = 'red')
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



result$diff_select_before/result$difference


