library(raster)
library(sp)
library(sf)
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
###############################################################
###############################################################
# read Uganda maps' shape file 
uga_shape_2 <- sf::st_read("uga_admbnda_ubos_20200824_shp/uga_admbnda_adm2_ubos_20200824.shp")
uga_shape_3 <- sf::st_read("uga_admbnda_ubos_20200824_shp/uga_admbnda_adm3_ubos_20200824.shp")
uga_shape_4 <- sf::st_read("uga_admbnda_ubos_20200824_shp/uga_admbnda_adm4_ubos_20200824.shp")
# read the cleaned data for further analysis 
FCSdata = read.csv("FullData2019.csv")
FCSusing <- FCSdata[!duplicated(FCSdata$hhid),]

giveFCs <- function(x){
  if(x<= 21){return(1)}
  else{return(0)}
}
# generate food insecure dummy variable, 1 for insecure, 0 for secure 
FCSusing['FCSStaus']<- apply(as.array(FCSusing$FCS), 1, giveFCs)
p <- ggplot(FCSusing, aes(x = FCS)) +
  geom_histogram(binwidth = 1, fill = "#2297E6", color = "black", alpha = 0.7) +
  # geom_vline(xintercept = 21.5, color = "#D55E00", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 21.5, xend = 21.5, y = 0, yend = 500), 
               linetype = "dashed", color = "#D55E00", size = 1) + 
  geom_segment(aes(x = 35.5, xend = 35.5, y = 0, yend = 500), 
               linetype = "dashed", color = "#009E73", size = 1) + 
  # geom_vline(xintercept = 35.5, color = "#009E73", linetype = "dashed", size = 1) +
  labs(title = " ",
       x = "FCS",
       y = "Household Frequency") + 
  theme_bw()
ggsave('bld/figures/FCS_Distribution.png', p, width = 8,
       height = 4, dpi = 1000)
#################################################################
### add date 
FCSusing['date'] <- paste(FCSusing$year, FCSusing$month, sep = '_')
FCSusing <- FCSusing %>%
  mutate(
    date = as.Date(paste0(date, "_1"), format = "%Y_%m_%d") # Add a day and parse
  )

##################################################################
# generate district inclusion indicators, 1 only appeared before covid, 
# 2 only during covid, and 3 for both cases 
district_status <- FCSusing %>%
  group_by(District) %>%
  summarize(
    before_covid = any(covid == 0),
    during_covid = any(covid == 1)
  ) %>%
  mutate(
    indicator = case_when(
      before_covid & during_covid ~ 3,
      before_covid & !during_covid ~ 1,
      !before_covid & during_covid ~ 2
    )
  ) %>%
  dplyr::select(District, indicator)
colnames(district_status)[2] <- 'District_covid'

county_status <- FCSusing %>%
  group_by(County) %>%
  summarize(
    before_covid = any(covid == 0),
    during_covid = any(covid == 1)
  ) %>%
  mutate(
    indicator = case_when(
      before_covid & during_covid ~ 3,
      before_covid & !during_covid ~ 1,
      !before_covid & during_covid ~ 2
    )
  ) %>%
  dplyr::select(County, indicator)
colnames(county_status)[2] <- 'county_covid'

suncounty_status <- FCSusing %>%
  group_by(s1aq5a) %>%
  summarize(
    before_covid = any(covid == 0),
    during_covid = any(covid == 1)
  ) %>%
  mutate(
    indicator = case_when(
      before_covid & during_covid ~ 3,
      before_covid & !during_covid ~ 1,
      !before_covid & during_covid ~ 2
    )
  ) %>%
  dplyr::select(s1aq5a, indicator)
colnames(suncounty_status)[2] <- 'subcounty_covid'
# merge with full data 
FCSdata_covid <- merge(FCSusing, district_status, by = 'District', all.x = TRUE)
FCSdata_covid <- merge(FCSdata_covid, county_status, by = 'County', all.x = TRUE)
FCSdata_covid <- merge(FCSdata_covid, suncounty_status, by = 's1aq5a', all.x = TRUE)

# plot the monthly FCS distribution 
p2 <- ggplot(FCSusing, aes(x = FCS, y = factor(date), fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "FCS", option = "D") + 
  geom_segment(aes(x = 21, xend = 21, y = 0.5, yend = length(unique(date)) + 1),
               linetype = "dashed", color = "#D55E00", size = 1) +
  geom_segment(aes(x = 35, xend = 35, y = 0.5, yend = length(unique(date)) + 1),
               linetype = "dashed", color = "#009E73", size = 1) +
  geom_segment(aes(x = -2, xend = 120, y = length(unique(date))/2 + 1, yend = length(unique(date))/2 + 1),
               color = "red", size = 1) +
  labs(
    x = "FCS",
    y = "Date",
    fill = "Density"
  ) +
  theme_classic() +
  scale_y_discrete(
    breaks = levels(factor(FCSusing$date)),  # Ensure y-axis is labeled correctly
    labels = format(as.Date(levels(factor(FCSusing$date))), "%Y-%m")
  )
p2
ggsave('bld/figures/FCS_Distribution_month.png', p2, width = 8,
       height = 6, dpi = 1000)
# plot monthly food insecure household proportions 
summary_data <- FCSusing %>%
  group_by(date) %>%
  summarize(
    mean_FCS = mean(FCSStaus),
    sd_FCS = sd(FCSStaus)
  )
summary_data
p3 <- ggplot(summary_data) +
  geom_point(aes(x = date, y = mean_FCS), size = 3, color = "darkblue") + # Points for mean values
  geom_text(aes(x = date, y = mean_FCS, label = scales::percent(mean_FCS)), 
            vjust = -0.5, color = "darkblue", size = 3) + # Annotations with percentage
  # geom_errorbar(aes(ymin = mean_FCS - sd_FCS, ymax = mean_FCS + sd_FCS), width = 0.2) +
  geom_vline(xintercept = as.Date("2020-03-01"), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-06-01"), color = "red", linetype = "dashed") + # Vertical red lines
  annotate("text", x = as.Date("2020-04-15"), y = mean(summary_data$mean_FCS), 
           label = "COVID", color = "red", angle = 0, vjust = -0.5) + # Annotation
  labs(
    x = "Date",
    y = "Food insecure proportion"
  ) +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months")
p3
ggsave('bld/figures/FoodInsecure_month.png', p3, width = 8,
       height = 6, dpi = 1000)
###############################################################################
###############################################################################
# here we compare the regional differences of food insecurity before and during covid 
FCSdata_covid_compare_d <- FCSdata_covid[FCSdata_covid$District_covid == 3, ]
FCSdata_covid_compare_c <- FCSdata_covid[FCSdata_covid$county_covid == 3, ]
FCSdata_covid_compare_s <- FCSdata_covid[FCSdata_covid$subcounty_covid == 3, ]
FCSdata_covid_compare_d2 <- FCSdata_covid[FCSdata_covid$District_covid != 3, ]
FCSdata_covid_compare_c2 <- FCSdata_covid[FCSdata_covid$county_covid != 3, ]
FCSdata_covid_compare_s2 <- FCSdata_covid[FCSdata_covid$subcounty_covid != 3, ]
table(district_status$District_covid)
table(county_status$county_covid)
table(suncounty_status$subcounty_covid)

FCSdata_covid['boxid'] <- 'total'
FCSdata_covid_compare_d['boxid'] <- 'district within'
FCSdata_covid_compare_c['boxid'] <- 'county within'
FCSdata_covid_compare_s['boxid'] <- 'sub-county within'
FCSdata_covid_compare_d2['boxid'] <- 'district between'
FCSdata_covid_compare_c2['boxid'] <- 'county between'
FCSdata_covid_compare_s2['boxid'] <- 'sub-county between'

FCS_boxplot <- bind_rows(FCSdata_covid, FCSdata_covid_compare_d, FCSdata_covid_compare_c, FCSdata_covid_compare_s, 
                         FCSdata_covid_compare_d2, FCSdata_covid_compare_c2, FCSdata_covid_compare_s2)
FCS_boxplot['sample'] <- FCS_boxplot$boxid
FCS_boxplot['Covid'] <- ifelse(FCS_boxplot$covid == 1, 'during', 'before')
# Box plot facetted by "dose" (not included in the main text)
p4 <- ggboxplot(FCS_boxplot, x = "Covid", y = "FCS",
               color = "Covid", palette = "jco",
               facet.by = "sample", short.panel.labs = FALSE)+ 
  stat_compare_means(label = "p.signif", method = 't.test', vjust = 0.75)
ggsave('bld/figures/FCS_compare_covid_geo.png', p4, width = 8,
       height = 6, dpi = 1000)
##################################################################
summary_data <- FCS_boxplot %>%
  group_by(sample, Covid) %>%
  summarize(
    mean_FCSStatus = mean(FCS, na.rm = TRUE),
    ci_lower = mean_FCSStatus - qt(0.975, df = n() - 1) * sd(FCS, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_FCSStatus + qt(0.975, df = n() - 1) * sd(FCS, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Perform t-tests and collect p-values
t_test_results <- FCS_boxplot %>%
  group_by(sample) %>%
  do({
    t_test <- t.test(FCS ~ Covid, data = .)
    data.frame(sample = unique(.$sample), p_value = t_test$p.value)
  })

# Add significance stars
t_test_results <- t_test_results %>%
  mutate(
    signif = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*",
      p_value <= 0.1 ~ "+",
      TRUE ~ "ns"
    )
  )

# Merge t-test results with summary data
summary_data <- summary_data %>%
  left_join(t_test_results, by = "sample")

# Create the plot
p8 <- ggplot(summary_data, aes(x = Covid, y = mean_FCSStatus, color = Covid)) +
  geom_point(size = 1) +  # Mean points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +  # Confidence intervals
  facet_wrap(~ sample, scales = "free", labeller = label_both) +  # Facet by sample
  geom_text(aes(x = 1.5, y = 65, label = signif),  # Show significance stars at a fixed position
            inherit.aes = FALSE, color = "black", size = 6, hjust = 0.5) +
  labs(
    y = "FCS",
    color = "Covid"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") + 
  coord_cartesian(ylim = c(30, 70))  # Restrict y-axis range
ggsave('bld/figures/FCS_compare_covid_geo_ci.jpg', p8, width = 8,
       height = 6, dpi = 2000)
################### compare the proportion of food insecure HHs ##############################
# Summarize data: calculate mean and confidence intervals
summary_data <- FCS_boxplot %>%
  group_by(sample, Covid) %>%
  summarize(
    mean_FCSStatus = mean(FCSStaus, na.rm = TRUE),
    ci_lower = mean_FCSStatus - qt(0.975, df = n() - 1) * sd(FCSStaus, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_FCSStatus + qt(0.975, df = n() - 1) * sd(FCSStaus, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Perform t-tests and collect p-values
t_test_results <- FCS_boxplot %>%
  group_by(sample) %>%
  do({
    t_test <- t.test(FCSStaus ~ Covid, data = .)
    data.frame(sample = unique(.$sample), p_value = t_test$p.value)
  })

# Add significance stars
t_test_results <- t_test_results %>%
  mutate(
    signif = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*",
      p_value <= 0.1 ~ "+",
      TRUE ~ "ns"
    )
  )

# Merge t-test results with summary data
summary_data <- summary_data %>%
  left_join(t_test_results, by = "sample")

summary_data2 <- summary_data[c(1, 2, 5, 6, 9, 10, 13, 14), ]
summary_data2$sample <- rep(c('county', 'district', 'sub-county', 'total'), each = 2)
summary_data3 <- summary_data[c(3, 4, 7, 8, 11, 12), ]
# Create the plot
p5 <- ggplot(summary_data2, aes(x = Covid, y = mean_FCSStatus, color = Covid)) +
  geom_point(size = 4) +  # Mean points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +  # Confidence intervals
  facet_wrap(~ sample, scales = "free", labeller = label_both) +  # Facet by sample
  geom_text(aes(x = 1.5, y = 0.08, label = signif),  # Show significance stars at a fixed position
            inherit.aes = FALSE, color = "black", size = 6, hjust = 0.5) +
  labs(
    y = "Food Insecure Household Proportion",
    color = "Covid"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(-0.02, 0.08))  # Restrict y-axis range

p6 <- ggplot(summary_data3, aes(x = Covid, y = mean_FCSStatus, color = Covid)) +
  geom_point(size = 4) +  # Mean points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +  # Confidence intervals
  facet_wrap(~ sample, scales = "free", labeller = label_both) +  # Facet by sample
  geom_text(aes(x = 1.5, y = 0.08, label = signif),  # Show significance stars at a fixed position
            inherit.aes = FALSE, color = "black", size = 6, hjust = 0.5) +
  labs(
    y = "Food Insecure Household Proportion",
    color = "Covid"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0, 0.08))  # Restrict y-axis range
ggsave('bld/figures/FoodInsecure_compare_geo_between.jpg', p5, width = 8,
       height = 6, dpi = 1000)
ggsave('bld/figures/FoodInsecure_compare_geo_within.jpg', p6, width = 8,
       height = 3, dpi = 1000)
###########################################################################
# generate new dataframe 
summary_data <- FCSdata_covid %>%
  group_by(date) %>%
  summarize(
    mean_FCS = mean(FCSStaus),
    sd_FCS = sd(FCSStaus)
  )
summary_data
FCSdata_covid <- merge(FCSdata_covid, as.data.frame(summary_data[, 1:2]), by = 'date', all.x = TRUE)
write.csv(FCSdata_covid, 'bld/datasets/data2019.csv')

####################### here the map plots starts ################################
# now create the regional average food insecurity status 
# Group by mean using dplyr
Plotting_adm2 <- FCSdata_covid[, c('District', 'FCSStaus', 'District_covid')] %>% group_by(District) %>% 
  summarise(FCSratio=mean(FCSStaus), 
            covid = mean(District_covid), 
            .groups = 'drop')
# Convert tibble to df
df2 <- Plotting_adm2 %>% as.data.frame()
summary(df2$FCSratio)
table(df2$covid)

Plotting_adm3 <- FCSdata_covid[, c('County', 'FCSStaus', 'county_covid')] %>% group_by(County) %>% 
  summarise(FCSratio=mean(FCSStaus), 
            covid = mean(county_covid), 
            .groups = 'drop')
# Convert tibble to df
df3 <- Plotting_adm3 %>% as.data.frame()
summary(df3$FCSratio)
table(df3$covid)

Plotting_adm4 <- FCSdata_covid[, c('s1aq5a', 'FCSStaus', 'subcounty_covid')] %>% group_by(s1aq5a) %>% 
  summarise(FCSratio=mean(FCSStaus), 
            covid = mean(subcounty_covid), 
            .groups = 'drop')
# Convert tibble to df
df4 <- Plotting_adm4 %>% as.data.frame()
summary(df4$FCSratio)
table(df4$covid)
############################################################
uga_shape_2$ADM2_EN <- tolower(uga_shape_2$ADM2_EN)
# Check if all unique elements of vec1 are in vec2
all_included <- all(df2$District %in% uga_shape_2$ADM2_EN)
all_included
not_included <- setdiff(df2$District, uga_shape_2$ADM2_EN)
not_included
colnames(df2)[1] <- 'ADM2_EN'
shape_adm2 <- merge(uga_shape_2, df2, by = 'ADM2_EN', all.x = T)


uga_shape_3$ADM3_EN <- tolower(uga_shape_3$ADM3_EN)
# Check if all unique elements of vec1 are in vec2
all_included <- all(df3$County %in% uga_shape_3$ADM3_EN)
all_included
not_included <- setdiff(df3$County, uga_shape_3$ADM3_EN)
not_included
colnames(df3)[1] <- 'ADM3_EN'
shape_adm3 <- merge(uga_shape_3, df3, by = 'ADM3_EN', all.x = T)

uga_shape_4$ADM4_EN <- tolower(uga_shape_4$ADM4_EN)
df4$s1aq5a <- tolower(df4$s1aq5a)
# Check if all unique elements of vec1 are in vec2
all_included <- all(df4$s1aq5a %in% uga_shape_4$ADM4_EN)
all_included
not_included <- setdiff(df4$s1aq5a, uga_shape_4$ADM4_EN)
not_included
colnames(df4)[1] <- 'ADM4_EN'
shape_adm4 <- merge(uga_shape_4, df4, by = 'ADM4_EN', all.x = T)

shape_adm2['rationlog'] <- log(1 + shape_adm2$FCSratio)
map <- ggplot(shape_adm2[c(1, 15, 16, 17, 18)]) +
  geom_sf(aes(fill = FCSratio), linewidth = 0.4, color = 'black', na.rm = TRUE) +  # na.rm removes empty polygons
  scale_fill_viridis_c(
    name = "Proportion of food insecure households", 
    option = "plasma",  # A color-blind-friendly palette
    na.value = "transparent",   # Ensures empty polygons are blank, 
    direction = -1
  ) +
  theme_void() +
  theme(
    legend.position = "top",  # Moves the color bar above the map
    legend.key.height = unit(0.3, "cm"),  # Adjust height for horizontal legend
    legend.key.width = unit(2, "cm")  # Adjust width for horizontal legend
  ) +
  guides(fill = guide_colorbar(
    direction = "horizontal",  # Ensures horizontal orientation
    title.position = "top",
    title.hjust = 0.5
  ))
ggsave('bld/figures/FCS_map.png', map, width = 6,
       height = 6, dpi = 1000)
#############################################################################
#############################################################################
################################ Moran I test ###############################
library(spdep)
################## district plot 
library(geosphere)
s.center <- st_point_on_surface(shape_adm2)
s.coord <- st_coordinates(s.center)
adj_list <- poly2nb(shape_adm2, queen=TRUE)

# Initialize a vector to store distances
distances <- c()

# Calculate distances for each pair of adjacent districts
for (i in seq_along(adj_list)) {
  for (j in adj_list[[i]]) {
    if (i < j) {  # To avoid duplicate calculations
      dist <- distHaversine(s.coord[i, ], s.coord[j, ])
      distances <- c(distances, dist)
    }
  }
}

# Compute the average distance of adjacent districts
average_distance <- mean(distances)

# Print the average distance in kilometers
upper <- average_distance/1000
thresh_yi <- data.frame(threshold = 17:100, p = NA, adj = NA)
for (i in 1:dim(thresh_yi)[1]) {
  t <- thresh_yi[i, 'threshold']
  weights <- dnearneigh(s.coord, 0, t, longlat = TRUE) # Neighbors within threshold
  weights_list <- nb2listw(weights, style = "W", zero.policy = TRUE) # Normalize weights
  thresh_yi[i, 'adj'] <- mean(card(weights_list$neighbours))
  moran_test <- moran.test(shape_adm2$FCSratio, listw = weights_list, zero.policy = TRUE, na.action = na.omit)
  thresh_yi[i, 'p'] <- moran_test$p.value
}
library(ggplot2)
p9 <- ggplot(thresh_yi, aes(x = threshold, y = p, color = adj)) +
  geom_point() +
  geom_hline(yintercept = 0.01, color = 'red') +
  geom_vline(xintercept = 23.87, color = '#D55E00') +
  geom_vline(xintercept = upper, color = '#CC79A7') +
  annotate("text", x = 23.87, y = 0.15, 
           label = "average radius\n per district", color = "#D55E00", angle = 0, vjust = -0.5) + # Annotation
  annotate("text", x = upper, y = 0.15, 
           label = "average distance\n per district", color = "#CC79A7", angle = 0, vjust = -0.5) + # Annotation
  annotate("text", x = 18, y = 0.015, 
           label = "p = 0.01", color = "red", angle = 0, vjust = -0.5) + # Annotation
  labs(
    x = "Distance in km", 
    y = "p-value of Moran's I test", 
    color = "average adjacent districts"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",  # Move color bar above the plot
    legend.direction = "horizontal"  # Make color bar horizontal
  ) +
  scale_color_viridis(discrete = FALSE) +  # Apply viridis color scheme
  # Add labels to every 5th point based on order in the dataset
  geom_text(
    data = thresh_yi %>%
      slice(seq(1, n(), by = 5)),  # Select every 5th point
    aes(label = round(adj, 1)),
    vjust = -0.5,
    size = 3
  )
p9
ggsave('bld/figures/Moran_district.jpg', p9, width = 8,
       height = 6, dpi = 1000)
# 
####################### county plot 
s.center <- st_point_on_surface(shape_adm3)
s.coord <- st_coordinates(s.center)
adj_list <- poly2nb(shape_adm3, queen=TRUE)

# Initialize a vector to store distances
distances <- c()

# Calculate distances for each pair of adjacent districts
for (i in seq_along(adj_list)) {
  for (j in adj_list[[i]]) {
    if (i < j) {  # To avoid duplicate calculations
      dist <- distHaversine(s.coord[i, ], s.coord[j, ])
      distances <- c(distances, dist)
    }
  }
}

# Compute the average distance of adjacent districts
average_distance <- mean(distances)

# Print the average distance in kilometers
upper <- average_distance/1000

thresh_yi <- data.frame(threshold = 10:100, p = NA, adj = NA)
for (i in 1:dim(thresh_yi)[1]) {
  t <- thresh_yi[i, 'threshold']
  weights <- dnearneigh(s.coord, 0, t, longlat = TRUE) # Neighbors within threshold
  weights_list <- nb2listw(weights, style = "W", zero.policy = TRUE) # Normalize weights
  thresh_yi[i, 'adj'] <- mean(card(weights_list$neighbours))
  moran_test <- moran.test(shape_adm3$FCSratio, listw = weights_list, zero.policy = TRUE, na.action = na.omit)
  thresh_yi[i, 'p'] <- moran_test$p.value
}
p10 <- ggplot(thresh_yi, aes(x = threshold, y = p, color = adj)) +
  geom_point() +
  geom_hline(yintercept = 0.01, color = 'red') +
  geom_vline(xintercept = 19.2, color = '#D55E00') +
  geom_vline(xintercept = upper, color = '#CC79A7') +
  annotate("text", x = 19.2, y = 0.25, 
           label = "average radius\n per county", color = "#D55E00", angle = 0, vjust = -0.5) + # Annotation
  annotate("text", x = upper, y = 0.25, 
           label = "average distance\n per county", color = "#CC79A7", angle = 0, vjust = -0.5) + # Annotation
  annotate("text", x = 18, y = 0.015, 
           label = "p = 0.01", color = "red", angle = 0, vjust = -0.5) + # Annotation
  labs(
    x = "Distance in km", 
    y = "p-value of Moran's I test", 
    color = "average adjacent counties"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",  # Move color bar above the plot
    legend.direction = "horizontal"  # Make color bar horizontal
  ) +
  scale_color_viridis(discrete = FALSE) +  # Apply viridis color scheme
  # Add labels to every 5th point based on order in the dataset
  geom_text(
    data = thresh_yi %>%
      slice(seq(1, n(), by = 5)),  # Select every 5th point
    aes(label = round(adj, 1)),
    vjust = -0.5,
    size = 3
  )
p10
ggsave('bld/figures/Moran_county.jpg', p10, width = 8,
       height = 6, dpi = 1000)

# ########################################################################
# read 2016 data 
FCSdata16 = read.csv("FullData2016.csv")
FCSusing16 <- FCSdata16[!duplicated(FCSdata16$hhid),]

giveFCs <- function(x){
  if(x<= 21){return(1)}
  else{return(0)}
}
FCSusing16['FCSStaus']<- apply(as.array(FCSusing16$FCS), 1, giveFCs)
FCSusing16['date'] <- paste(FCSusing16$year, FCSusing16$month, sep = '_')
FCSusing16 <- FCSusing16 %>%
  mutate(
    date = as.Date(paste0(date, "_1"), format = "%Y_%m_%d") # Add a day and parse
  )
# plot monthly food insecure household proportions 
summary_data <- FCSusing16 %>%
  group_by(date) %>%
  summarize(
    mean_FCS = mean(FCSStaus),
    sd_FCS = sd(FCSStaus)
  )
summary_data
FCSusing16 <- merge(FCSusing16, as.data.frame(summary_data[, 1:2]), by = 'date', all.x = TRUE)
write.csv(FCSusing16, 'bld/datasets/data2016.csv')

p3 <- ggplot(summary_data) +
  geom_point(aes(x = date, y = mean_FCS), size = 3, color = "darkblue") + # Points for mean values
  # geom_errorbar(aes(ymin = mean_FCS - sd_FCS, ymax = mean_FCS + sd_FCS), width = 0.2) +
  labs(
    x = "Date",
    y = "Food insecure proportion"
  ) +
  theme_bw() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 months") 
p3
ggsave('bld/figures/FoodInsecure_month2016.png', p3, width = 8,
       height = 6, dpi = 1000)

###############################################################################
# summary statistics of data
train_df <- read.csv('bld/datasets/combined_train_data.csv')
test_df_district <- read.csv('bld/datasets/combined_test_data_district.csv')
test_df_county <- read.csv('bld/datasets/combined_test_data_county.csv')
test_df_subcounty <- read.csv('bld/datasets/combined_test_data_subcounty.csv')

# Load necessary library
library(dplyr)

# Example data transformation for string "TRUE"/"FALSE" to numeric 1/0
convert_true_false <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      as.numeric(col == "True")
    } else {
      col
    }
  })
  return(df)
}

# Apply transformation to each dataframe
train_df <- convert_true_false(train_df)
test_df_district <- convert_true_false(test_df_district)
test_df_county <- convert_true_false(test_df_county)
test_df_subcounty <- convert_true_false(test_df_subcounty)

train_df <- apply(train_df, 2, as.numeric)
test_df_district <- apply(test_df_district, 2, as.numeric)
test_df_county <- apply(test_df_county, 2, as.numeric)
test_df_subcounty <- apply(test_df_subcounty, 2, as.numeric)
train_df <- as.data.frame(train_df)
test_df_district <- as.data.frame(test_df_district)
test_df_county <- as.data.frame(test_df_county)
test_df_subcounty <- as.data.frame(test_df_subcounty)
# Function to calculate summary statistics for each column
 
# Function to calculate summary statistics for each column
calculate_summary <- function(df) {
  df %>%
    summarise(across(everything(), list(
      min = ~min(.x, na.rm = TRUE),
      q1 = ~quantile(.x, 0.25, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      q3 = ~quantile(.x, 0.75, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE), 
      Missing = ~sum(is.na(.x))
    ))) %>%
    pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_pattern = "(.*)_(.*)")
}

# Calculate summaries for each dataframe with an added 'type' column
summary_train <- calculate_summary(train_df) %>% mutate(type = "train")
summary_district <- calculate_summary(test_df_district) %>% mutate(type = "test_district")
summary_county <- calculate_summary(test_df_county) %>% mutate(type = "test_county")
summary_subcounty <- calculate_summary(test_df_subcounty) %>% mutate(type = "test_subcounty")

summary_train_w <- summary_train %>%
  pivot_wider(
    names_from = stat, 
    values_from = value
  )
summary_district_w <- summary_district %>%
  pivot_wider(
    names_from = stat, 
    values_from = value
  )
summary_county_w <- summary_county %>%
  pivot_wider(
    names_from = stat, 
    values_from = value
  )
summary_subcounty_w <- summary_subcounty %>%
  pivot_wider(
    names_from = stat, 
    values_from = value
  )

# Combine all summaries into a single dataframe
summary_combined <- bind_rows(summary_train_w, summary_district_w, summary_county_w, summary_subcounty_w)


# Function to round numbers conditionally
conditional_round <- function(x) {
  rounded <- round(x, 2)
  if (rounded == 0) {
    rounded <- round(x, 4)
  }
  return(rounded)
}

summary_combined <- summary_combined %>%
  arrange(variable) %>%
  mutate(across(where(is.numeric), ~sapply(., conditional_round)))


View(summary_combined)
colnames(summary_combined)[2] <- 'data'
write.csv(summary_combined, 'bld/summary_stats.csv')

################################ in text table ################################### 
# summary statistics of data
train_df <- read.csv('bld/datasets/combined_train_data.csv')
test_df <- read.csv('bld/datasets/combined_test_data.csv')
# Load necessary library
library(dplyr)

# Example data transformation for string "TRUE"/"FALSE" to numeric 1/0
convert_true_false <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      as.numeric(col == "True")
    } else {
      col
    }
  })
  return(df)
}

# Apply transformation to each dataframe
train_df <- convert_true_false(train_df)
test_df <- convert_true_false(test_df)
train_df <- apply(train_df, 2, as.numeric)
test_df <- apply(test_df, 2, as.numeric)

train_df <- as.data.frame(train_df)
test_df <- as.data.frame(test_df)
# Function to calculate summary statistics for each column
calculate_summary <- function(df) {
  df %>%
    summarise(across(everything(), list(
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    ))) %>%
    pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_pattern = "(.*)_(.*)")
}

# Calculate summaries for each dataframe with an added 'type' column
summary_train <- calculate_summary(train_df) %>% mutate(type = "train")
summary_test <- calculate_summary(test_df) %>% mutate(type = "test")

summary_train_w <- summary_train %>%
  pivot_wider(
    names_from = stat, 
    values_from = value
  )
summary_test_w <- summary_test %>%
  pivot_wider(
    names_from = stat, 
    values_from = value
  )

# Combine all summaries into a single dataframe
summary_combined <- bind_columns(summary_train_w, summary_test_w)

summary_combined <- cbind(summary_train_w, summary_test_w)

head(summary_combined)
summary_combined[, c(3, 4, 5, 8, 9, 10)] <- round(summary_combined[, c(3, 4, 5, 8, 9, 10)], 2)

View(summary_combined)
write.csv(summary_combined, 'bld/summary_stats_intext.csv')

