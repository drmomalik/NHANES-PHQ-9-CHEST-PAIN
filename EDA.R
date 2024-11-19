library(tidyverse)
library(nhanesA)
library(dplyr)

dpq_J <- nhanes("DPQ_J", translated = F) # for year 2017-2018

dpq_vars <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
              "DPQ060", "DPQ070", "DPQ080", "DPQ090")
# Filter out rows where any of the DPQ variables are 7 or 9 using package 'dplyr' (refused/don't know)
df_fin <- dpq_J %>%
  dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 7))) %>%
  dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 9)))

###### variable transformation

df_fin <- df_fin %>% mutate(DEPR_TOT = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090)

### Create PHQ-9 bins
df_fin$DEPR_LVL <- cut(df_fin$DEPR_TOT,
                     breaks = c(-Inf, 4, 9, 14, 19, Inf),
                     labels = 1:5,
                     right = TRUE) 


## Import outcome variable of chest pain presence and location
df_cp <- nhanes("CDQ_J", translated = F)
cp_var <- c("CDQ001", "CDQ002", "CDQ003",  "CDQ004",  "CDQ005",  "CDQ006",  "CDQ009A", "CDQ009B", "CDQ009C", "CDQ009D", "CDQ009E",
            "CDQ009F", "CDQ009G", "CDQ009H", "CDQ008", "CDQ010")
df_cp <- df_cp %>%
  dplyr::mutate(across(all_of(cp_var), ~na_if(., 77))) %>%
  dplyr::mutate(across(all_of(cp_var), ~na_if(., 99)))

## Merge data frames
merge_df <- merge(df_cp, df_fin, by = "SEQN", all = FALSE)

##Keep only those that have had chest pain
df <- subset(merge_df, CDQ001 == 1)

# Subset dataframe with participants that responded to any of the pain location questions
df_lim <- df[rowSums(is.na(df[, c("CDQ009A", "CDQ009B", "CDQ009C", "CDQ009D", 
                              "CDQ009E", "CDQ009F", "CDQ009G", "CDQ009H")])) < 8, ]


### Repeat above steps but for all years between 2005 - 2020 (Prepandemic)
years <- c("DPQ_D", "DPQ_E", "DPQ_F", "DPQ_G", "DPQ_H", 
          "DPQ_I", "DPQ_J", "P_DPQ", "CDQ_D", "CDQ_E", "CDQ_F", "CDQ_G", "CDQ_H", 
              "CDQ_I", "CDQ_J", "P_CDQ")

allyr_df <- data.frame()
for (i in 1:8) {
    df_dep <- nhanes(years[i], translated = F) 
    dpq_vars <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                  "DPQ060", "DPQ070", "DPQ080", "DPQ090")
    # Filter out rows where any of the DPQ variables are 7 or 9 using package 'dplyr' (refused/don't know)
    df_fin <- df_dep %>%
      dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 7))) %>%
      dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 9)))
    
    ###### variable transformation
    
    df_fin <- df_fin %>% mutate(DEPR_TOT = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090)
    
    ### Create PHQ-9 bins
    df_fin$DEPR_LVL <- cut(df_fin$DEPR_TOT,
                           breaks = c(-Inf, 4, 9, 14, 19, Inf),
                           labels = 1:5,
                           right = TRUE) # Right = TRUE includes upper limits in intervals
    
    
    ## Import outcome variable of chest pain presence and location
    df_cp <- nhanes(years[i+8], translated = F)
    cp_var <- c("CDQ001", "CDQ002", "CDQ003",  "CDQ004",  "CDQ005",  "CDQ006",  "CDQ009A", "CDQ009B", "CDQ009C", "CDQ009D", "CDQ009E",
                "CDQ009F", "CDQ009G", "CDQ009H", "CDQ008", "CDQ010")
    df_cp <- df_cp %>%
      dplyr::mutate(across(all_of(cp_var), ~na_if(., 99))) %>%
      dplyr::mutate(across(all_of(cp_var), ~na_if(., 77)))
    
    ## Merge data frames
    merge_df <- merge(df_cp, df_fin, by = "SEQN", all = FALSE)
    
    ##Keep only those that have had chest pain
    df <- subset(merge_df, CDQ001 == 1)
    
    # Subset dataframe with participants that responded to any of the pain location questions
    df_lim <- df[rowSums(is.na(df[, c("CDQ009A", "CDQ009B", "CDQ009C", "CDQ009D", 
                                      "CDQ009E", "CDQ009F", "CDQ009G", "CDQ009H")])) < 8, ]
    
    allyr_df <- rbind(allyr_df, df_lim)
}

loc_total <- numeric(8)
for (i in 8:15) {
  loc_total[i-7] <- 1682 - sum(is.na(allyr_df[i]))
}
print(loc_total)

allyr_df_coded <- allyr_df %>%
  mutate(across(c("CDQ009A", "CDQ009B", "CDQ009C", "CDQ009D", "CDQ009E", 
                  "CDQ009F", "CDQ009G", "CDQ009H", "CDQ008", "CDQ010"), 
                ~ ifelse(is.na(.), 0, 1)))
colnames(allyr_df_coded)[8:15] <- c("Right Arm", "Right Chest",
                          "Neck", "Upper Sternum",
                          "Lower Sternum", "Left Chest",
                          "Left arm", "Epigastric")

## Sample histograms of depression level and chest pain location
library(ggplot2)

# Reshape the data to long format for CDQ009A through CDQ009H
allyr_long <- allyr_df_coded %>%
  pivot_longer(cols = 8:15, names_to = "Outcome", values_to = "BinaryOutcome")

# Summarize data to get counts and proportions for each level of DEPR_LVL and each Outcome
allyr_summary <- allyr_long %>%
  group_by(DEPR_LVL, Outcome, BinaryOutcome) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(DEPR_LVL, Outcome) %>%
  mutate(total_count = sum(count),
         proportion = count / total_count * 100) %>%
  ungroup()

# Plot the histograms with faceting by Outcome
ggplot(allyr_summary, aes(x = DEPR_LVL, y = count, fill = factor(BinaryOutcome))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0("Total: ", total_count), 
                y = total_count + 1), 
            vjust = -0.5, size = 3, color = "black", 
            data = distinct(allyr_summary, DEPR_LVL, Outcome, .keep_all = TRUE)) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(x = "DEPR_LVL", y = "Count", fill = "Binary Outcome") +
  facet_wrap(~ Outcome) +
  theme_minimal()


## To plot only proportions


# Reshape the data to long format for CDQ009A through CDQ009H
allyr_long <- allyr_df_coded %>%
  pivot_longer(cols = 8:15, names_to = "Outcome", values_to = "BinaryOutcome")

# Summarize data to get counts and proportions for each level of DEPR_LVL and each Outcome (only for 1's)
allyr_summary <- allyr_long %>%
  group_by(DEPR_LVL, Outcome, BinaryOutcome) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(DEPR_LVL, Outcome) %>%
  mutate(total_count = sum(count),
         proportion_ones = ifelse(BinaryOutcome == 1, count / total_count * 100, 0)) %>%
  ungroup()

# Calculate max proportion for each Outcome to set dynamic y-axis limit
max_proportion <- allyr_summary %>%
  group_by(Outcome) %>%
  summarise(max_prop = max(proportion_ones, na.rm = TRUE))

# Merge max_proportion with the main summary data to adjust y-axis limits
allyr_summary <- allyr_summary %>%
  left_join(max_proportion, by = "Outcome") %>%
  mutate(y_max = max_prop * 1.5)

# Plot the histograms with the proportion of 1's on the y-axis and faceting by Outcome
ggplot(allyr_summary, aes(x = DEPR_LVL, y = proportion_ones)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(proportion_ones, 1), "%"), 
                y = proportion_ones / 2), # Adjust label position to be centered in the bar
            size = 3, color = "white") +
  labs(x = "DEPR_LVL", y = "Proportion of 1's (%)") +
  facet_wrap(~ Outcome, scales = "free_y") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Format as percentage
  theme_minimal() +
  theme(strip.text = element_text(size = 10), # Adjust facet label size
        panel.spacing = unit(1, "lines"), # Increase space between panels
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed




