
dfc <- wd

## Correlation matrix for regions of chest pain stratified by PHQ9-score

# List of binary variables
binary_vars <- c("CDQ009A", "CDQ009B", "CDQ009C", "CDQ009D", "CDQ009E", "CDQ009F", "CDQ009G", "CDQ009H")

# Convert binary variables to numeric
dfc[binary_vars] <- lapply(dfc[binary_vars], as.numeric)

# Function to calculate correlation matrix for a subset
calculate_correlation <- function(subset_df) {
  cor(subset_df[, binary_vars], method = "pearson")  # Can use "spearman" if desired
}

# Split the data by the categorical variable
stratified_correlation <- lapply(split(dfc, dfc$DEPR_LVL), calculate_correlation)

# View correlation matrices for each group
stratified_correlation

library(corrplot)

# Visualize correlation matrix for each level
for (level in names(stratified_correlation)) {
  cat("Correlation Matrix for Group:", level, "\n")
  corrplot(stratified_correlation[[level]], method = "circle", title = paste("Group", level))
}



## Correlation matrix for PHQ-9 score (cont.) by total regions of chest pain

# Calculate correlation while excluding NAs
correlation <- cor(wd$TOT_REG, wd$DEPR_TOT, use = "complete.obs", method = "pearson")
print(correlation)

library(ggplot2)

# Create a data frame excluding rows with NAs
wd_clean <- na.omit(wd[, c("TOT_REG", "DEPR_TOT")])

# Scatterplot with regression line
ggplot(wd_clean, aes(x = TOT_REG, y = DEPR_TOT)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red") +  # Add regression line
  labs(title = paste("Correlation =", round(correlation, 2)),
       x = "TOT_REG", y = "DEPR_TOT") +
  theme_minimal()