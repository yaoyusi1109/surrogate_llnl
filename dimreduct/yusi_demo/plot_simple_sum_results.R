library(ggplot2)
library(dplyr)
library(tidyr)

# Robustly determine input path
csv_path <- "simple_sum_results.csv"
if (file.exists("dimreduct/yusi_demo/simple_sum_results.csv")) {
  csv_path <- "dimreduct/yusi_demo/simple_sum_results.csv"
}

results <- read.csv(csv_path)

# Ensure model factor levels for ordering
results$model <- factor(results$model, levels = c("Stationary GP", "DGP (D=1)", "DGP (D=d)"))

# 1. RMSE Boxplot
p1 <- ggplot(results, aes(x = model, y = rmse, fill = model)) +
  geom_boxplot() +
  labs(title = "RMSE Comparison", y = "RMSE", x = "Model") +
  theme_minimal()

# 2. CRPS Boxplot
p2 <- ggplot(results, aes(x = model, y = crps, fill = model)) +
  geom_boxplot() +
  labs(title = "CRPS Comparison", y = "CRPS", x = "Model") +
  theme_minimal()

# 3. Time Boxplot
p3 <- ggplot(results, aes(x = model, y = time, fill = model)) +
  geom_boxplot() +
  labs(title = "Computation Time", y = "Time (s)", x = "Model") +
  theme_minimal()

# Print plots
print(p1)
print(p2)
print(p3)

# Summary Table
summary_table <- results %>%
  group_by(model) %>%
  summarise(
    Mean_RMSE = mean(rmse),
    Mean_CRPS = mean(crps),
    Mean_Time = mean(time)
  )

print(summary_table)
