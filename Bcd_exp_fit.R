library(tidyverse)
library(readxl)
library(writexl)
library(fs)
library(stringr)

parent_folder <- "/Users/shaheenkabir/Bcd_analysis/Ore"  # Adjust as needed
folder_pattern <- "^Ore.*$"  
file_pattern <- paste0(folder_pattern, "\\.xlsx") 

log_path <- file.path(parent_folder, "pipeline_log.txt")
write("=== Analysis Log -- Failures ===\n", log_path)
lambda_table <- tibble(file = character(), lambda = numeric(), folder = character())

all_dirs <- dir_ls(parent_folder, type = "directory", recurse = FALSE)
matched_folders <- all_dirs[str_detect(basename(all_dirs), folder_pattern)]
matched_folders <- sort(matched_folders)
for (input_folder in matched_folders) {
  cat("\nStarting analysis in:", input_folder, "\n")
  output_folder <- file.path(input_folder, "results")
  dir_create(output_folder)
  all_files <- dir_ls(output_folder, glob = "*.xlsx")
  processed_files <- all_files[str_detect(basename(all_files), folder_pattern)]
  # Start assigning variables.
  percent_list <- list()
  intensity_list <- list()
  sample_names <- c()
  
  for (i in seq_along(processed_files)) {
    file <- processed_files[i]
    filename <- basename(file)
    prefix <- str_extract(tolower(filename), "^[^_\\- ]+")
    if (prefix %in% c("yw", "Berlink", "CantonS", "Ore", "BR81", "BR87", "BR45", "BR8","ISO1", "ISO2","MASS")){
      folder <- prefix
    } else {folder <- "general"}
    tryCatch({
      df <- read_excel(file, sheet = "Sheet1")
      # First column is percent length and 2nd column is raw intensity.
      percent_col <- df[[1]]
      intensity_col <- df[[2]]
      # assign labels and sample names according to basenames
      label <- paste0(basename(input_folder), "_", i)
      sample_names <- c(sample_names, label)
      # Normalizing the intensity (all file max ~ 1)
      norm_intensity <- intensity_col/max(intensity_col)
      percent_list[[label]] <- percent_col
      intensity_list[[label]] <- norm_intensity
    },
    error = function(e) {
      msg <- paste0("skipping ", filename, ": ", e$message, "\n")
      write(msg, log_path, append = TRUE)
    })
    # Doing individual fit.
    df_single <- df %>% 
      mutate(Percent_length = (df$AP_Position/max(df$AP_Position))*100) %>% 
      mutate(Normalized_intensity = df$Intensity/max(df$Intensity))
    fit_single <- nls(Intensity ~ A * exp(-Percent_length / lambda) + B,data = df_single, 
                      start = list(A = max(df_single$Intensity), lambda = 20, B = min(df_single$Intensity)))
    
    lambda_val <- round(coef(fit_single)["lambda"], 2)
    baseline_val <- round(coef(fit_single)["B"], 2)
    df_single$fit <- predict(fit_single, newdata = df_single)
    lambda_table <- bind_rows(lambda_table, tibble(file = filename, lambda = lambda_val, 
                                                   baseline = baseline_val,folder = folder))
  }
  percent_df <- as_tibble(percent_list)
  percent_df$Average <- rowMeans(percent_df, na.rm = TRUE)
  intensity_df <- as_tibble(intensity_list)
  intensity_df$Average <- rowMeans(intensity_df, na.rm = TRUE)
  combined_path <- file.path(output_folder, "combined_bcd.xlsx")
  write_xlsx(list(
    "Percent Length" = percent_df,
    "Normalized Intensity" = intensity_df
  ), combined_path)
}
write_xlsx(lambda_table, file.path(output_folder, "lambda_summary.xlsx"))
# Combining data from all samples.
length_averages <- list()
intensity_averages <- list()
labels <- c()

for (subdir in matched_folders) {
  combined_avg_path <- file.path(subdir, "results", "combined_bcd.xlsx")
  tryCatch({
    percent_df <- read_excel(combined_avg_path, sheet = "Percent Length")
    intensity_df <- read_excel(combined_avg_path, sheet = "Normalized Intensity")
    avg_length <- percent_df$Average
    avg_intensity <- intensity_df$Average
    
    name <- basename(subdir)
    labels <- c(labels, name)
    length_averages[[name]] <- avg_length
    intensity_averages[[name]] <- avg_intensity
  },
  error = function(e) {
    msg <- paste0("⚠️ Could not extract from ", subdir, ": ", e$message, "\n")
    write(msg, log_path, append = TRUE)
  })
}

# Save the cross-folder averages

length_avg_df <- as_tibble(length_averages)
intensity_avg_df <- as_tibble(intensity_averages)
output_combined_file <- file.path(parent_folder, "all_avg_summary.xlsx")
write_xlsx(list(
  "Average Percent Length" = length_avg_df,
  "Average Intensity" = intensity_avg_df
), output_combined_file)

# Plot average curves.
avg_df_22 <- data.frame(
  Length = length_avg_df[[1]],
  Intensity = intensity_avg_df[[1]],
  Condition = "22C")
avg_df_25 <- data.frame(
  Length = length_avg_df[[2]],
  Intensity = intensity_avg_df[[2]],
  Condition = "25C")
avg_df_29 <- data.frame(
  Length = length_avg_df[[3]],
  Intensity = intensity_avg_df[[3]],
  Condition = "29C")
head(avg_df_22)
all_avg_df <- bind_rows(avg_df_22, avg_df_25, avg_df_29) %>%
  mutate(Condition = factor(Condition, levels = c("22C", "25C", "29C"))) %>% 
  mutate(Percent_length = Length * 100)

# Fitting exponential.
fits_list <- list()
fits_df <- tibble()
fit_results <- tibble(Condition = character(), Lambda = numeric(), Baseline_B = numeric())
conditions <- unique(all_avg_df$Condition)
for (cond in conditions) {
  df_sample <- all_avg_df %>% 
    dplyr::filter(Condition == cond)
  fit <- tryCatch(
    nls(Intensity ~ A * exp(-Percent_length / lambda) + B,
        data = df_sample,
        start = list(A = max(df_sample$Intensity), lambda = 30, B = min(df_sample$Intensity))),
    error = function(e) NULL)
  df_sample$Fit <- predict(fit, newdata = df_sample)
  fits_list[[cond]] <- df_sample
  coefs <- coef(fit)
  lambda_val <- round(coefs["lambda"], 2)
  baseline_val <- round(coefs["B"], 2)
  fit_results <- bind_rows(fit_results, 
                           tibble(Condition = cond, Lambda = lambda_val, Baseline_B = baseline_val))
  fits_df <- bind_rows(fits_df, df_sample)
}
fit_path <- file.path(parent_folder, "all_fitted.xlsx")
write_xlsx(fits_list, fit_path)
write_xlsx(list(Fitted_Curves = fits_df,Fit_Summary = fit_results),
           file.path(parent_folder, "all_fitted_and_summary.xlsx"))

# Configuration for proper graph.
all_avg_df <- all_avg_df %>% mutate(Condition = case_when(Condition == "22C" ~ "22 °C", 
                                                          Condition == "25C" ~ "25 °C",
                                                          TRUE               ~ "29 °C"))
fit_results <- fit_results %>% mutate(Condition = case_when(Condition == "22C" ~ "22 °C", 
                                                            Condition == "25C" ~ "25 °C",
                                                            TRUE               ~ "29 °C"))
fits_df <- fits_df %>% mutate(Condition = case_when(Condition == "22C" ~ "22 °C", 
                                                    Condition == "25C" ~ "25 °C",
                                                    TRUE               ~ "29 °C"))
lambda_text <- fit_results %>%
  mutate(Label = paste0(Condition, ": λ = ", Lambda)) %>%
  pull(Label) %>%
  paste(collapse = "\n")
all_avg_df$LineType <- "Raw"
fits_df$LineType <- "Fit"
# Plotting the graph.
colors <- c("22 °C" = "#cce5ff", "25 °C" = "#3399ff", "29 °C" = "#336699")
p <- ggplot(all_avg_df, aes(x = Percent_length, y = Intensity, color = Condition, linetype = LineType)) +
  geom_line(size = 1) +
  geom_line(data = fits_df, aes(x = Percent_length, y = Fit, color = Condition)) +
  labs(x = "Percent Length (%)", y = "Intensity", title = "Raw With Exponential Fits for Oregon") +
  scale_color_manual(values = colors, labels = c("22 °C (n = 7)", "25 °C (n = 6)", "29 °C (n = 6)"))+ 
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  scale_y_continuous(breaks = seq(0, 1.1, by = 0.1))+
  scale_linetype_manual(values = c("Raw" = "solid", "Fit" = "dashed")) +
  annotate("label",x = 80, y = max(all_avg_df$Intensity) * 1.1, label = lambda_text, color = "black",
           fill = "white", label.size = 0.5, size = 5, hjust = 0.3, vjust = 1)+
  theme_bw(base_rect_size = 0.5) 
p+ theme(legend.key.size = unit(0.9, "cm"), legend.text = element_text(size = 12),
         legend.title = element_text(size = 12, face = 'bold'),
         #legend.background = element_rect(fill = "white", color = "black"),
         panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
         panel.grid.major.x = element_line(linetype = "dashed"),
         axis.title = element_text(size = 14, face = 'bold'), 
         axis.text = element_text(size = 12, face = 'bold'),
         plot.title = element_text(face = "bold", size = 18))

ggsave(file.path(parent_folder, "Ore_avg_fits.png"), 
       plot = p, width = 9, height = 6, dpi = 300)

