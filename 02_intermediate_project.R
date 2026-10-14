# INTERMEDIATE PROJECT: Student Performance Analysis (dplyr + ggplot2)
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(tidyr)})
raw <- read.csv("data/students_raw.csv", stringsAsFactors = FALSE)
cat("Raw rows:", nrow(raw), "| duplicates:", sum(duplicated(raw)), "\n")
cat("Missing per column:\n"); print(colSums(is.na(raw)))

# --- Clean ---
clean <- raw %>%
  distinct() %>%
  mutate(gender     = ifelse(is.na(gender), "Unknown", gender),
         math       = ifelse(is.na(math), median(math, na.rm = TRUE), math),
         attendance = ifelse(is.na(attendance), median(attendance, na.rm = TRUE), attendance),
         average    = round((math + science + english) / 3, 1),
         attendance_band = cut(attendance, c(0, 75, 90, 100),
                               labels = c("Low (<=75)", "Medium (76-90)", "High (>90)")))
cat("\nClean rows:", nrow(clean), "| remaining NAs:", sum(is.na(clean)), "\n")
stopifnot(nrow(clean) == 200, sum(is.na(clean)) == 0)

# --- Analyse ---
cat("\nAverage by section:\n")
print(clean %>% group_by(section) %>% summarise(n = n(), avg = round(mean(average),1), att = round(mean(attendance),1)))
cat("\nAverage by attendance band:\n")
print(clean %>% group_by(attendance_band) %>% summarise(n = n(), avg = round(mean(average),1)))
cat("\nTop 5 students:\n"); print(clean %>% arrange(desc(average)) %>% select(student_id, section, attendance, average) %>% head(5))
r <- cor(clean$attendance, clean$average); cat("\nCorr(attendance, average):", round(r, 3), "\n")
cat("Corr(study_hours, average):", round(cor(clean$study_hours, clean$average), 3), "\n")
tt <- t.test(average ~ gender, data = filter(clean, gender != "Unknown"))
cat("t-test (gender) p-value:", signif(tt$p.value, 3), "\n")
print(summary(aov(average ~ section, data = clean)))

# --- Visualise ---
p1 <- ggplot(clean, aes(average)) + geom_histogram(binwidth = 5, fill = "steelblue", colour = "white") +
  labs(title = "Distribution of Average Marks", x = "Average mark", y = "Students")
p2 <- ggplot(clean, aes(attendance, average)) + geom_point(alpha = .6) + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Attendance vs Average Marks", x = "Attendance (%)", y = "Average mark")
p3 <- ggplot(clean, aes(section, average, fill = section)) + geom_boxplot(show.legend = FALSE) +
  labs(title = "Average Marks by Section", x = "Section", y = "Average mark")
ggsave("output/int_hist.png", p1, width = 6, height = 4, dpi = 120)
ggsave("output/int_scatter.png", p2, width = 6, height = 4, dpi = 120)
ggsave("output/int_box.png", p3, width = 6, height = 4, dpi = 120)
write.csv(clean, "data/students_cleaned.csv", row.names = FALSE)
cat("\nSaved 3 charts + cleaned CSV.\n")
