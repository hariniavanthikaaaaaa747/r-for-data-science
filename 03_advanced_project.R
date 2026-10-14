# ADVANCED PROJECT: End-to-End Student Performance Prediction (regression + ML)
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(rpart); library(randomForest); library(cluster)})
set.seed(42)
d <- read.csv("data/students_cleaned.csv", stringsAsFactors = TRUE)
d <- d %>% mutate(pass = factor(ifelse(average >= 60, "Pass", "Fail"), levels = c("Fail","Pass")))
cat("Rows:", nrow(d), "\nPass/Fail:\n"); print(table(d$pass))

# --- Train/test split 80/20 ---
idx   <- sample(seq_len(nrow(d)), size = 0.8 * nrow(d))
train <- d[idx, ]; test <- d[-idx, ]
cat("Train:", nrow(train), "Test:", nrow(test), "\n")

# --- Linear regression ---
lm_fit <- lm(average ~ attendance + study_hours, data = train)
print(summary(lm_fit))
pred <- predict(lm_fit, test)
rmse <- sqrt(mean((test$average - pred)^2)); mae <- mean(abs(test$average - pred))
r2   <- 1 - sum((test$average - pred)^2) / sum((test$average - mean(test$average))^2)
cat(sprintf("\nTest RMSE: %.2f | MAE: %.2f | R-squared: %.3f\n", rmse, mae, r2))

# --- Classification: decision tree vs random forest ---
tree <- rpart(pass ~ attendance + study_hours + section, data = train, method = "class")
rf   <- randomForest(pass ~ attendance + study_hours + section, data = train, ntree = 200)
acc  <- function(m, type) { p <- predict(m, test, type = type); mean(p == test$pass) }
cat(sprintf("\nDecision tree accuracy: %.3f\nRandom forest accuracy: %.3f\n", acc(tree, "class"), acc(rf, "class")))
cat("Random forest confusion matrix (rows = actual):\n")
print(table(Actual = test$pass, Predicted = predict(rf, test)))
cat("\nVariable importance:\n"); print(round(importance(rf), 2))

# --- Clustering ---
km_data <- scale(d[, c("attendance", "study_hours", "average")])
km <- kmeans(km_data, centers = 3, nstart = 25)
d$cluster <- factor(km$cluster)
cat("\nCluster profiles (original units):\n")
print(d %>% group_by(cluster) %>% summarise(n = n(), attendance = round(mean(attendance),1),
      study_hours = round(mean(study_hours),1), average = round(mean(average),1)))
sil <- mean(silhouette(km$cluster, dist(km_data))[, 3]); cat("Mean silhouette:", round(sil, 3), "\n")

# --- Charts ---
p1 <- ggplot(data.frame(Actual = test$average, Predicted = pred), aes(Actual, Predicted)) +
  geom_point(alpha = .7) + geom_abline(linetype = "dashed", colour = "red") +
  labs(title = "Linear Model: Actual vs Predicted Average")
p2 <- ggplot(d, aes(attendance, average, colour = cluster)) + geom_point(alpha = .7) +
  labs(title = "Student Clusters (k-means, k = 3)", x = "Attendance (%)", y = "Average mark")
ggsave("output/adv_actual_vs_pred.png", p1, width = 6, height = 4, dpi = 120)
ggsave("output/adv_clusters.png", p2, width = 6, height = 4, dpi = 120)
cat("\nSaved 2 charts.\n")
