# BEGINNER PROJECT: First Student Performance Analysis (base R only)
student_id <- c("S001","S002","S003","S004","S005","S006","S007","S008","S009","S010")
name   <- c("Asha","Ravi","Meena","Karthik","Divya","Arun","Sneha","Vijay","Priya","Kiran")
math   <- c(78, 85, 92, 66, 74, 58, 88, 95, 70, 81)
science<- c(82, 79, 90, 70, 68, 62, 91, 93, 75, 84)
english<- c(75, 88, 85, 72, 80, 65, 90, 89, 78, 83)
attendance <- c(88, 92, 96, 75, 82, 68, 94, 98, 80, 90)

students <- data.frame(student_id, name, math, science, english, attendance,
                       stringsAsFactors = FALSE)
students$total   <- students$math + students$science + students$english
students$average <- round(students$total / 3, 2)
students$grade   <- ifelse(students$average >= 85, "A",
                    ifelse(students$average >= 70, "B",
                    ifelse(students$average >= 60, "C", "D")))
print(students)
cat("\n--- Summary statistics ---\n")
print(summary(students[, c("math","science","english","attendance","average")]))
cat("\nMean math:", mean(students$math), " Median math:", median(students$math),
    " SD math:", round(sd(students$math), 2), "\n")
cat("\nGrade distribution:\n"); print(table(students$grade))
top <- students[which.max(students$average), ]
cat("\nTop performer:", top$name, "with average", top$average, "\n")
cat("Correlation (attendance, average):", round(cor(students$attendance, students$average), 3), "\n")
# Verification
stopifnot(nrow(students) == 10, all(students$total == rowSums(students[, c("math","science","english")])))
cat("\nVerification passed: 10 rows, totals match.\n")
