# Generates a reproducible synthetic Student Performance dataset
set.seed(2026)
n <- 200
students <- data.frame(
  student_id   = sprintf("S%03d", 1:n),
  name         = paste("Student", 1:n),
  gender       = sample(c("Male","Female"), n, replace = TRUE),
  section      = sample(c("A","B","C"), n, replace = TRUE),
  attendance   = round(pmin(100, pmax(40, rnorm(n, 82, 10))), 1),
  study_hours  = round(pmin(8, pmax(0.5, rnorm(n, 3.5, 1.5))), 1)
)
noise <- rnorm(n, 0, 6)
students$math    <- round(pmin(100, pmax(0, 20 + 0.25*students$attendance + 6*students$study_hours + noise)), 0)
students$science <- round(pmin(100, pmax(0, 18 + 0.25*students$attendance + 5.5*students$study_hours + rnorm(n,0,6))), 0)
students$english <- round(pmin(100, pmax(0, 30 + 0.2*students$attendance + 4*students$study_hours + rnorm(n,0,7))), 0)
# messy version for Intermediate project: missing values + duplicates
messy <- students
messy$math[sample(n, 8)]        <- NA
messy$attendance[sample(n, 6)]  <- NA
messy$gender[sample(n, 4)]      <- NA
messy <- rbind(messy, messy[sample(n, 5), ])
write.csv(students, "data/students_clean.csv", row.names = FALSE)
write.csv(messy,    "data/students_raw.csv",   row.names = FALSE)
cat("clean rows:", nrow(students), " raw rows:", nrow(messy), "\n")
