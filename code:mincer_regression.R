##############################################
#        PART B – MINCER REGRESSION         #
#   Inequality project – CPS 1985 (USA)     #
#   Authors: arrya & shin-shin          #
##############################################

### 1. Load libraries ---------------------------------------

# Install once if needed:
# install.packages(c("dplyr", "ggplot2", "stargazer", "readr"))

library(dplyr)
library(ggplot2)
library(stargazer)
library(readr)

### 2. GINI COEFFICIENT FUNCTION ----------------------------

# Generic Gini function – works for any numeric vector (income, wage, etc.)
gini <- function(x) {
  # Remove missing or negative values
  x <- x[!is.na(x) & x >= 0]
  
  # Sort values
  x_sorted <- sort(x)
  n <- length(x_sorted)
  
  # Handle edge case: no or 1 observation
  if (n <= 1) return(NA_real_)
  
  # G = (2 * sum(i * x_i)) / (n * sum(x)) - (n + 1) / n
  index <- 1:n
  G <- (2 * sum(index * x_sorted)) / (n * sum(x_sorted)) - (n + 1) / n
  
  return(G)
}

### 3. Load CPS data ----------------------------------------

# Put your CPS file in the /data folder and check the EXACT filename.
# Example: "cps1985.csv" – adjust if your name is different.

cps <- read_csv("data/cps1985.csv")

# Quick checks – helps you verify variable names
head(cps)
str(cps)
summary(cps)

# TODO: check that the following columns exist and match your dataset:
# - wage (hourly wage)
# - age
# - schooling   (years of education)
# - gender      (e.g. "Male"/"Female")
# - race        (e.g. "White", "Black", "Other")
# - union       (e.g. "Union", "Non-union")
# - marital_status (e.g. "Married", "Single", etc.)
#
# If names differ, FIX THEM HERE (e.g. cps <- rename(cps, wage = AHE, ...))


### 4. Construct variables ----------------------------------

cps <- cps %>%
  # 4.1 Dependent variable: log(HOURLY wage)
  mutate(
    ln_wage = log(wage),
    
    # 4.2 Potential experience (Borjas: age - schooling - 6)
    experience  = age - schooling - 6,
    experience2 = experience^2,
    
    # 4.3 Gender dummy: 1 = female, 0 = male
    female = ifelse(gender == "Female", 1, 0),
    
    # 4.4 Race dummy: 1 = non-white, 0 = white
    nonwhite = ifelse(race != "White", 1, 0),
    
    # 4.5 Union dummy: 1 = union member, 0 = not
    union_member = ifelse(union == "Union", 1, 0),
    
    # 4.6 Marital dummy: 1 = married, 0 = not married
    married = ifelse(marital_status == "Married", 1, 0)
  )

# Optional: drop obvious problems (negative wages, crazy experience, etc.)
cps <- cps %>%
  filter(wage > 0, experience >= 0)


### 5. Descriptive stats & inequality (Gini) ----------------

# Basic summary of wages
summary(cps$wage)
summary(cps$ln_wage)

# Gini coefficient for wage distribution
wage_gini <- gini(cps$wage)
print(paste("Gini coefficient – hourly wage (CPS 1985):", round(wage_gini, 3)))

# Save Gini result to a text file in /output
if (!dir.exists("output")) dir.create("output")
writeLines(
  paste("Gini coefficient – hourly wage (CPS 1985):", round(wage_gini, 3)),
  "output/gini_wage.txt"
)


### 6. Basic plots (for slides) -----------------------------

# Wage vs schooling
ggplot(cps, aes(x = schooling, y = wage)) +
  geom_point(alpha = 0.2) +
  theme_minimal() +
  labs(
    title = "Hourly wage vs schooling (CPS 1985)",
    x = "Years of schooling",
    y = "Hourly wage"
  )

# Wage distribution: male vs female (optional)
ggplot(cps, aes(x = wage, fill = factor(female))) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Wage density by gender (CPS 1985)",
    x = "Hourly wage",
    fill = "Female (1 = yes)"
  )


### 7. Mincer regression ------------------------------------

# Full model with schooling, experience, gender, race, union, marital status
mincer_full <- lm(
  ln_wage ~ schooling + experience + experience2 +
    female + nonwhite + union_member + married,
  data = cps
)

# Console output
summary(mincer_full)

# Optional: try a simpler model for comparison
mincer_basic <- lm(
  ln_wage ~ schooling + experience + experience2,
  data = cps
)


### 8. Export regression table ------------------------------

# This creates a nice table in the console AND writes to /output
stargazer(
  mincer_basic, mincer_full,
  type  = "text",                           # "text" for console; use "html" or "latex" if needed
  title = "Mincer Earnings Functions – CPS 1985",
  column.labels = c("Basic", "Full"),
  dep.var.labels = "log(hourly wage)",
  out   = "output/mincer_regression_table.txt"
)

