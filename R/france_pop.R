### Load all the functions
lapply(list.files("R", full.names = T), source)

# Load the packages
library(janitor)

### Functions ------------------

# Clean the result
clean_pop_data <- function(df) {
  df <- df[, c("indicator", "variant", "ageStart", "ageEnd", "sex" , "value")]
  df <- df[df$variant == "Median" & df$ageStart >= 15 & df$ageEnd <= 55, ]
  df <- df[df$sex %in% c("Male", "Female")]
  df <- clean_names(df)
  df <- na.omit(df)
  return(df)
}

# Function to estimate the sex ratio
estimate_asr <- function(df) {
  total <- tapply(df$value, df$sex, sum, simplify = F)
  return(total$Male / total$Female)
}


# Function to estimate the age ratio
estimate_age_ratio <- function(df) {
  # Estimate mid age
  df$mid_age <- 0.5*df$age_end + 0.5*df$age_start
  # Split the data
  df_split <- split(df, df$sex)
  # Function to estimate the age ratio
  age_ratio <- function(age, count) {
    sum(age * count) / sum(count)
  }

  # Estimate the age ratio
  res <- lapply(df_split, function(x) age_ratio(x$mid_age, x$value))

  # Return the age ratio
  return(res$Female / res$Male)
}

### Data wrangling -------------

# Set the parameters
country <- "France"
years <- 1950:2022

# Find the value series
indicator <- browse_indicators("population")
indicator <- indicator[str_detect(indicator$name, "Population by 5-year"), ]
indicator <- indicator$name

# Load the series
pop5 <- vector("list", length = length(years))
names(pop5) <- years
for (year in years) {
  cat("Year:", year, "\n")
  pop5[[paste(year)]] <- load_time_series(country, indicator, year, year)
}

# Clean the data
pop5 <- lapply(pop5, clean_pop_data)

# Estimate the results
adult_sex_ratio <- sapply(pop5, estimate_asr)
adult_age_ratio <- sapply(pop5, estimate_age_ratio)


# Plot the result
pdf("figures/pop_imbalance_fra.pdf", width = 10, height = 7)
henrik_plot(xlim = range(years), ylim = c(0.9, 1.1),
            title = "Population trend in France",
            xlab = "Year", ylab = "Sex ratio")
abline(h = 1, col = "grey")
lines(years, adult_sex_ratio, lwd = 3)
lines(years, adult_age_ratio, lwd = 3, lty = 2)
#text(2019, 0.97, expression(frac(M[15-55],F[15-55])))
#text(2019, 1.03, expression(frac(f[y] %*% y, f[x] %*% x)))
legend("topright", inset = 0.05,
       lwd = 3,
       lty = c(1, 3),
       legend = c(expression(frac(M[15-55],F[15-55])),
                  expression(frac(f[y] %*% y, f[x] %*% x))),
       box.col = "white")
dev.off()

### END ####################
