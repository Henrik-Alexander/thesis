######################################
# Purpose: Cohort component method
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mgp.de
# Date: 08.08.2024
#####################################



# Data preparation -----------------------

rm(list = ls())

# Load the web-scraping
source("R/wpp_scraping.R")

# Load the DemoTools package
# install.packages("remotes)
#install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#remotes::install_github("timriffe/DemoTools")
library(DemoTools)

# Define the projection horizon
end_year <- 2023
start_year <- 2023
country <- "France"

## Functions -------------------------------------------------------

# Function to extract lower age
lower_age <- function(agegroup) as.numeric(str_extract(agegroup, "^[0-9]+"))
upper_age <- function(agegroup){
  ifelse(agegroup == "100+", Inf,  as.numeric(str_extract(agegroup, "[0-9]+$")))
 
}

# Clean the life table data
clean_lt <- function(lt) {
  lt$ageStart <- lt$Age
  lt$ageEnd <- lt$ageStart + lt$AgeInt - 1
  lt$ageLabel <- ifelse(lt$ageStart < 5, "0-4", 
                        ifelse(is.na(lt$ageEnd), "100+", paste0(lt$ageStart, "-", lt$ageEnd)))
  
  Lx <- tapply(lt$nLx, lt$ageLabel, sum)
  data.frame("ageLabel"=unique(lt$ageLabel),
             "nLx"=Lx[unique(lt$ageLabel)],
             "Tx"=rev(cumsum(rev(Lx[unique(lt$ageLabel)]))),
             "startAge"=lower_age(unique(lt$ageLabel)),
             "endAge"=upper_age(unique(lt$ageLabel)))
}

# Clean data
clean_data <- function(wpp_data) {
  wpp_data$year <- as.numeric(wpp_data$timeLabel)
  wpp_data <- wpp_data[wpp_data$variantLabel == "Median", ]
  return(wpp_data[, c("location", "sex", "ageLabel", "ageStart", "ageEnd", "value")])
}

# Function to create df
create_df <- function(lt, fx, pop) {
  
  # Combine fertility and mortality
  df <- merge(lt, fx, all.x = T)
  df <- df[order(df$startAge), ]
  
  # Combine the data
  data.frame("ageLabel"=df$ageLabel,
             "value_pop"=pop,
             "value_fx"=df$value,
             "nLx"=df$nLx,
             "Tx"=df$Tx,
             "fx"=df$value)
}


# Cohort component method
project <- function(pop_f, pop_m, age=rownames(projection_f), mx_m, mx_f, fx, srb=1.05) {
  
  # Get the length of the data
  n <- length(age)
  
  # Clean the life table
  lt_f <- lt_abridged(nMx=mx_f$value, Age=mx_f$ageStart, sex="f", mod=F, axmethod= "un", radix=1)
  lt_m <- lt_abridged(nMx=mx_m$value, Age=mx_f$ageStart, sex="m", mod=F, axmethod= "un", radix=1)
  lt_f <- clean_lt(lt_f)
  lt_m <- clean_lt(lt_m)
  
  # Create df
  df <- create_df(lt_f, fx, pop_f)
  
  # Estimate the population in 5 years
  pop_f_t <- df$value_pop * df$nLx[2:n] / df$nLx
  pop_f_t[n] <- (df$value_pop[n] + df$value_pop[n-1]) * (df$Tx[n] / df$Tx[n-1])
  
  # Estimate the births
  Births <- 5 * df$value_fx * (df$value_pop + df$value_pop[2:n])/2
  B <- sum(Births, na.rm = T)
  B_f <- B * 1 / (1 + srb)
  
  # Create the first age category
  pop_f_t[1] <- B_f * (df$nLx[1]) / (5*1)

  # Create df for men
  df <- create_df(lt_m, fx, pop_m)
  
  # Estimate the population in 5 years
  pop_m_t <- df$value_pop * df$nLx[2:n] / df$nLx
  pop_m_t[n] <- (df$value_pop[n] + df$value_pop[n-1]) * (df$Tx[n] / df$Tx[n-1])
  
  # Assign the births
  B_m <- B * srb / (1 + srb)
  pop_m_t[1] <- B_m * (df$nLx[1]) / (5*1)

  # Combine the data
  data.frame("Age"=df$ageLabel, "Females"=pop_m_t, "Males"=pop_f_t)
  
}

# Cohort component prjection
cohort_component_method <- function(country, start_year, horizon, momentum=F, srb=1.05) {
  
  # Range of indicators
  pop <- load_time_series(country=country, indicator="Population by 5-year age groups and sex", start=start_year, end=start_year) 
  mx <- load_time_series(country=country, indicator="Age specific mortality rate m(x,n) - abridged", start=start_year, end=start_year)
  fx <- load_time_series(country=country, indicator="Fertility rates by age of mother (5-year)", start=start_year, end=start_year)
  
  # Clean the data
  pop <- clean_data(pop)
  mx <- clean_data(mx)
  fx <- clean_data(fx)
  
  # Clean the population data
  pop_f <- pop[pop$sex=="Female", ]
  pop_m <- pop[pop$sex=="Male", ]
  
  # Combine the first two age groups of the mortality table
  mx_f <- mx[mx$sex=="Female", c("ageStart", "ageEnd", "value")]
  mx_m <- mx[mx$sex=="Male", c("ageStart", "ageEnd", "value")]
  lt_f <- lt_abridged(nMx=mx_f$value, Age=mx_f$ageStart, sex="f", mod=F, axmethod= "un", radix=1)
  lt_m <- lt_abridged(nMx=mx_m$value, Age=mx_f$ageStart, sex="m", mod=F, axmethod= "un", radix=1)
  lt_f <- clean_lt(lt_f)
  lt_m <- clean_lt(lt_m)
  
  # Clean the fertility data
  fx$value <- fx$value / 1000
  
  # Population momentum
  if (momentum) {
    df <- create_df(lt_f, fx, pop_m)
    nrr <- sum(1 / (1 + srb) * df$nLx * df$fx, na.rm = T)
    fx$value <- fx$value / nrr
  }
  
  # Create the containers
  projection_m <- projection_f <- matrix(NA, nrow=nrow(pop_f), ncol=horizon+1)
  colnames(projection_m) <- colnames(projection_f) <- start_year:(start_year + horizon)
  rownames(projection_m) <- rownames(projection_f) <- pop_m$ageLabel
  
  # Assign the jump-off population
  projection_m[, 1] <- pop_m$value
  projection_f[, 1] <- pop_f$value
  
  
  # Project the data
  for (i in 1:horizon) {
    cat("Year:", i + start_year, "\n")
    projection <- project(projection_f[, i], projection_m[, i], mx_m = mx_m, mx_f = mx_f, fx=fx, srb = srb)
    projection_m[, i+1] <- projection$Males
    projection_f[, i+1] <- projection$Females
  }
  
  # Create the results
  projection <- list("males" = projection_m,
                     "females" = projection_f)
  
  # Return the results
  return(projection)
  
}

# Estimate the trend in the data
compare <- function(observed, counterfactual, f = sum, label = "Population size") {
  # Combine male and female data
  observed <- observed$males + observed$females
  counterfactual <- counterfactual$females + counterfactual$males
  
  # Apply the function
  observed <- apply(observed, 2, FUN = f)
  counterfactual <- apply(counterfactual, 2, FUN = f)
  
  # Adjust downwards
  if (max(observed) > 1e+6) {
    observed <- observed / 1e+6
    counterfactual <- counterfactual / 1e+6
    label <- paste(label, "(m)")
  }
  # Plot the result
  henrik_plot(xlim=range(as.numeric(names(observed))), ylim=range(c(observed, counterfactual)),
              xlab = "Year", ylab = label)
  lines(x=as.numeric(names(observed)), y=observed)
  lines(x=as.numeric(names(counterfactual)), y=counterfactual, lty=2)
}


# Cohort component method ------------------------------------------

# World
un_observed <- cohort_component_method("World", 2023, 100)
un_counter <- cohort_component_method("World", 2023, 100, momentum=T)

# Cohort component projection
us_observed <- cohort_component_method("United States of America", 2023, 100)
us_counter <- cohort_component_method("United States of America", 2023, 100, momentum=T)

# Germany
de_observed <- cohort_component_method("Germany", 2023, 100)
de_counter <- cohort_component_method("Germany", 2023, 100, momentum=T)

# Finland
fi_observed <- cohort_component_method("Finland", 2023, 100)
fi_counter <- cohort_component_method("Finland", 2023, 100, momentum=T)

# Make the comparisons
compare(un_observed, un_counter)

## Projection function ----------------------------------------------


# Plot the 


## Plot the data ----------------------------------------------------

# Make age-specific plot
age_specific_plot <- function(data, label) {
  henrik_plot(range(data$year), range(data$value), xlab="Year", ylab=label, title = label)
  for(agegroup in unique(data$ageLabel)) {
    col <- colours[which(agegroup==unique(data$ageLabel))]
    tmp <- data[data$ageLabel == agegroup, ]
    lines(tmp$year, y=tmp$value, col = col)
  }
  legend(x = quantile(data$year)[2], y=quantile(data$value)[3], legend=unique(data$ageLabel), lty = 1, col=colours[1:(length(unique(data$ageLabel)))])
}


# Plot the data
age_specific_plot(pop, "Population")

# Age specific plot
age_specific_plot(mx, "Death rates")

### END ##########################
