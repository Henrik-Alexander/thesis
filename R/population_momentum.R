### Population momentum

# Load Demotools
# install.packages("remotes)
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
remotes::install_github("timriffe/DemoTools")

# Load the web-scraping
source("kapitel8/wpp_scraping.R")

# Load packages
library(DemoTools)

# Load the data ---------------------------

## Input data:
# N_a: Number of women aged a to a+5
# L_a: person-years lived between ages a and a+5 in the actual female life table with a radix of unity
# N_f: total number of female in the population
# N_m: total number of males in the population
# e_m: female life expectancy at birth
# e_f: male life expectancy at birth
# m_x: maternity rates
# nrr: net reproduction rate


# Load the population data: N_f and N_m
pop <- load_time_series(country="World", indicator="Population by 5-year age groups and sex", start=2022, end=2022) 
N_f <- sum(pop$value[pop$sex=="Female"])
N_m <- sum(pop$value[pop$sex=="Male"])

# Number of women aged a to a+5: N_a
N_a <- pop[pop$sex=="Female", c("ageStart", "ageEnd", "value")]

# Get the life expectancies: e_f and e_m
e0 <- load_time_series(country="World", indicator="Life expectancy at birth", start=2022, end=2022)
e_m <- e0$value[e0$sex=="Male"]
e_f <- e0$value[e0$sex=="Female"]

# Get the exposures
mx <- load_time_series(country="World", indicator="Age specific mortality rate m(x,n) - abridged", start=2022, end=2022)
mx_f <- mx[mx$sex=="Female", c("ageStart", "ageEnd", "value")]
lt <- lt_abridged(nMx=mx_f$value, Age=mx_f$ageStart, sex="f", mod=F, axmethod= "un", radix=1)
lt$ageEnd <- lt$Age + lt$AgeInt -1

# Get the fertility rates
m_x <- load_time_series(country="World", indicator="Fertility rates by age of mother (5-year)", start=2022, end=2022)
m_x$value <- m_x$value / 1000
m_x <- m_x[, c("ageStart", "ageEnd", "indicator", "value", "ageMid")]

# Keyfitz's population moementum --------------------------------

## 2. Estimate the replacement-level fertiltiy rates for the world

# Combine life table and fertility rates
m_ax <- merge(lt, m_x, by.x = c("Age", "ageEnd"), by.y = c("ageStart", "ageEnd"), all.x = T)

# Estimate the net reproduction rate
nrr <- sum(m_ax$value * m_ax$nLx, na.rm = T)

# Rescale the fertility rates
m_ax$rep_fert <- m_ax$value / nrr

## 3. Estimation of the ultimate number of females births in the stationary population

# Estimate the mean age in the stationary population
A <- with(m_ax, sum(ageMid * rep_fert * nLx, na.rm = T))

# 
tmp_res <- numeric()
for (i in 1:nrow(m_ax)) {
  len <- nrow(m_ax)
  tmp <- sum(m_ax$nLx[(i+1):len] * m_ax$rep_fert[i:len], na.rm = T)
  tmp_res <- c(tmp_res, ifelse(is.na(tmp), 0, tmp))
}
wa <- (m_ax$nLx / 2 * m_ax$rep_fert + tmp_res) / A

# Births
nLx <- with(m_ax, c(nLx[1] + nLx[2], nLx[3:length(nLx)]))
B <- sum(N_a / (nLx/5) * wa[2:length(wa)], na.rm = T)

## 4. Estimation of ultimate population and momentum ----------------

# Number of females in the ulimate population
N_sf <- B * e_f
N_sm <- B * e_m

# Estimate the population momentum
(M = (N_sf + N_sm) / (N_f + N_m))



### END #############################################################