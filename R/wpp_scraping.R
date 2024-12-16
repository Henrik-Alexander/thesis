# Interact with World Population Prospects


# Load the packages
library(jsonlite)
library(httr)
library(stringr)
library(RCurl)

# Set the path to the package
path_wpp <- "U:/packages/wpp/R"

# Load the graphics
source(file.path(path_wpp, "graphics.R"))

# Authentification
source(file.path(path_wpp, "authentification.R"))

# Declares the base url for calling the API
url_unpd <- "https://population.un.org/dataportalapi/api/v1"


# Functions ---------------------------------

# Function to load attributes: either locations or indicators
load_undp_meta <- function(attribute = "locations") {
  if (!(attribute %in% c("locations", "indicators"))) {
    stop("Must be either locations or indicators")
  } else {
  # Set the path
  url_unpd <- "https://population.un.org/dataportalapi/api/v1"

  # Define the target url
  target <- paste0(url_unpd, "/", attribute, "/")

  # Call the API
  response <- fromJSON(target)

  # Get the data
  df <- response$data

  # Get the other pages with data
  while (!is.null(response$nextPage)){
    response <- fromJSON(response$nextPage)
    df <- rbind(df, response$data)
  }
    return(df)
  }
}

# Function to translate country into country number
country_to_id <- function(countryname) {
  if(!exists("locations")) locations <- load_undp_meta("locations")
  if (!(countryname %in% locations$name)) {
    stop("Country name is not known!")
  } else {
    return(locations$id[locations$name == countryname])
  }
}

# Function to create indicattors
indicator_to_id <- function(indicator){
  if(!exists("indicators")) indicators <- load_undp_meta("indicators")
  if (!(indicator %in% indicators$name)) {
    stop("Indicator name is not known!")
  } else {
    return(indicators$id[indicators$name == indicator])
  }
}

# Get the meta data: sex, ages
get_meta <- function(indicator="Net Reproduction Rate", category) {
  if(!(category %in% c("ages", "sexes"))) stop("Category must be either ages, sexes, categories or variants!")
  if(!exists("indicators")) indicators <- load_undp_meta("indicators")
  if (!(indicator %in% indicators$name)) {
    stop("Indicator name is not known!")
  } else {
    indicator_id <- indicators$id[indicators$name == indicator]
    target <- paste(url_unpd, "metadata", category,  indicator_id, sep = "/")
    response <- fromJSON(target)
    return(response)
  }
}

# Function to browse indicators
browse_indicators <- function(searchterm) {
  if(!exists("indicators")) indicators <- load_undp_meta("indicators")
  tmp <- indicators[str_detect(indicators$description, searchterm), ]
  return(tmp[, c("id", "name", "description", "sourceYear", "sourceStartYear", "sourceEndYear", "sourceCitation")])
}

# Load time-series data for one country and one indicator
load_time_series <- function(country="Germany", indicator="Net Reproduction Rate", start=2019, end=2019) {
  # Check the existence
  if(!exists("indicators")) indicators <- load_undp_meta("indicators")
  if(!exists("locations")) locations <- load_undp_meta("locations")
  # Check if the country and indicator exists
  if (!(country %in% locations$name)) {
    warning("Country does not exist!")
    return(NULL)
  } else if (!(indicator %in% indicators$name)) {
    warning("Indicator does not exist!")
    return(NULL)
  } else {
    # Create the search ids
    country_id <- country_to_id(country)
    indicator_id <- indicator_to_id(indicator)

    # Paste the query together
    target <- paste(url_unpd, "data/indicators",
                     indicator_id, "locations", country_id,
                     "start", start, "end", end, sep = "/")
    print(target)

    # Call the API
    res <- getURL(target, .opts=list(httpheader = headers, followlocation = TRUE))
    res <- fromJSON(res)

    # Get the first page of data
    df <- res$data

    # Get the other pages with data
    while (!is.null(res$nextPage)){

      res <- fromJSON(res$nextPage)
      df <- rbind(df, res$data)

    }
    return(df)
  }
}

# Function to create a time-series plot
wpp_plot <- function(wpp_data) {
  years <- as.numeric(wpp_data$timeLabel)
  countries <- wpp_data$location
  values <- wpp_data$value
  henrik_plot(xlim=range(years), ylim=range(values),
              xlab="Year", ylab=unique(wpp_data$indicator),
              title=unique(wpp_data$indicatorDisplayName))
  for (country in unique(countries)) {
    col <- colours[which(country==unique(countries))]
    lines(x=years[countries==country], y=values[countries==country], col=col)
    text(x=max(years), y=values[countries==country&years==max(years)], label=country, xpd=T, pos=2, col=col)
    #mtext(text=country, at=values[countries==country&years==max(years)]) #, las=1
  }
}

### Load data --------------------------------------



run_examples <- function() {
  
  # Load the meta data
  indicators <- load_undp_meta("indicators")
  locations <- load_undp_meta("locations")
  
  # Get the number for Germany
  country_to_id("Germany")
  
  # Get fertiltiy information
  browse_indicators("fertility")
  browse_indicators("pop")
  
  # Indicator to id
  indicator_to_id("Net Reproduction Rate")
  
  #
  nrr_ger <- load_time_series(start=1950, end=2020)
  nrr_usa <- load_time_series("United States of America", start=1950, end=2020)
  
  # Plot the data
  wpp_plot(wpp_data=rbind(nrr_ger, nrr_usa))
  
  
  # Save the nrr data
  save(nrr_ger, file = "kapitel8/nrr_germany.Rda")
  
}





### END ##########################