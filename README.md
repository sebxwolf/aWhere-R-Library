# aWhere API R Package

For customers that use R in their statistical modeling work, this R package helps get you up and running with our APIs with minimal integration effort. There is a function for each of our most regularly used APIs.

_Note: These functions will only return metric units_


## Installation

**Note:** Prior to installation you will need a working installation of R.

**Note:** You will need to have the devtools library installed in R. Consult documentation if necessary.

### Automatically Install

This package can be installed directly from GitHub with the following command:

    devtools::install_github("aWhereAPI/aWhere-R-Library")

#### If you have problems, try
On Windows
    install.packages(c('chron', 'magrittr', 'DBI', 'assertthat', 'bitops', 'Rcpp', 'tibble', 'jsonlite', 'data.table', 'httr', 'lubridate', 'RCurl'))

On CentOS 7
yum install openssl-devel libcurl-devel -y
Rscript --slave --no-save --no-restore-history -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
Rscript --slave --no-save --no-restore-history -e "devtools::install_github('aWhereAPI/aWhere-R-Library')"

### Manually Install

1. Download this Github repo and keep the `aWhere-R-Library-master` folder somewhere useful on your computer, such as your Desktop or My Documents.

2. Set the Working Path in R to the location that contains the `aWhere-R-Library-master` folder. If you placed it on your Desktop the working directory would be something like `C:\Users\USERNAME\Desktop`. In R, this command is:
	* `setwd("C:\Users\USERNAME\Desktop")`

3. Run the following set of commands to install and add the library to your environment:
	* `library(devtools)`
	* `install("aWhere-R-Library-master")`
	* `library(aWhereAPI)`



## Documentation

Because customers using R have many different use cases, we've organized the documentation accordingly in order to present only the most useful set of functions for your work. You will find all the documentation in the `documentation` folder.

[complete-documentation.md](https://github.com/aWhereAPI/aWhere-R-Library/blob/master/documentation/complete-documentation.md)
* This file includes every function.

[geolocation-usage.md](https://github.com/aWhereAPI/aWhere-R-Library/blob/master/documentation/geolocation-usage.md)
* Some customers will prefer to use latitude and longitude for selecting weather data. This is generally only appropriate and allowed when doing large regional work or for discrete point-by-point data needs.
* This approach is *not* appropriate when tracking data for a field or if not explicitly allowed by your agreement with aWhere.

[field-based-usage.md](https://github.com/aWhereAPI/aWhere-R-Library/blob/master/documentation/field-based-usage.md)
* This is the primary method for accessing the aWhere platform, and with this approach you register Field Locations with the platform and can add planting data to each field to more easily monitor agronomics and track models.
* If your work is aware of specific field locations and tracking specific growing seasons, this is the appropriate approach to use.
