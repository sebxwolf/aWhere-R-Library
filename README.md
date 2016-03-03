# aWhere API R Package

For customers that use R in their statistical modeling work, this R package helps get you up and running with our APIs with minimal integration effort. There is a function for each of our most regularly used APIs. 

## Installation 

1. Prior to installation you will need a working installation of R. 

2. Download or clone this Github repo and keep the `aWhere-API-R-Package` somewhere useful on your computer, such as your Desktop or My Documents.

3. Set the Working Path in R to the location that contains the `aWhere-API-R-Package` folder. If you placed it on your Desktop the working directory would be something like `C:\Users\USERNAME\Desktop`. In R, this command is:  

    setwd("C:\Users\USERNAME\Desktop")

4. Run the following set of commands to install and add the library to your environment:

    library(devtools)
    install("aWhere-API-R-Package")
	library(aWhereAPI)

*Note: You will need to have the devtools library installed in R. Consult documentation if necessary.*

