# aWhere API R Package for Geolocation-Based Use Cases

Some customers will prefer to use latitude and longitude for selecting weather data. This is generally only appropriate and allowed when doing large regional work or for discrete point-by-point data needs. This approach is *not* appropriate when tracking data for a field or if not explicitly allowed by your agreement with aWhere. 

_Note: These functions will only return metric units_




## Usage

In order to use any of the functions in this package, or the APIs in general, you will need API credentials provided by aWhere. These keys are used to generate an Access Token, which is then passed to each API to authenticate the request. This R package will handle token generation and usage automatically, but you will need to get API keys first. 

To get API keys, follow these instructions at the [aWhere Developer Community](http://developer.awhere.com/start). Note that normally the API Key and Secret are made up of random characters. 


### Using Credentials Directly

Prior to using of the any other functions, you'll need to use `GetAccessToken()` function to generate a token. Pass the API Key and API Secret from aWhere as the two parameters to this function. For example: 

    GetAccessToken("JFKL24JF290FJSKAFDF","jf0afsd9af0a"); 

You could also save the key and secret as variables. This function saves the token to a direct child of the base environment for use by any other aWhere function. 

### Loading Credentials from a Saved File

An alternative approach is to save your credentials to an external text file and use the `loadCredentials()` function to load them into the environment and generate a token. 

First, create a text file on your computer. Enter your API Key on the first line, and your Secret on the second. Save the file and note the complete path to it. For example, this might be `C:\Users\USERNAME\Desktop\credentials.txt`. 

In R, supply the path to the credentials file to the function: 

    loadCredentials("C:\Users\USERNAME\Desktop\credentials.txt")

This function invokes `GetAccessToken()` so no further work is needed to generate an Access Token.



## API Functions Available 

**Contents**

* Weather 
	* Observations (Daily History)
	* Forecast
	* Norms 
	* Current Conditions
* Agronomics
	* Agronomic Values (Daily History)
	* Norms



___________________________________________________________________________
---------------------------------------------------------------------------

### Weather Data

The aWhere Platform provides agriculturally relevant weather data globally. 

All weather data requests are requested point by point. There is not a way to download regional data in a single API call. Use the [batch jobs system](http://developer.awhere.com/api/reference/batch) when requesting a large amount of data (e.g., many hundreds or thousands of points). 

Learn more [about the weather data](http://developer.awhere.com/api/about-our-data) we provide and our [Weather Data APIs](http://developer.awhere.com/api/reference/weather) at the aWhere Developer Community. 



- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#### Observations (Daily History)

This function uses the Observations API to retrieve observed weather data for any given day. By default, you can request daily data from up to 30 months ago. For data beyond that, use the Norms API (function below) to calculate long-term norms. Or, speak with your aWhere representative for access to more of the daily data archive. [API Documentation](http://developer.awhere.com/api/reference/weather/observations)

##### Function Signatures

    GetDailyObservedWeatherLatLon(latitude, longitude, dayStart, dayEnd)


##### Parameters

* `latitude` and `longitude` _(numerics)_
	* The geolocation for which you want weather data
	* Required 
* `dayStart` _(character)_
	* The starting date of a range of dates for which you want weather data 
	* Alternatively, if you only want data for a single date, enter that date here 
	* If not supplied, the API defaults to returning the last week of data
	* Format is YYYY-MM-DD and the date must fall before today
	* Optional
* `dayEnd` _(character)_
	* The ending date of a range of dates for which you want weather data. 
	* If not supplied, but a `dayStart` is, then the API returns data only for a single day
	* Format is YYYY-MM-DD and the date must fall before today and after `dayStart`
	* Optional
	
##### Examples

    GetDailyObservedWeatherLatLon('39.8282', '-98.5795', '2015-07-01','2015-07-14')


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#### Forecast

Retrieve the forecast for today plus up to the next 8 days with this function. The forecast is available at multiple temporal resolutions - in 1, 2, 3, 4, 6, 8, 12, or 24-hour summary blocks. [API Documentation](http://developer.awhere.com/api/forecast-weather-api)

##### Function Signatures

    GetForecastsLatLon(latitude, longitude, dayStart, dayEnd, blockSize)

##### Parameters

* `latitude` and `longitude` _(numerics)_
	* The geolocation for which you want weather data
	* Required 
* `dayStart` _(character)_
	* The starting date of a range of dates for which you want forecast data 
	* Alternatively, if you only want data for a single date, enter that date here 
	* If not supplied, the API defaults to today and the next seven days of data
	* Format is YYYY-MM-DD and the date must be equal to today or any of the next seven days
	* Optional
* `dayEnd` _(character)_
	* The ending date of a range of dates for which you want weather data. 
	* If not supplied, but a `dayStart` is, then the API returns data only for a single day
	* Format is YYYY-MM-DD and the date must fall after today and after `dayStart`
	* Optional
* `blockSize` _(numeric)_
	* The number of forecast hours to roll into a single forecast block 
	* Options are 1, 2, 3, 4, 6, 8, 12, and 24
	* Default is 1 (hourly forecast)
	* Optional
	

##### Examples

    GetForecastsLatLon('39.8282', '-98.5795', '2016-02-29','2016-03-02', 24)


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#### Long Term Norms 

The Weather Norms API allows you to offload the calculation of multi-year averages for any days of the year across any number of years for which we have data (minimum 3). This eliminates the need, most of the time, to download years and years of daily data just to calculate averages. The results also include the standard deviation for each average. [API Documentation](http://developer.awhere.com/api/reference/weather/norms)

##### Function Signatures 

    GetWeatherNormsLatLon(latitude, longitude, monthDayStart, monthDayEnd, yearStart, yearEnd, excludeYears)

##### Parameters

* `latitude` and `longitude` _(numerics)_
	* The geolocation for which you want weather data
	* Required 
* `monthDayStart` _(character)_
	* The month and day of the first day in a range for which you want norms.
	* Alternatively, if you only want data for a single date, enter that date here 
	* Format is MM-DD
	* Required
* `monthDayEnd` _(character)_
	* The month and day of the last day in the range.
	* If not supplied, but a `monthDayStart` is, then the API returns data only for a single day
	* Format is MM-DD
	* Optional
* `yearStart` _(character)_
	* The first of a range of years over which to calculate norms (inclusive)
	* Note: a minimum of three years is required
	* Use a four-digit year (YYYY)
	* Optional; if not used, the API defaults to a 10-year norm
* `yearEnd` _(character)_
	* The last of a range of years over which to calculate norms (inclusive)
	* Note: a minimum of three years is required
	* Use a four-digit year (YYYY)
	* Optional:
		* If `yearStart` is used then this is required
		* if not used, the API defaults to a 10-year norm
* `excludeYears` _(character)_
	* A comma-separated list of years that you don't want included in the average
	* Note: a minimum of three years is required even after years are excluded
	* Use four-digit years (YYYY)
	* Optional


##### Example

    GetWeatherNormsLatLon('39.8282', '-98.5795','07-01','07-31','2010','2015')


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#### Current Conditions 

The Current Conditions API returns a snapshot of area weather for a location using recent data from the nearest available station. While the data is QA'd before delivery, this service does not do the same advanced processing or interpolation as Daily Observed data. [API Documentation](http://developer.awhere.com/api/reference/weather/current)

##### Function Signatures 

    GetCurrentConditionsLatLon(latitude, longitude, sources)

##### Parameters

* `latitude` and `longitude` _(numerics)_
	* The geolocation for which you want weather data
	* Required 
* `sources` _(character)_
	* Filters what kind of stations to use, as different station networks have different standards for quality and reporting
	* Options are: 
		* metar = METAR stations are the National Weather Service stations commonly found at airports
		* mesonet = MESONET stations are typically high-quality stations maintained by academic or other highly-attentive organizations
		* pws = Personal Weather Stations are typically good quality but do not have the same guarantees as other networks
		* metar-mesonet = select from either METAR or MESONET stations
		* all = Use all available stations
	* Default is all 
	* Optional 

##### Example

This example excludes personal weather stations and returns the data from nearest METAR or MESONET station:

    GetCurrentConditionsLatLon('39.8282', '-98.5795', 'metar-mesonet')




___________________________________________________________________________
---------------------------------------------------------------------------

### Agronomic Data

aWhere provides a rich set of agronomic values that adds deeper layers of insight on top of weather data alone. These functions provide quick access to derived values like GDD or PET, and the long-term norms for each value. If you intend to use aWhere's modeling capabilities in the future, you should must Field Locations and Plantings (see other documentation).

All weather data requests are requested point by point. There is not a way to download regional data in a single API call. Use the [batch jobs system](http://developer.awhere.com/api/reference/batch) when requesting a large amount of data (e.g., many hundreds or thousands of points). 


Learn more at the [aWhere Developer Community](http://developer.awhere.com/api/reference/agronomics)


- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#### Agronomic Values 

Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop. The aWhere platform can calculate GDD (using a variety of different equations), PET (using the Penman-Monteith Equation), P/PET, and accumulated precipitation, accumulated GDD, accumulated PET, and P/PET over a range of days. [API Documentation](http://developer.awhere.com/api/reference/agronomics/values)

##### Function Signatures 

    GetAgronomicValuesLatLon(fieldId, dayStart, dayEnd, accumulationStartDate, gddMethod, gddBaseTemp, gddMinBoundary, gddMaxBoundary) 

##### Parameters 

* `latitude` and `longitude` _(numerics)_
	* The geolocation for which you want weather data
	* Required 
* `dayStart` _(character)_
	* The starting date of a range of dates for which you want agronomic values 
	* Alternatively, if you only want data for a single date, enter that date here 
	* If not supplied, the API defaults to returning the last week of data
	* Format is YYYY-MM-DD and the date must fall before today
	* Optional
* `dayEnd` _(character)_
	* The ending date of a range of dates for which you want data. 
	* If not supplied, but a `dayStart` is, then the API returns data only for a single day
	* Format is YYYY-MM-DD and the date must fall before today and after `dayStart`
	* Optional
* `accumulationStartDate` _(character)_
	* If you want the accumulations to start counting from a date before `dayStart` enter that date here
	* Format is YYYY-MM-DD and the date must fall before today and before `dayStart`
	* Optional
* `gddMethod` _(character)_
	* Which GDD equation to use 
	* Options are standard, modifiedstandard, min-temp-cap, min-temp-constant
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional
* `gddBaseTemp` _(numeric)_
	* The base temp to use with the GDD equation
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional
* `gddMinBoundary` _(numeric)_
	* The lower boundary value for the GDD equation (if used)
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional
* `gddMaxBoundary` _(numeric)_
	* The upper boundary value for the GDD equation (if used)
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional

##### Example

    GetAgronomicValuesLatLon('39.8282', '-98.5795','2015-07-01','2015-07-31','2015-06-01','modifiedstandard','10','10','30')



- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#### Agronomic Norms 

The Agronomic Norms API returns the long-term normals for the agronomic values on any given set of days across any range of years for which we have data. This allows you to calculate the desired averages without having to download huge data sets. [API Documentation](http://developer.awhere.com/api/reference/agronomics/norms). 

##### Function Signatures 

    GetAgronomicNormsLatLon(latitude, longitude, monthDayStart, monthDayEnd, yearStart, yearEnd, excludeYears, accumulationStartDate, gddMethod, gddBaseTemp, gddMinBoundary, gddMaxBoundary)

##### Parameters

* `latitude` and `longitude` _(numerics)_
	* The geolocation for which you want weather data
	* Required 
* `monthDayStart` _(character)_
	* The month and day of the first day in a range for which you want norms.
	* Alternatively, if you only want data for a single date, enter that date here 
	* Format is MM-DD
	* Required
* `monthDayEnd` _(character)_
	* The month and day of the last day in the range.
	* If not supplied, but a `monthDayStart` is, then the API returns data only for a single day
	* Format is MM-DD
	* Optional
* `yearStart` _(character)_
	* The first of a range of years over which to calculate norms (inclusive)
	* Note: a minimum of three years is required
	* Use a four-digit year (YYYY)
	* Optional; if not used, the API defaults to a 10-year norm
* `yearEnd` _(character)_
	* The last of a range of years over which to calculate norms (inclusive)
	* Note: a minimum of three years is required
	* Use a four-digit year (YYYY)
	* Optional:
		* If `yearStart` is used then this is required
		* if not used, the API defaults to a 10-year norm
* `excludeYears` _(character)_
	* A comma-separated list of years that you don't want included in the average
	* Note: a minimum of three years is required even after years are excluded
	* Use four-digit years (YYYY)
	* Optional
* `gddMethod` _(character)_
	* Which GDD equation to use 
	* Options are standard, modifiedstandard, min-temp-cap, min-temp-constant
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional
* `gddBaseTemp` _(numeric)_
	* The base temp to use with the GDD equation
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional
* `gddMinBoundary` _(numeric)_
	* The lower boundary value for the GDD equation (if used)
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional
* `gddMaxBoundary` _(numeric)_
	* The upper boundary value for the GDD equation (if used)
	* See [documentation on selecting and configuring a GDD equation](http://developer.awhere.com/api/reference/agronomics/values#about-gdds-equations-and-default-values)
	* Optional


##### Example

    GetAgronomicNormsLatLon('39.8282', '-98.5795','07-01','07-31','2010','2015')
