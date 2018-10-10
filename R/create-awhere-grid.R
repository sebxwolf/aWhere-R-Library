
#' @title Create aWhere Grid
#'
#' @description
#' \code{create_awhere_grid} based on a SpatialPolygon, creates a data frame based on aWhere's grid system
#'
#' @details
#' Creates a grid of lat/lon points within given polygon.
#' The aWhere grid is spaced at .08333 decimal degrees resolution (or 5 arc-minutes),
#' so spacing a new grid at every .08 degrees should guarantee
#' a grid point in each aWhere grid cell.
#'
#' @import rgeos
#' @import raster
#' @import sp
#'
#' @param - polygon: either a SpatialPolygons data type object or a character string of valid WKT

create_awhere_grid <- function(polygon) {

  ## If polygon is WKT, convert to SpatialPolygons class
  if(class(polygon) == "character") {
    tryCatch({polygon <- rgeos::readWKT(polygon)}, error = function(e) {
      stop(e)
    })
  } else if (class(polygon) == 'Extent') {
    polygon <- as(polygon, 'SpatialPolygons')
  }

  ## Buffer polygon by 0.5 degrees to make sure we capture all valid grid cells
  grid <- suppressWarnings(sp::makegrid(raster::buffer(polygon, .5), cellsize = .08))
  ## Create SpatialPoints data frame from calculated grid
  grid <- sp::SpatialPoints(grid, proj4string = sp::CRS(sp::proj4string(polygon)))
  ## Only keep grid points that fall within the polygon
  grid <- grid[polygon,]

  ## Keep only the lat/lon columns
  grid <- as.data.frame(grid@coords)

  colnames(grid) <- c("lon", "lat")

  ## Calculate GridX and GridY for each calculated grid point
  grid$gridx <- getGridX(grid$lon)
  grid$gridy <- getGridY(grid$lat)

  ## Keep only unique GridX/GridY pairings
  grid <- unique(grid[,c("gridx", "gridy")])

  ## Calculate centroid lat and lon for each GridX/GridY
  grid$lon <- getLongitude(grid$gridx)
  grid$lat <- getLatitude(grid$gridy)

  return(grid)
}
