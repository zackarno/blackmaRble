

#'vnp46A_to_wgs84
#'@description project VNP46A black marble products to WGS84. Input can be multi-band. Can use in conjunction with
#'[purrr::map] to reproject all bands of all rasters contained in folder
#'@param path \code{character} path to hd5 black marble product (VNP46A1 or VNP46A2)
#'@return VNP46A raster/stack in WGS84
#'@export
#'@examples \dontrun{
#'    library(terra)
#'    vnp46a1_wgs84 <- vnp46A_to_wgs84(path = "../../raster/VNP46A1.A2022043.h21v04.001.2022044083115.h5")
#'    # DNB band is 5th band so we can just write that out or we could combine bands however we want
#'    vnp46a1_dnb <- vnp46a1_wgs84[[5]]
#'    writeRaster(vnp46a1_dnb,"vnp46a1_dnb.tiff")
#'}

vnp46A_to_wgs84 <-  function(path){

  terra_ras <- suppressWarnings(terra::rast(path))
  wgs84_extent <- get_vnp46A_extent(path)
  terra::ext(terra_ras) <- wgs84_extent
  terra::crs(terra_ras) <- "epsg:4326"
  cat(crayon::green("returning terra SpatRaster with CRS = WGS84\n"))
  return(terra_ras)
}


#' get_vnp46A_extent
#' @param path \code{character} path to hd5 black marble product (VNP46A1 or VNP46A2)
#' @return A \code{numeric} vector containing new extent in correct extent format for `SpatRaster` class ([terra] package)
#' @export

get_vnp46A_extent <- function(path){
  file_meta<- terra::describe(path,meta=T,parse=T)
  tile_number_meta<- stringr::str_subset(file_meta,"^HorizontalTileNumber.+|^VerticalTileNumber.+")
  tile_numbers<- readr::parse_number(tile_number_meta)
  horizontal_tile_number <- tile_numbers[1]
  vertical_tile_number <- tile_numbers[2]
  west_bound_coord <-  (10*horizontal_tile_number) - 180
  north_bound_coord <-  90-(10*vertical_tile_number)
  east_bound_coord <-  west_bound_coord + 10
  south_bound_coord = north_bound_coord - 10

  return(c(west_bound_coord,east_bound_coord,south_bound_coord,north_bound_coord))
}



