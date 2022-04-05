

#' Mosaic by day of year
#'
#' @param ras_list named list of `SpatRaster`s. Names must follow NASA VNP46A file naming convention
#'
#' @return nested list where each level contains all images for doy.
#' @export
#'
#' @examples \dontrun{
#'  library(terra)
#'
#'  h5_filenames <- list.files("raster/h5/")
#'  h5_paths <- list.files("raster/h5/", full.names = T)

#'  black_marble <- h5_paths |>
#'     map(~vnp46A_to_wgs84(path =.x)) |>
#'      set_names(h5_filenames)
#'
#'  black_marble_by_doy<- mosaic_vnp46a_by_doy(vnp46a1_wgs84)
#'
#'
#' }
mosaic_vnp46a_by_doy<- function(ras_list,output=NULL){
  ras_names<- names(ras_list)
  doys<- stringr::str_split(ras_names,"\\.") |>
    map(2) |>
    map(~str_sub(.x,start = 6,end = 8)) |>
    unlist()
  ras_name_doy_lookup <- data.frame(ras_names,doys)
  u_doys <- unique(ras_name_doy_lookup$doys)
  doy_group_list <-  list()
  for(i in 1:length(u_doys)){
    doy_temp <- u_doys[i]
    ras_name_temp<- ras_name_doy_lookup |>
      filter(doys==doy_temp) |>
      pull(ras_names)

    terra_collection <- terra::sprc(ras_list[ras_name_temp])
    image_mosaic <- terra::mosaic(terra_collection)
    doy_group_list[[doy_temp]] <- image_mosaic
    if(!is.null(output)){
      cat(crayon::green("writing output to ",output,"\n"))
      cal_date <- as.Date(as.numeric(doy_temp), origin = '2021-12-31')|> stringr::str_remove_all(pattern = "-")

      ras_name_temp |> length()
      ras_name_temp[1]
      file_name_prefix <- ras_name_temp |>
        stringr::str_split(pattern = "\\.") |>
        map(1) |> unique() |> unlist()
      output_path_name <-  paste0(output,"/",file_name_prefix,".",cal_date,".wgs84.tiff")
      cat(crayon::green("writing ", output_path_name,"\n"))
      terra::writeRaster(image_mosaic,output_path_name)
    }


  }
  cat(crayon::green("returning SpatRaster mosaics as list"))
  return(doy_group_list)



}



#' group_vnp46a_images_by_doy
#'
#' @param ras_list named list of `SpatRaster`s. Names must follow NASA VNP46A file naming convention
#'
#' @return nested list where each level contains all images for doy.
#' @export
#'
#' @examples \dontrun{
#'  library(terra)
#'
#'  h5_filenames <- list.files("raster/h5/")
#'  h5_paths <- list.files("raster/h5/", full.names = T)

#'  black_marble <- h5_paths |>
#'     map(~vnp46A_to_wgs84(path =.x)) |>
#'      set_names(h5_filenames)
#'
#'  black_marble_by_doy<- group_vnp46a_images_by_doy(vnp46a1_wgs84)
#'
#'
#' }
group_vnp46a_images_by_doy<- function(ras_list){
  ras_names<- names(ras_list)
  doys<- stringr::str_split(ras_names,"\\.") |>
    map(2) |>
    map(~str_sub(.x,start = 6,end = 8)) |>
    unlist()
  ras_name_doy_lookup <- data.frame(ras_names,doys)
  u_doys <- unique(ras_name_doy_lookup$doys)
  doy_group_list <-  list()
  for(i in 1:length(u_doys)){
    doy_temp <- u_doys[i]
    ras_name_temp<- ras_name_doy_lookup |>
      filter(doys==doy_temp) |>
      pull(ras_names)

    doy_group_list[[doy_temp]] <- terra::sprc(ras_list[ras_name_temp])

  }
  return(doy_group_list)


}

# vnp_doy_paths_to_mosaic_paths <-  function
