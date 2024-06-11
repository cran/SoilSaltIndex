#' Generation of Soil Salinity Indices using Satellite Data
#'
#' @param B Raster layer representing the Blue band (default NULL)
#' @param G Raster layer representing the Green band (default NULL)
#' @param R Raster layer representing the Red band (default NULL)
#' @param NIR Raster layer representing the Near-Infrared band (default NULL)
#' @param SW1 Raster layer representing the Shortwave Infrared band 1 (default NULL)
#' @param SW2 Raster layer representing the Shortwave Infrared band 2 (default NULL)
#' @return RasterStack of relevant salinity indices based on available input bands
#' @examples
#' \donttest{
#' library(SoilSaltIndex)
#' library (raster)
#' # Example usage:
#' B <- raster::raster(system.file("extdata", "Blue.tif", package = "SoilSaltIndex"))
#' G <- raster::raster(system.file("extdata", "Green.tif", package = "SoilSaltIndex"))
#' R <- raster::raster(system.file("extdata", "Red.tif", package = "SoilSaltIndex"))
#' NIR <- raster::raster(system.file("extdata", "NIR.tif", package = "SoilSaltIndex"))
#' SW1 <- raster::raster(system.file("extdata", "SWIR1.tif", package = "SoilSaltIndex"))
#' SW2 <- raster::raster(system.file("extdata", "SWIR2.tif", package = "SoilSaltIndex"))
#' Salt_Index <- Soil_Salinity_Indices(B=B, G=G, R=R, NIR=NIR, SW1=SW1, SW2=SW2)
#' Salt_Index <- Soil_Salinity_Indices(SW1=SW1, SW2=SW2)
#' Salt_Index <- Soil_Salinity_Indices(B=B, G=G, R=R, NIR=NIR)
#' }
#' @references
#' 1. Rani, A., Kumar, N., Sinha, N. K., & Kumar, J. (2022). Identification of salt-affected soils using remote sensing data through random forest technique: a case study from India. Arabian Journal of Geosciences, 15(5), 381.<DOI:10.1007/s12517-022-09682-3>
#' 2. Kumar et al. (2023). SpatGRID:Spatial Grid Generation from Longitude and Latitude List. R package version 0.1.0.
#' @export
#' @import raster
#' @import sp
Soil_Salinity_Indices <- function(B = NULL, G = NULL, R = NULL, NIR = NULL, SW1 = NULL, SW2 = NULL) {
  ## List to store the calculated indices
  indices <- list()
  index_names <- list()
  ## Conditionally calculate indices based on available bands
  if (!is.null(R) && !is.null(NIR)) {
    indices[["NDSI"]] <- (R - NIR) / (R + NIR)
  }
  if (!is.null(NIR) && !is.null(SW1)) {
    indices[["SIT"]] <- (NIR / SW1)
  }
  if (!is.null(R) && !is.null(NIR)) {
    indices[["BI"]] <- sqrt(R^2 + NIR^2)
  }
  if (!is.null(G) && !is.null(R)) {
    indices[["SI1"]] <- sqrt(G * R)
  }
  if (!is.null(G) && !is.null(R) && !is.null(NIR)) {
    indices[["SI2"]] <- sqrt(G^2 + R^2 + NIR^2)
  }
  if (!is.null(G) && !is.null(R)) {
    indices[["SI3"]] <- sqrt(G^2 + R^2)
  }
  if (!is.null(SW1) && !is.null(SW2)) {
    indices[["SI4"]] <- SW1 / SW2
  }
  if (!is.null(NIR) && !is.null(SW1)) {
    indices[["SI5"]] <- (NIR - SW1) / (NIR + SW1)
  }
  if (!is.null(SW1) && !is.null(SW2)) {
    indices[["SI6"]] <- (SW1 - SW2) / (SW1 + SW2)
  }
  if (!is.null(B) && !is.null(R)) {
    indices[["SI7"]] <- B / R
  }
  if (!is.null(B) && !is.null(R)) {
    indices[["SI8"]] <- (B - R) / (B + R)
  }
  if (!is.null(G) && !is.null(R) && !is.null(B)) {
    indices[["SI9"]] <- (G * R) / B
  }
  if (!is.null(B) && !is.null(R)) {
    indices[["SI10"]] <- sqrt(B * R)
  }
  if (!is.null(B) && !is.null(R) && !is.null(G)) {
    indices[["SI11"]] <- (B * R) / G
  }
  if (!is.null(R) && !is.null(NIR) && !is.null(G)) {
    indices[["SI12"]] <- (R * NIR) / G
  }
  if (!is.null(R) && !is.null(NIR)) {
    indices[["SI13"]] <- sqrt(R * NIR)
  }
  if (!is.null(R) && !is.null(NIR)) {
    indices[["SI14"]] <- (R / NIR) * 100
  }
  if (!is.null(R) && !is.null(NIR) && !is.null(G) && !is.null(B)) {
    ## Compute the numerator, denominator and ratio separately
    num_csri <- (R * NIR) - (G * B)
    denom_csri <- (R * NIR) + (G * B)
    ratio_csri <- num_csri / denom_csri
    # Replace negative values with NA (to avoid NaNs in sqrt)
    ratio_csri[ratio_csri < 0] <- NA
    indices[["CSRI"]] <- sqrt(ratio_csri)
  }
  if (!is.null(B) && !is.null(G) && !is.null(R) && !is.null(NIR)) {
    indices[["COSRI"]] <- ((B + G) / (R + NIR)) * ((NIR - R) / (NIR + R))
  }
  if (!is.null(SW1) && !is.null(SW2) && !is.null(NIR)) {
    indices[["NSI"]] <- (SW1 - SW2) / (SW1 - NIR)
  }
  if (!is.null(SW1) && !is.null(NIR) && !is.null(SW2)) {
    indices[["SI.ASTER"]] <- (SW1 - NIR) / (SW1 + SW2)
  }
  if (!is.null(SW1) && !is.null(SW2)) {
    indices[["SSSI1"]] <- (SW1 - SW2)
  }
  if (!is.null(SW1) && !is.null(SW2)) {
    indices[["SSSI2"]] <- (SW1 * SW2 - SW2 * SW2) / SW1
  }
  if (!is.null(G) && !is.null(R) && !is.null(NIR)) {
    indices[["VSSI"]] <- 2 * G - (5 * (R + NIR))
  }

  ## Stack the indices
  Stack_Salinity_Indices <- stack(indices)
  names(Stack_Salinity_Indices) <- names(indices)

  SI <- writeRaster(Stack_Salinity_Indices, filename = names(Stack_Salinity_Indices),
                    path = getwd(), bylayer = TRUE, format = "GTiff", overwrite = TRUE)
  return(SI)
}

