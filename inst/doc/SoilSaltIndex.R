## -----------------------------------------------------------------------------
##Installation and loading the library of SoilSaltIndex R package
# You can install the SoilSaltIndex package from CRAN using the following command:
# install.packages("SoilSaltIndex")
# Once installed, you can load the package using
# library(SoilSaltIndex)

###  Generating Salinity Indices using spectral bands

# Example:
#'Example usage:
#'Importing all the required spectral bands from extdata foldar
library(SoilSaltIndex)
library(raster)
B <- raster::raster(system.file("extdata", "Blue.tif", package = "SoilSaltIndex"))
G <- raster::raster(system.file("extdata", "Green.tif", package = "SoilSaltIndex"))
R <- raster::raster(system.file("extdata", "Red.tif", package = "SoilSaltIndex"))
NIR <- raster::raster(system.file("extdata", "NIR.tif", package = "SoilSaltIndex"))
SW1 <- raster::raster(system.file("extdata", "SWIR1.tif", package = "SoilSaltIndex"))
SW2 <- raster::raster(system.file("extdata", "SWIR2.tif", package = "SoilSaltIndex"))

# Salinity Indices generation using all the bands
Salt_Index1 <- Soil_Salinity_Indices(B=B, G=G, R=R, NIR=NIR, SW1=SW1, SW2=SW2)
# Relevant Salinity indices generation using only SWIR1 AND SWIR 2 bands
Salt_Index2 <- Soil_Salinity_Indices(SW1=SW1, SW2=SW2)
# Relevant Salinity indices generation using only Blue, Green, Red and NIR bands
Salt_Index3 <- Soil_Salinity_Indices(B=B, G=G, R=R, NIR=NIR)


## ----echo=FALSE, results='asis'-----------------------------------------------

cat("
1. Normalized Difference Salinity Index (NDSI): $(R - NIR) / (R + NIR)$ ; (Reference: Major et al., 1990)
2. Salinity Index 1 (SI1): $sqrt(G*R)$   ; (Reference: Khan et al., 2005)
3. Salinity Index 2 (SI2): $sqrt(G^2 + R^2 + NIR^2)$   ; (Reference: Douaoui et al., 2006)
4. Salinity Index 3 (SI3): $sqrt(G^2 + R^2)$           ; (Reference: Douaoui et al., 2006)
5. Salinity Index 4 (SI4): $SW1 / SW2$                 ; (Reference: INDP, 2002)
6. Salinity Index 5 (SI5): $(NIR - SW1) / (NIR + SW1)$ ; (Reference: INDP, 2002)
7. Salinity Index 6 (SI6): $(SW1 - SW2) / (SW1 + SW2)$ ; (Reference: INDP, 2002)
8. Salinity Index 7 (SI7): $B / R$                     ; (Ref:Abbas and Khan, 2007; Abbas et al., 2013)
9. Salinity Index 8 (SI8): $(B - R) / (B + R)$         ; (Ref:Abbas and Khan, 2007; Abbas et al., 2013)
10. Salinity Index 9 (SI9): $(G * R) / B$              ; (Ref:Abbas and Khan, 2007; Abbas et al., 2013)
11. Salinity Index 10 (SI10): $sqrt(B * R)$            ; (Ref:Abbas and Khan, 2007; Abbas et al., 2013)
12. Salinity Index 11 (SI11): $(B * R) / G$            ; (Ref:Abbas and Khan, 2007; Abbas et al., 2013)
13. Salinity Index 12 (SI12): $(R * NIR) / G$          ; (Ref:Abbas and Khan, 2007; Abbas et al., 2013)
14. Salinity Index 13 (SI13): $sqrt(R * NIR)$          ; (Ref:Dehni and Lounis, 2012)
15. Salinity Index 14 (SI14): $(R / NIR) * 100$        ; (Ref:Allbed et al., 2014)
16. Canopy Response Salinity Index (CRSI): $sqrt(((R * NIR) - (G * B)) / ((R * NIR) + (G * B)))$ ; (Ref: Scudiero et al., (2014;2015))
17. Combined Spectral Response Index (COSRI): $((B + G) / (R + NIR)) * ((NIR - R) / (NIR + R))$  ;(Ref: Fernandez-Buces et al., 2006)
18. NIR-SWIR Salinity Index (NSI): $(SW1 - SW2) / (SW1 - NIR)$   ; (Ref: Abuelgasim and Ammad, 2019)
19. ASTER Salinity Index (SI.ASTER): $(SW1 - NIR) / (SW1 + SW2)$ ; (Ref: Abuelgasim and Ammad, 2019)
20. Soil Salinity and Sodicity Index 1 (SSSI1): $SW1 - SW2$      ; (Ref: Abuelgasim and Ammad, 2019)
21. Soil Salinity and Sodicity Index 2 (SSSI2): $(SW1 * SW2 - SW2 * SW2) / SW1$  ; (Ref: Abuelgasim and Ammad, 2019)
22. Vegetation Soil Salinity Index (VSSI): $2 * G - 5 * (R + NIR)$  ; (Ref:Dehni and Lounis, 2015 )
23. Salinity Index Tripathi (SIT): $NIR/SW1$                        ; (Ref:Tripathi et al., 1997 )
24. Brightness Index (BI): $sqrt(R^2+NIR^2)$                        ; (Ref:Khan et al., 2005 )
")


