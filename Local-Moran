#LOCAL MORAN
#It has been proposed to combine the NKDE with the local Moran I autocorrelation index to identify significant hot spots or cold spots of events’ intensity Beyond 200 meters the weight is set to 0, and the matrix is row standardized. This example illustrates that spNetwork is well integrated with the actual ecosystem of R packages dedicated to spatial analysis.

##Install Package
install.packages("spdep")
library(spdep)
library(dplyr)

##calculating the spatial weight matrix
listw <- network_listw(origins = sample_pts,
                       lines = simple_lines_valid,
                       maxdistance = 200, dist_func = "inverse",
                       matrice_type = "W",
                       grid_shape = c(100,100), digits = 3,
                       verbose = FALSE, tol = 0.01)

##density to vector
densitas_vektor <- as.numeric(lixels$continuous_density)
densitas_vektor_S <- as.numeric(lixels$simple_density)
densitas_vektor_DIS <- as.numeric(lixels$dicontinuous_density)

##calculating the local Moran I values
locmoran <- localmoran(densitas_vektor, listw = listw)
locmoran_S <- localmoran(densitas_vektor_S, listw = listw)
locmoran_DIS <- localmoran(densitas_vektor_DIS, listw = listw)

##centering the NKDE values and calculating the spatially lagged variable
cent_density <- scale(lixels$continuous_density, scale = F)
cent_density_S <- scale(lixels$simple_density, scale = F)
cent_density_DIS <- scale(lixels$dicontinuous_density, scale = F)

lag_cent_density <- lag.listw(listw, cent_density)
lag_cent_density_S <- lag.listw(listw, cent_density_S)
lag_cent_density_DIS <- lag.listw(listw, cent_density_DIS)

##mapping the results
###CONTINUOUS
library(tmap)
lixels$moran_quad <- case_when(
  cent_density > 0 & lag_cent_density > 0 & locmoran[,5]<=0.2 ~ "high-high",
  cent_density < 0 & lag_cent_density < 0 & locmoran[,5]<=0.2 ~ "low-low",
  cent_density > 0 & lag_cent_density < 0 & locmoran[,5]<=0.2 ~ "high-low",
  cent_density < 0 & lag_cent_density > 0 & locmoran[,5]<=0.2 ~ "low-high",
  locmoran[,5] > 0.2 ~ "not sign.",
)
colors <- c("high-high" = "#E63946",
            "high-low" = "#EC9A9A",
            "low-high" = "#A8DADC",
            "low-low" = "#457B9D",
            "not sign." = "black")
not_sign <- subset(lixels, lixels$moran_quad == "not sign.")
sign <- subset(lixels, lixels$moran_quad != "not sign.")
tm_shape(not_sign,unit = 'm') +
  tm_lines(1.2, title.col = 'Local Moran for the continuous NKDE',
           col = 'green') +
  tm_shape(sign,unit = 'm') +
  tm_lines(2, title.col = 'Local Moran for the continuous NKDE',
           col = 'moran_quad', palette = colors) +
  tm_layout(legend.outside = TRUE, inner.margins = 0,
            outer.margins = 0, legend.title.size = 1,
            legend.outside.position = "right",
            frame = FALSE)

###SIMPLE
lixels$moran_quad_S <- case_when(
  cent_density_S > 0 & lag_cent_density_S > 0 & locmoran_S[,5]<=0.2 ~ "high-high",
  cent_density_S < 0 & lag_cent_density_S < 0 & locmoran_S[,5]<=0.2 ~ "low-low",
  cent_density_S > 0 & lag_cent_density_S < 0 & locmoran_S[,5]<=0.2 ~ "high-low",
  cent_density_S < 0 & lag_cent_density_S > 0 & locmoran_S[,5]<=0.2 ~ "low-high",
  locmoran_S[,5] > 0.2 ~ "not sign Simple.",
)
colors <- c("high-high" = "#E63946",
            "high-low" = "#EC9A9A",
            "low-high" = "#A8DADC",
            "low-low" = "#457B9D",
            "not sign." = "black")
not_sign_S <- subset(lixels, lixels$moran_quad_S == "not sign Simple.")
sign_S <- subset(lixels, lixels$moran_quad_S != "not sign Simple.")
tm_shape(not_sign_S,unit = 'm') +
  tm_lines(1.2, title.col = 'Local Moran for the simple NKDE',
           col = 'green') +
  tm_shape(sign_S,unit = 'm') +
  tm_lines(2, title.col = 'Local Moran for the simple NKDE',
           col = 'moran_quad_S', palette = colors) +
  tm_layout(legend.outside = TRUE, inner.margins = 0,
            outer.margins = 0, legend.title.size = 1,
            legend.outside.position = "right",
            frame = FALSE)

###DISCONTINUOUS
lixels$moran_quad_DIS <- case_when(
  cent_density_DIS > 0 & lag_cent_density_DIS > 0 & locmoran_DIS[,5]<=0.2 ~ "high-high",
  cent_density_DIS < 0 & lag_cent_density_DIS < 0 & locmoran_DIS[,5]<=0.2 ~ "low-low",
  cent_density_DIS > 0 & lag_cent_density_DIS < 0 & locmoran_DIS[,5]<=0.2 ~ "high-low",
  cent_density_DIS < 0 & lag_cent_density_DIS > 0 & locmoran_DIS[,5]<=0.2 ~ "low-high",
  locmoran_DIS[,5] > 0.2 ~ "not sign Discontinuous.",
)
colors <- c("high-high" = "#E63946",
            "high-low" = "#EC9A9A",
            "low-high" = "#A8DADC",
            "low-low" = "#457B9D",
            "not sign." = "black")
not_sign_DIS <- subset(lixels, lixels$moran_quad_DIS == "not sign Discontinuous.")
sign_DIS <- subset(lixels, lixels$moran_quad_DIS != "not sign Discontinuous.")
tm_shape(not_sign_DIS,unit = 'm') +
  tm_lines(1.2, title.col = 'Local Moran for the Discontinuous NKDE',
           col = 'green') +
  tm_shape(sign_DIS,unit = 'm') +
  tm_lines(2, title.col = 'Local Moran for the Discontinuous NKDE',
           col = 'moran_quad_DIS', palette = colors) +
  tm_layout(legend.outside = TRUE, inner.margins = 0,
            outer.margins = 0, legend.title.size = 1,
            legend.outside.position = "right",
            frame = FALSE)
