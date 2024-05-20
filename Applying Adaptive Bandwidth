#APPLYING ADAPTIVE BANDWITH
#can be used to produce a new version of the three NKDE As we can see, the adaptive kernel produces more smoothed results in low-intensity subregions and more detailed results in high-density regions

##calculating the continuous kernel with an adaptive bandwidth IN CONTINUOUS NKDE
##package for this stage
library(rgeos)
library(sf)

### NKDE CONTINUOUS
nkde_abw_CON <- nkde(lines = simple_lines_valid,
                        events = plots_datalaka,
                        w = rep(1, nrow(plots_datalaka)),
                        samples = sample_pts,
                        kernel_name = "quartic",
                        bw = 1500, 
                        adaptive = TRUE, trim_bw = 400,
                        method = "continuous",
                        div = "bw", 
                        digits = 3, tol = 0.01,
                        agg = 1, grid_shape = c(100, 100),
                        verbose = FALSE)
lixels$continuous_density_abw <- nkde_abw_CON$k * nrow(plots_datalaka) * 1000

### NKDE SIMPLE
nkde_abw_SIM <- nkde(lines = simple_lines_valid,
                     events = plots_datalaka,
                     w = rep(1, nrow(plots_datalaka)),
                     samples = sample_pts,
                     kernel_name = "quartic",
                     bw = 1500, 
                     adaptive = TRUE, trim_bw = 400,
                     method = "simple",
                     div = "bw", 
                     digits = 3, tol = 0.01,
                     agg = 1, grid_shape = c(100, 100),
                     verbose = FALSE)
lixels$simple_density_abw <- nkde_abw_SIM$k * nrow(plots_datalaka) * 1000

### NKDE DIS
nkde_abw_DIS <- nkde(lines = simple_lines_valid,
                     events = plots_datalaka,
                     w = rep(1, nrow(plots_datalaka)),
                     samples = sample_pts,
                     kernel_name = "quartic",
                     bw = 1500, 
                     adaptive = TRUE, trim_bw = 400,
                     method = "discontinuous",
                     div = "bw", 
                     digits = 3, tol = 0.01,
                     agg = 1, grid_shape = c(100, 100),
                     verbose = FALSE)
lixels$discontinuous_density_abw <- nkde_abw_DIS$k * nrow(plots_datalaka) * 1000

## Using geometric object from sf 
local_bw <- nkde_abw_CON$events
localS_bw <- nkde_abw_SIM$events
localDIS_bw <- nkde_abw_DIS$events

## Menggunakan fungsi st_transform untuk mengubah CRS dan merubah koordinat geometri
local_bw <- st_transform(local_bw, 4326)
localS_bw <- st_transform(localS_bw, 4326)
localDIS_bw <- st_transform(localDIS_bw, 4326)

## Melakukan buffering menggunakan st_buffer
buff_bw <- st_buffer(local_bw, dist = local_bw$bw)
buff_bw_S <- st_buffer(localS_bw, dist = localS_bw$bw)
buff_bw_DIS <- st_buffer(localDIS_bw, dist = localDIS_bw$bw)

## Mapping the elements
### CONTINUOUS
install.packages("tmap")
library(tmap)
tm_shape(lixels, unit = 'm') +
  tm_lines(col = 'continuous_density_abw', title.col = 'continuous NKDE density', n = 3,
           style = 'jenks', palette = c("green", "yellow", "red"))  +
  tm_shape(buff_bw) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(legend.outside = TRUE, 
            inner.margins = 0, outer.margins = 0, 
            legend.title.size = 1,
            legend.outside.position = "right",
            frame = FALSE)

###SIMPLE
tm_shape(lixels, unit = 'm') +
  tm_lines(col = 'simple_density_abw', title.col = 'simple NKDE density', n = 3,
           style = 'jenks', palette = c("green", "yellow", "red"))  +
  tm_shape(buff_bw_S) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(legend.outside = TRUE, 
            inner.margins = 0, outer.margins = 0, 
            legend.title.size = 1,
            legend.outside.position = "right",
            frame = FALSE)

###DISCONTINUOUS
tm_shape(lixels, unit = 'm') +
  tm_lines(col = 'discontinuous_density_abw', title.col = 'discontinuous NKDE density', n = 3,
           style = 'jenks', palette = c("green", "yellow", "red"))  +
  tm_shape(buff_bw_DIS) +
  tm_borders(col = "white", lwd = 1) +
  tm_layout(legend.outside = TRUE, 
            inner.margins = 0, outer.margins = 0, 
            legend.title.size = 1,
            legend.outside.position = "right",
            frame = FALSE)
