# APPLYING NKDE ON TRAFFIC ACCIDENT SEMARANG
#calculated three NKDE with a quartic kernel. Bandwith = 1500 meters based on cross validation likelihood. This bandwidth ensures that, most often, only events located closer than two street segments from a sampling point contribute to its density.

## INSTALL PACKAGE HERE
install.packages("sf")
library(sf)
library(ggplot2)
library(spNetwork)

## INPUTING SPATIAL DATA
plots_datalaka <- st_read("D:/KULIAH UNDIP/SEMESTER 8/TUGAS AKHIR/PENGOLAHAN/DATA SHP POIN/Kecelakaan_2021-2023_SRGI.shp")
plots_road <- st_read("D:/KULIAH UNDIP/SEMESTER 8/TUGAS AKHIR/PENGOLAHAN/DATA JALAN/Jalan_SMG_SRGI1.shp")

## SIMPLIFY LINE
simple_lines <- sf::st_cast(plots_road, "MULTILINESTRING")

## MAKING VALID GEOMETRY
simple_lines_simplified <- st_simplify(simple_lines, dTolerance = 0.05)
simple_lines_valid <- st_make_valid(simple_lines_simplified)

## CONVERT TO LINESTRING, IF NEEDED
if (!inherits(simple_lines_valid, "MULTILINESTRING")) {
  simple_lines_valid <- st_cast(simple_lines_valid, "LINESTRING")
}

## SPLITTING LINE AS LIXELS
lixels <- lixelize_lines(simple_lines_valid, 200, 100)

## EXTRACTING THE CEBTER OF LIXELS AS SAMPLING POINTS
sample_pts <- lines_center(lixels)

## CALCULATING THE DENSITIES ESTIMATIONS
###method 1: densities for the simple NKDE
nkde_simple <- nkde(lines = simple_lines_valid,
                    events = plots_datalaka,
                    w = rep(1, nrow(plots_datalaka)),
                    samples = sample_pts,
                    kernel_name = "quartic",
                    bw = 1500, method = "simple",
                    div = "bw", digits = 3,
                    agg = 1, grid_shape = c(100, 100),
                    verbose = FALSE)

###method 2: densities for the discontinuous NKDE
nkde_discontinuous <- nkde(lines = simple_lines_valid,
                           events = plots_datalaka,
                           w = rep(1, nrow(plots_datalaka)),
                           samples = sample_pts,
                           kernel_name = "quartic",
                           bw = 1500, method = "discontinuous",
                           div = "bw", digits = 3,
                           agg = 1, grid_shape = c(100, 100),
                           verbose = FALSE)

###method 3: densities for the continuous NKDE
nkde_continuous <- nkde(lines = simple_lines_valid,
                        events = plots_datalaka,
                        w = rep(1, nrow(plots_datalaka)),
                        samples = sample_pts,
                        kernel_name = "quartic",
                        bw = 1500, method = "continuous",
                        div = "bw", digits = 3,
                        agg = 1, grid_shape = c(100, 100),
                        verbose = FALSE)

#To obtain more readable results, one can multiply the obtained densities by the total number of accident (to make the spatial integral equal to the number of event) and multiply this value again by 1000 to get the estimaed numbers of accidents per kilometer
## ADDING THE DENSITIES AS NEW COLOUMNS TO THE SAMPLING
lixels$simple_density <- nkde_simple * nrow(plots_datalaka) * 1000
lixels$continuous_density <- nkde_continuous * nrow(plots_datalaka) * 1000
lixels$dicontinuous_density <- nkde_discontinuous * nrow(plots_datalaka) * 1000

#IF U WANNA KNOW THE DIFERENCES OF EACH METHOD
lixels$difference <- lixels$simple_density - lixels$continuous_density
lixels$difference2 <- lixels$continuous_desity - lixels$discontinuous_density

## COMPARISON THE ESTIMATES DENSITIES 
## PACKAGE FOR THIS STAGES
library(ggplot2)
library(tidyr)
library(dplyr)
df <- st_drop_geometry(lixels)[c("simple_density",
                                 "continuous_density",
                                 "dicontinuous_density")]
df <- df[order(df$continuous_density), ]
df$oid <- 1:nrow(df)
pivot_cols <- names(df)[names(df) != "oid"]
df2 <- pivot_longer(df, cols = all_of(pivot_cols))
df2$name <- case_when(
  df2$name == "simple_density" ~ "Simple NKDE",
  df2$name == "continuous_density" ~ "Continuous NKDE",
  df2$name == "dicontinuous_density" ~ "Discontinuous NKDE")
ggplot(df2) +
  geom_point(aes(x = oid, y = value, color = name), size = 0.2)+
  ylab("Estimated densities")+
  scale_color_manual(values = c("Simple NKDE"="yellow",
                                "Continuous NKDE"="red",
                                "Discontinuous NKDE"="green"))+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.title = element_blank())
