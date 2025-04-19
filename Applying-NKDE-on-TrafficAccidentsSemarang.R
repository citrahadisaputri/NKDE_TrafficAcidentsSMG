#APPLYING NKDE ON TRAFFIC ACCIDENT SEMARANG #####################
#calculated three NKDE with a quartic kernel. Bandwith = 1500 meters based on cross validation likelihood. This bandwidth ensures that, most often, only events located closer than two street segments from a sampling point contribute to its density.

##Install package here
install.packages("sf")
library(sf)
library(ggplot2)
library(spNetwork)

##Inputing Spatial Data
plots_datalaka <- st_read("D:/University Course/SEMESTER 8/TUGAS AKHIR/PENGOLAHAN/DATA SHP POIN/Kecelakaan_2021-2023_SRGI.shp")
plots_road <- st_read("D:/University Course/SEMESTER 8/TUGAS AKHIR/PENGOLAHAN/DATA JALAN/Jalan_SMG_SRGI1.shp")

##simplify
simple_lines <- sf::st_cast(plots_road, "MULTILINESTRING")

##Making valid geometry 
simple_lines_simplified <- st_simplify(simple_lines, dTolerance = 0.05)
simple_lines_valid <- st_make_valid(simple_lines_simplified)

##Convert to LINESTRING, if needed
if (!inherits(simple_lines_valid, "MULTILINESTRING")) {
  simple_lines_valid <- st_cast(simple_lines_valid, "LINESTRING")
}

##splitting the lines as lixel
lixels <- lixelize_lines(simple_lines_valid, 200, 100)

##extracting the center of lixels as sampling points
###Setelah lixel terbentuk, dicarilah center atau titik tengah lixel sebagai sampling titik dengan variabel ‘sample_pts’. Digunakannya center point ini, nantinya diharapkan mampu memperoleh representasi yang lebih baik dari garis asli dan memastikan bahwa titik sampel tersebar secara merata di sepanjang garis
sample_pts <- lines_center(lixels)

#DATAFRAME PEMBOBOTAN######################
# Inisialisasi dataframe dengan kolom yang benar
df_W <- data.frame(
  location_id = character(0),
  severity = character(0),
  geometry = character(0)
)

# Mengubah format dataframe
for (i in 1:nrow(plots_datalaka)) {
  jalan <- plots_datalaka$Lokasi_Kej[i]
  x_coord <- plots_datalaka$Long[i]
  y_coord <- plots_datalaka$Lat[i]
  MD_count <- plots_datalaka$MD[i]
  LB_count <- plots_datalaka$LB[i]
  LR_count <- plots_datalaka$LR[i]
  
  # Menambahkan data untuk setiap severity
  if (!is.na(MD_count) && MD_count > 0) {
    df_W <- rbind(df_W, data.frame(location_id = rep(jalan, MD_count), severity = "MD", geometry = paste0("POINT (", x_coord, " ", y_coord, ")")))
  }
  if (!is.na(LB_count) && LB_count > 0) {
    df_W <- rbind(df_W, data.frame(location_id = rep(jalan, LB_count), severity = "LB", geometry = paste0("POINT (", x_coord, " ", y_coord, ")")))
  }
  if (!is.na(LR_count) && LR_count > 0) {
    df_W <- rbind(df_W, data.frame(location_id = rep(jalan, LR_count), severity = "LR", geometry = paste0("POINT (", x_coord, " ", y_coord, ")")))
  }
}

# Menambahkan kolom bobot berdasarkan tingkat keparahan
df_W$weight <- ifelse(df_W$severity == "MD", 12,
                      ifelse(df_W$severity == "LB", 6,
                             ifelse(df_W$severity == "LR", 3, NA)))

##calculating the densities estimations
###method 1: densities for the simple NKDE
future::plan(future::multisession(workers=4))
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

## make sure any open connections are closed afterward
if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)

#To obtain more readable results, one can multiply the obtained densities by the total number of accident (to make the spatial integral equal to the number of event) and multiply this value again by 1000 to get the estimaed numbers of accidents per kilometer
## Adding the densities as new columns to the sampling
lixels$simple_density <- nkde_simple * nrow(plots_datalaka) * 1000
lixels$continuous_density <- nkde_continuous * nrow(plots_datalaka) * 1000
lixels$dicontinuous_density <- nkde_discontinuous * nrow(plots_datalaka) * 1000

#wanna know the diferences of each method
lixels$difference <- lixels$simple_density - lixels$dicontinuous_density
lixels$difference2 <- lixels$continuous_desity - lixels$discontinuous_density

## Comparison of the estimated densities.
## pakeage for this stage
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
