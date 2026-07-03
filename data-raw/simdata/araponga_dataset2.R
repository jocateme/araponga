library(araponga)
library(fst)
library(data.table)
library(dplyr)
library(arrow)

dt <- fread("~/Documents/Projetos/Bellbirds/araponga_dataset/sim_data.csv")
dt <- dt[,c(1, 2, 3, 9, 10, 19, 20)]
write.fst(dt, "~/Documents/Projetos/Bellbirds/araponga_dataset/sim_data.fst", compress = 100)

sim_data <- read.fst("~/Documents/Projetos/Bellbirds/araponga_dataset/sim_data.fst",
                     as.data.table = TRUE)
setDT(sim_data)
sim_data$tip_py_tl <- sim_data$tip_py_tl*-1
sim_data$base_py_tl <- sim_data$base_py_tl*-1
sim_data[, pitch2d := pitch2d.from.xy(sim_data$tip_px_tl,
                                      sim_data$tip_py_tl,
                                      sim_data$base_px_tl,
                                      sim_data$base_py_tl)]
dx <- sim_data$tip_px_tl - sim_data$base_px_tl
dy <- sim_data$tip_py_tl - sim_data$base_py_tl
sim_data[, length := sqrt(dx^2 + dy^2)]
# imputing 0 to match behavior of atan2(0, 0) when points are essentially overlaid
sim_data$pitch2d[sim_data$length <= 0.001] <- 0

sim_data <- sim_data[,c(1:3, 8)]
colnames(sim_data) <- c("pitch", "yaw", "view_elevation", "pitch2d")

sim_data <- sim_data %>%
  mutate(
    yaw = as.integer(yaw),
    pitch = as.integer(pitch),
    view_elevation = as.integer(view_elevation)
  )

write_dataset(sim_data,
              path = "~/Documents/Projetos/Bellbirds/araponga_dataset/sim_data_parquet",
              format = "parquet",
              partitioning = c("view_elevation"))
