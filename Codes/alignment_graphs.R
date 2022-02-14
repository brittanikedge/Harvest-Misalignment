# /*=================================================*/
#' # Preparation
# /*=================================================*/

library(here)
library(future.apply)
library(tmap)
library(sp)
library(sf)
library(agricolae)
library(lwgeom)
library(scam)
library(measurements)
library(raster)
library(data.table)
library(tidyverse)
library(gdata)
library(mgcv)
library(MonteCarlo)
library(parallel)
library(gstat)
library(mapproj)
library(RColorBrewer)

#--- source functions ---#
source(here("Codes", "functions new.R"))

#--- github ---#
source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")

#--- setup ---#

trial_grids <- readRDS("Results/trial_grids.rds")
harvester_polygons <- readRDS("Results/harvester_polygons.rds")

rates_ls <- seq(80, 280, by = 40)
exp_design <- assign_rates(
  filter(trial_grids, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

trial_design <- merge(trial_grids, exp_design, by = "td_grid_id")
xlim <- c(352640 - 60, 352640 + 60)
ylim <- c(4337720 - 100, 4337720 + 100)

ggplot(trial_design) +
  geom_sf(data = trial_design, aes(fill = as.factor(rate)), lwd = 0) +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())

##### Angle and Misalignment #####
alignment_case <- c("angle", "mis-alignment")
error_degree <- c(0, 10, 30)

### alignment maps ###

map_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_harvest = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_harvest(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

map_data_for_plot <- map_data %>%
  rowwise() %>% 
  mutate(coverage_harvest = list(
    data.table(coverage_harvest) %>% 
      .[, alignment_case := alignment_case] %>% 
      .[, error_degree := error_degree]
  )) %>% 
  pluck("coverage_harvest") %>% 
  rbindlist() %>% 
  st_as_sf()

map_data_for_plot$error_degree <- as.factor(map_data_for_plot$error_degree)
levels(map_data_for_plot$error_degree) <- c("0-degree", "10-degree", "30-degree")

trial_design$rate <- as.factor(trial_design$rate)

alignment_map_angle <- ggplot(subset(map_data_for_plot, alignment_case == "angle")) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  facet_grid( ~ error_degree) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  guides(fill = guide_legend(title = "Target Rate")) +
  scale_fill_brewer(palette="Blues")
  
saveRDS(alignment_map_angle, file = here("Results", "alignment_map_angle.rds"))

levels(map_data_for_plot$error_degree) <- c("0-foot shift", "10-foot shift", "30-foot shift")

alignment_map_shift <- ggplot(subset(map_data_for_plot, alignment_case == "mis-alignment")) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  facet_grid( ~ error_degree) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  guides(fill = guide_legend(title = "Target Rate")) +
  scale_fill_brewer(palette="Blues")
saveRDS(alignment_map_shift, file = here("Results", "alignment_map_shift.rds"))


##### Mismatch #####

boundary <- st_read(here("Data/Wendte_LaueLib80_2020/Raw", "boundary.shp")) %>%
  st_transform_utm()

abline <- st_read(here("Data/Wendte_LaueLib80_2020/Raw", "ab-line.shp")) %>%
  st_transform_utm()

# perhaps these are not in feet but meters
trial_grids <- make_trial_grids(field = boundary,
                                ab_line = abline,
                                plot_width = 18.288,
                                cell_height = 30,
                                headland_length = 0) %>%
  mutate(td_grid_id = paste0(strip_id, "_", cell_id))

rates_ls <- seq(80, 280, by = 40)
exp_design <- assign_rates(
  filter(trial_grids, td_grid_id != "headland"), 
  rates_ls
) %>% 
  data.table() %>% 
  .[, .(td_grid_id, rate)]

trial_design <- merge(trial_grids, exp_design, by = "td_grid_id")

harvest_grids <- make_trial_grids(field = boundary,
                                  ab_line = abline,
                                  plot_width = 18.288,
                                  cell_height = 2,
                                  headland_length = 0) %>%
  mutate(td_grid_id = paste0(strip_id, "_", cell_id))

alignment_case <- c("mismatch")
error_degree <- c(0.75, 1, 1.25)

### alignment maps ###

map_data <- expand.grid(
  alignment_case = alignment_case,
  error_degree = error_degree
) %>% 
  tibble() %>% 
  mutate(
    coverage_harvest = mclapply(
      seq_len(nrow(.)),
      function(i) {
        get_harvest(
          harvester_polygons,
          .$alignment_case[[i]],
          .$error_degree[[i]]
        )
      },
      mc.cores = 3
    )
  )

map_data_for_plot <- map_data %>%
  rowwise() %>% 
  mutate(coverage_harvest = list(
    data.table(coverage_harvest) %>% 
      .[, alignment_case := alignment_case] %>% 
      .[, error_degree := error_degree]
  )) %>% 
  pluck("coverage_harvest") %>% 
  rbindlist() %>% 
  st_as_sf()

map_data_for_plot$error_degree <- as.factor(map_data_for_plot$error_degree)
levels(map_data_for_plot$error_degree) <- c("0.75 Ratio", "1 Ratio", "1.25 Ratio")

trial_design$rate <- as.factor(trial_design$rate)

alignment_map_mismatch <- ggplot(map_data_for_plot) +
  geom_sf(data = trial_design, aes(fill = rate), lwd = 0) +
  geom_sf(fill = NA, size = 0.2, color = "black") +
  coord_sf(xlim = xlim, 
           ylim = ylim,
           datum = st_crs(trial_grids)) +
  facet_grid( ~ error_degree) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  guides(fill = guide_legend(title = "Target Rate")) +
  scale_fill_brewer(palette="Blues")

saveRDS(alignment_map_mismatch, file = here("Results", "alignment_map_mismatch.rds"))

