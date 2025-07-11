pacman::p_load(sf, trend, tidyverse, patchwork,
               usmap, ggspatial, snakecase)
options(scipen = 0)
#### UPPER LEFT PANEL  ####
sen_data <- c("nwood_forest_change_data_all.csv", 
              "woody_encroach_change_data_all.csv", 
              "forest_change_data_all.csv", 
              "nwood_transitional_change_data_all.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(across(starts_with("class_"), str_trim),
         change_type = case_when(
           str_detect(class_21, "Deciduous|Evergreen|Mixed") & 
             str_detect(class_01, "Deciduous|Evergreen|Mixed") ~ "Infilling",
           TRUE ~ "Conversion")) %>%
  group_by(us_l3name, change_type, year) %>%
  summarize(area_wc = sum(area_wc),
            area_ha = sum(area_ha), .groups = "drop") %>%
  mutate(perc_wc = area_wc / area_ha * 100) %>%
  arrange(year)

sen_data_l1 <- sen_data %>%
  group_by(change_type, year) %>%
  summarize(area_wc = sum(area_wc),
            area_ha = sum(area_ha), .groups = "drop") %>%
  mutate(perc_wc = area_wc / area_ha * 100) %>%
  arrange(year)

wc_change_l1 <- sen_data_l1 %>%
  filter(year %in% c(2001, 2021)) %>%
  select(year, change_type, area_wc, perc_wc) %>%
  pivot_wider(names_from = year,
              values_from = c(area_wc, perc_wc)) %>%
  mutate(diff_area_wc = `area_wc_2021` - `area_wc_2001`,
         diff_perc_wc = `perc_wc_2021` - `perc_wc_2001`)

sen_results_l1 <- sen_data_l1 %>%
  group_by(change_type) %>%
  summarise(perc_wc_slope = sens.slope(perc_wc)$estimates,
            perc_wc_p_value = sens.slope(perc_wc)$p.value)

# merge wc_change_l1 with sen_results_l1
wc_change_l1_merge <- wc_change_l1 %>%
  merge(sen_results_l1, by = "change_type") %>%
  write_csv("outputs/objective_2/sens_results_l1.csv")

sens_results <- sen_data %>%
  group_by(us_l3name, change_type) %>%
  summarise(perc_wc_slope = sens.slope(perc_wc)$estimates,
            perc_wc_p_value = sens.slope(perc_wc)$p.value)
# Load ecoregion data and prepare it
l3_eco <-
  # Windows path
  #st_read("C:/Users/noah.weidig/OneDrive - University of Florida/thesis/data/us_eco_l3/us_eco_l3.shp", quiet = TRUE) %>%
  # Mac path
  st_read("~/OneDrive - University of Florida/thesis/data/us_eco_l3/us_eco_l3.shp") %>%
  filter(NA_L1CODE == 8) %>%
  dplyr::select(us_l3name = NA_L3NAME) %>%
  group_by(us_l3name) %>%
  summarise() %>%
  mutate(us_l3name = str_replace_all(us_l3name, "/", " "),
         us_l3name = str_replace_all(us_l3name, c(
           "Southern Michigan Northern Indiana Drift Plains" =
             "S MI/N IN Drift Plains",
           "Southeastern Wisconsin Till Plains" =
             "SE Wisconsin Till Plains")))


range(sens_results$perc_wc_slope)
# find bounding box of l3_eco

us_map <- usmap::us_map() %>%
  filter(abbr != "HI" & abbr != "AK") %>%
  st_transform(., st_crs(l3_eco))

# Merge change data with eco-region data and classify significance
top_left_data <-
  sens_results %>%
  filter(change_type == "Infilling") %>%
  mutate(us_l3name = case_when(
    us_l3name == "Huron/Erie Lake Plains" ~ "Huron Erie Lake Plains",
    us_l3name == "Southeastern Wisconsin Till Plains" ~ "SE Wisconsin Till Plains",
    us_l3name == "Southern Michigan/Northern Indiana Drift Plains" ~ "S MI/N IN Drift Plains",
    TRUE ~ us_l3name
  )) %>%
  write_csv("outputs/objective_2/sens_results_infilling_l3.csv") %>%
  merge(l3_eco, by = "us_l3name") %>%
  mutate(sig = ifelse(perc_wc_p_value > 0.05, "non_sig", "sig"))

top_left <- 
  ggplot(data = top_left_data) +
  geom_sf(color = "black",
          aes(geometry = geometry,
              fill = ifelse(sig == "non_sig", NA, perc_wc_slope))) +
  geom_sf(data = us_map, fill = NA, color = "grey60",
          size = 0.5, inherit.aes = F) +
  geom_sf(color = "black", fill = NA, size = 0.5, aes(geometry = geometry)) +
  scale_fill_steps2(mid = "white", high = colorspace::darken("#a68541", 0.3),
                    midpoint = 0, na.value = "grey",
                    name = "Sen's Slope\n(%/year)",
                    n.breaks = 6) +
  labs(title = "a. Infilling") + theme_void() +
  geom_point(aes(size = "n.s.", x = 1, y = 1),
             shape = NA, colour = "grey")+
  guides(fill = guide_colorsteps(order = 1, even.steps = TRUE),
         size = guide_legend(NULL, override.aes=list(shape = 15, size = 8),
                             order = 2)) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
  coord_sf(xlim = c(st_bbox(l3_eco)["xmin"], st_bbox(l3_eco)["xmax"]),
           ylim = c(st_bbox(l3_eco)["ymin"], st_bbox(l3_eco)["ymax"])) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())

#### UPPER RIGHT PANEL ####
top_right_data <-
  sens_results %>%
  filter(change_type == "Conversion") %>%
  mutate(us_l3name = case_when(
    us_l3name == "Huron/Erie Lake Plains" ~ "Huron Erie Lake Plains",
    us_l3name == "Southeastern Wisconsin Till Plains" ~ "SE Wisconsin Till Plains",
    us_l3name == "Southern Michigan/Northern Indiana Drift Plains" ~ "S MI/N IN Drift Plains",
    TRUE ~ us_l3name
  )) %>%
  write_csv("outputs/objective_2/sens_results_conversion_l3.csv") %>%
  merge(l3_eco, by = "us_l3name") %>%
  mutate(sig = ifelse(perc_wc_p_value > 0.05, "non_sig", "sig"))

top_right <-
  ggplot(data = top_right_data) +
  geom_sf(color = "black",
          aes(geometry = geometry,
              fill = ifelse(sig == "non_sig", NA, perc_wc_slope))) +
  geom_sf(data = us_map, fill = NA, color = "grey60",
          size = 0.5, inherit.aes = F) +
  geom_sf(color = "black", fill = NA, size = 0.5, aes(geometry = geometry)) +
  scale_fill_steps2(mid = "white", high = "#288078",
                    midpoint = 0, na.value = "grey",
                    name = "Sen's Slope\n(%/year)",
                    n.breaks = 6) +
  labs(title = "b. Conversion") + theme_void() +
  geom_point(aes(size = "n.s.", x = 1, y = 1),
             shape = NA, colour = "grey") +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
  guides(fill = guide_colorsteps(order = 1, even.steps = TRUE),
         size = guide_legend(NULL, override.aes=list(shape = 15, size = 8),
                             order = 2)) +
  coord_sf(xlim = c(st_bbox(l3_eco)["xmin"], st_bbox(l3_eco)["xmax"]),
           ylim = c(st_bbox(l3_eco)["ymin"], st_bbox(l3_eco)["ymax"]))

#### LOWER LEFT PANEL  ####
all_data <- c("nwood_forest_change_data.csv", 
              "woody_encroach_change_data.csv", 
              "forest_change_data.csv", 
              "nwood_transitional_change_data.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(class_01 = str_trim(str_remove(class_01, "Forest")),
         class_21 = str_trim(str_remove(class_21, "Forest")),
         change_type = case_when(class_21 %in%
                                   c("Deciduous", "Evergreen", "Mixed") & 
                                   class_01 %in%
                                   c("Deciduous", "Evergreen", "Mixed") ~
                                   "Infilling", TRUE ~ "Conversion"))

fig_data <- all_data %>%
  group_by(change_type, us_l3name) %>%
  summarize(area_change = sum(area_change)) %>%
  pivot_wider(names_from = change_type, values_from = area_change) %>%
  mutate(ratio_change = (Conversion - Infilling) / (Conversion + Infilling),
         diff_change = abs(Conversion - Infilling)) %>%
  mutate(us_l3name = case_when(us_l3name == "Huron/Erie Lake Plains" ~
                                 "Huron Erie Lake Plains",
                               TRUE ~ us_l3name))

l3_eco <-
  # Windows path
  #st_read("C:/Users/noah.weidig/OneDrive - University of Florida/thesis/data/us_eco_l3/us_eco_l3.shp") %>%
  # Mac path
  st_read("~/OneDrive - University of Florida/thesis/data/us_eco_l3/us_eco_l3.shp") %>%
  filter(NA_L1CODE == 8) %>%
  group_by(US_L3NAME) %>%
  summarize(geometry = st_union(geometry)) %>%
  # replace / with space in US_L3NAME
  mutate(US_L3NAME = str_replace_all(US_L3NAME, "/", " "),
         US_L3NAME = str_replace_all(US_L3NAME, c(
         "Southern Michigan Northern Indiana Drift Plains" =
           "S MI/N IN Drift Plains",
         "Southeastern Wisconsin Till Plains" =
           "SE Wisconsin Till Plains"))) %>%
  rename(us_l3name = US_L3NAME)

fig_data_shp <- left_join(fig_data, l3_eco, by = "us_l3name")

bot_left <- 
  ggplot(fig_data_shp) +
  geom_sf(aes(fill = ratio_change, geometry = geometry), color = "black") +
  geom_sf(data = us_map, fill = NA, color = "grey60",
          size = 0.5, inherit.aes = F) +
  geom_sf(color = "black", fill = NA, size = 0.5, aes(geometry = geometry)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm")) +
  labs(fill = NULL, title = "c. Ratio of Woody Cover Area Added") +
  scale_fill_gradient2(low = colorspace::darken("#a68541", 0.3), mid = "grey90", high = "#288078",
                       limits = c(-1, 1), breaks = c(-1, 0, 1),
                       labels = c("Infilling", "0", "Conversion")) +
    guides(fill = guide_colorbar(ticks = FALSE)) +
  coord_sf(xlim = c(st_bbox(l3_eco)["xmin"], st_bbox(l3_eco)["xmax"]),
           ylim = c(st_bbox(l3_eco)["ymin"], st_bbox(l3_eco)["ymax"]))

bar_plot_data_l1 <-
  c("nwood_forest_change_data_all.csv", 
    "woody_encroach_change_data_all.csv", 
    "forest_change_data_all.csv", 
    "nwood_transitional_change_data_all.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(across(starts_with("class_"), str_trim),
         convers_type = case_when(
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Conversion",
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             !str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Early",
           TRUE ~ "Infilling")) %>%
  group_by(us_l3name, convers_type, year) %>%
  summarize(area_wc = sum(area_wc), .groups = "drop")

############
infill_convers <-
  c("nwood_forest_change_data_all.csv", 
    "woody_encroach_change_data_all.csv", 
    "forest_change_data_all.csv", 
    "nwood_transitional_change_data_all.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(across(starts_with("class_"), str_trim),
         convers_type = case_when(
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Conversion",
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             !str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Early",
           TRUE ~ "Infilling")) %>%
  group_by(convers_type, year) %>%
  summarize(area_ha = sum(area_ha),
            area_wc = sum(area_wc)) %>%
  mutate(perc_wc = area_wc / area_ha * 100) %>%
  arrange(year)

infill_convers2 <- infill_convers %>%
  filter(year %in% c(2001, 2021)) %>%
  select(year, convers_type, area_wc, perc_wc) %>%
  pivot_wider(names_from = year,
              values_from = c(area_wc, perc_wc)) %>%
  mutate(diff_area_wc = `area_wc_2021` - `area_wc_2001`,
         diff_perc_wc = `perc_wc_2021` - `perc_wc_2001`)

infill_convers_results <- infill_convers %>%
  group_by(convers_type) %>%
  summarise(perc_wc_slope = sens.slope(perc_wc)$estimates,
            perc_wc_p_value = sens.slope(perc_wc)$p.value)

# merge wc_change_l1 with sen_results_l1
infill_convers_merge <- infill_convers2 %>%
  merge(infill_convers_results, by = "convers_type") %>%
  write_csv("outputs/objective_2/sens_results_comparison_l1.csv")

############
infill_convers <-
  c("nwood_forest_change_data_all.csv", 
    "woody_encroach_change_data_all.csv", 
    "forest_change_data_all.csv", 
    "nwood_transitional_change_data_all.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(across(starts_with("class_"), str_trim),
         convers_type = case_when(
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Conversion",
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             !str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Early",
           TRUE ~ "Infilling")) %>%
  group_by(convers_type, year, us_l3name) %>%
  summarize(area_ha = sum(area_ha),
            area_wc = sum(area_wc)) %>%
  mutate(perc_wc = area_wc / area_ha * 100) %>%
  arrange(year)

infill_convers2 <- infill_convers %>%
  filter(year %in% c(2001, 2021)) %>%
  select(year, convers_type, area_wc, perc_wc, us_l3name) %>%
  pivot_wider(names_from = year,
              values_from = c(area_wc, perc_wc)) %>%
  mutate(diff_area_wc = `area_wc_2021` - `area_wc_2001`,
         diff_perc_wc = `perc_wc_2021` - `perc_wc_2001`)

infill_convers_results <- infill_convers %>%
  group_by(convers_type, us_l3name) %>%
  summarise(perc_wc_slope = sens.slope(perc_wc)$estimates,
            perc_wc_p_value = sens.slope(perc_wc)$p.value)

# merge wc_change_l1 with sen_results_l3
infill_convers_merge <- infill_convers2 %>%
  merge(infill_convers_results, by = c("convers_type", "us_l3name")) %>%
  write_csv("outputs/objective_2/sens_results_comparison_l3.csv")
############

bar_plot_data_l1_results <- bar_plot_data_l1 %>%
  group_by(convers_type, year) %>%
  summarize(area_wc = sum(area_wc), .groups = "drop") %>%
  filter(year %in% c(2001, 2021)) %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(diff_change = `2021` - `2001`) %>%
  write_csv("outputs/bar_plot_data_l1_results.csv")

bar_plot_data_l3_results <- bar_plot_data_l1 %>%
  group_by(us_l3name, convers_type, year) %>%
  summarize(area_wc = sum(area_wc), .groups = "drop") %>%
  filter(year %in% c(2001, 2021)) %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(diff_change = `2021` - `2001`) %>%
  write_csv("outputs/bar_plot_data_l3_results.csv")

bar_plot_data <- bar_plot_data_l1 %>%
  filter(year %in% c(2001, 2021)) %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(diff_change = `2021` - `2001`) %>%
  group_by(us_l3name, convers_type) %>%
  summarize(total_positive = sum(diff_change[diff_change > 0], na.rm = TRUE),
            total_negative = sum(diff_change[diff_change < 0], na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(total_positive, total_negative),
               names_to = "change_category",
               values_to = "total_change") %>%
  filter(total_change >= 0) %>%
  group_by(us_l3name) %>%
  mutate(perc_change = total_change / sum(abs(total_change)) * 100)

# Create a named vector for state names and their abbreviations
state_abbrs <- setNames(as.list(state.abb), state.name)

# Function to replace state names with abbreviations
replace_state_abbr <- function(text) {
  for (state in names(state_abbrs)) {
    text <- str_replace_all(text, fixed(state), state_abbrs[state])
  }
  return(text)
}

bot_right <- bar_plot_data %>%
  mutate(type = ifelse(convers_type == "Infilling", "Infilling", "Conversion"),
         convers_type = factor(convers_type,
                               levels = c("Infilling", "Conversion", "Early")),
         us_l3name = str_replace_all(us_l3name, c(
           "(?i)northeastern " = "NE ", "(?i)northwestern " = "NW ",
           "(?i)southeastern " = "SE ", "(?i)southwestern " = "SW ",
           "(?i)northcentral " = "NC ", "(?i)southcentral " = "SC ",
           "(?i)eastcentral " = "EC ", "(?i)westcentral " = "WC ",
           "(?i)northern " = "N ", "(?i)southern " = "S ",
           "(?i)eastern " = "E ", "(?i)western " = "W ",
           "(?i)north " = "N ", "(?i)south " = "S ",
           "(?i)east " = "E ", "(?i)west " = "W ",
           "(?i)central " = "C ", "(?i)interior " = "Int. ",
           "(?i)coastal " = "Coast. ", "(?i)atlantic " = "Atl.",
           "(?i)middle " = "Mid ")),
    us_l3name = factor(sapply(us_l3name, replace_state_abbr),
                       levels = sort(unique(sapply(us_l3name,
                                                   replace_state_abbr))))) %>%
  ggplot(aes(x = perc_change / 100, y = fct_rev(us_l3name),
             fill = convers_type, color = type)) +
  geom_col(position = "stack", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 8)) +
  labs(x = "Percent of Woody Cover Area Added",
       fill = NULL, color = NULL, y = NULL,
       title = "d. Sources of Woody Cover") +
  scale_fill_manual(values = c("Early" = "#4FB8A8",
                               "Conversion" = "#288078",
                               "Infilling" = colorspace::darken("#a68541", 0.15))) +
  scale_color_manual(values = c("Conversion" = "#288078",
                                "Infilling" = colorspace::darken("#a68541", 0.15)),
                     guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = 0)) +
  scale_y_discrete(labels = scales::wrap_format(35)) +
  guides(fill = guide_legend(override.aes = list(color = c(colorspace::darken("#a68541", 0.15),
                                                           "#288078",
                                                           "#288078"))))

#### COMBINED PLOT ####
ragg::agg_png("outputs/objective_2/objective_2_fig.png", res = 600, width = 169, height = 190,
              units = "mm", bg = "white", scaling = 0.7)
top_left + top_right + bot_left + bot_right + plot_layout(ncol = 2, nrow = 2)
dev.off()

#### FIGURE 2 ####
bar_plot_data2_l1 <-
  c("nwood_forest_change_data_all.csv", 
    "woody_encroach_change_data_all.csv", 
    "forest_change_data_all.csv", 
    "nwood_transitional_change_data_all.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(class_01 = case_when(class_01 == "Herbaceous Wetlands" ~
                               "Emergent Herbaceous Wetlands",
                             class_01 == "Grassland Herbaceous" ~
                               "Grassland/Herbaceous",
                             class_01 == "Shrub Scrub" ~
                               "Shrub/Scrub", 
                             class_01 == "Pasture Hay" ~
                               "Pasture/Hay", TRUE ~ class_01)) %>%
  mutate(across(starts_with("class_"), str_trim),
         convers_type = case_when(
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Conversion",
           !str_detect(class_01, "Deciduous|Evergreen|Mixed") & 
             !str_detect(class_21, "Deciduous|Evergreen|Mixed") ~ "Early",
           TRUE ~ "Infilling")) %>%
  group_by(us_l3name, convers_type, class_01, year) %>%
  summarize(area_wc = sum(area_wc), .groups = "drop")

bar_plot_data2_l1_results <- bar_plot_data2_l1 %>%
  group_by(convers_type, class_01, year) %>%
  summarize(area_wc = sum(area_wc), .groups = "drop") %>%
  filter(year %in% c(2001, 2021)) %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(diff_change = `2021` - `2001`) %>%
  group_by(convers_type) %>%
  mutate(total_area_wc = sum(diff_change, na.rm = TRUE),
         perc_change = diff_change / total_area_wc) %>%
  ungroup() %>%
  write_csv("outputs/objective_2/bar_plot_data2_l1_results.csv")

ggplot() +
  geom_col(data = bar_plot_data2_l1_results,
           aes(x = perc_change, y = convers_type, fill = class_01),
           position = "stack", width = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_manual(values = c("Deciduous Forest" = "#68ab5f",
                               "Evergreen Forest" = "#1c5f2c",
                               "Mixed Forest" = "#b5c58f",
                               "Cultivated Crops" = "#ab6c28",
                               "Grassland/Herbaceous" = "#dfdfc2",
                               "Emergent Herbaceous Wetlands" = "#6c9fb8",
                               "Pasture/Hay" = "#dcd939",
                               "Shrub/Scrub" = "#ccb879",
                               "Woody Wetlands" = "#b8d9eb")) +
  labs(x = "Percent of woody cover area added by land use type",
       fill = NULL, color = NULL, y = NULL)

ggsave("outputs/objective_2/bar_plot_data2_l1_results.jpg", width = 9, height = 6)

################################################################################
############################## CHORD DIAGRAM ###################################
################################################################################
pacman::p_load(tidyverse, ggfittext, migest, magick, pdftools, extrafont)

chord_data_l1 <-
  c("nwood_forest_change_data_all.csv", 
    "woody_encroach_change_data_all.csv", 
    "forest_change_data_all.csv", 
    "nwood_transitional_change_data_all.csv") %>% 
  map(~ read_csv(file.path("outputs", .x), show_col_types = F)) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(class_01 =
           case_when(class_01 == "Herbaceous Wetlands" ~ "Emergent Herbaceous Wetlands",
                              TRUE ~ class_01)) %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(area_wc = `2021` - `2001`) %>%
  group_by(class_01, class_21) %>%
  summarise(area_wc = sum(area_wc, na.rm = TRUE), .groups = "drop") %>%
  select(orig = class_01, dest = class_21, flow = area_wc)

options(scipen = -100)

r <- chord_data_l1 %>%
  sum_region() %>%
  mutate(lab = str_wrap_n(string = region, n = 2)) %>%
  separate(col = lab, into = c("lab1", "lab2"), sep = "\n",
           remove = FALSE, fill = "right")

loadfonts(device = "pdf")  # Load fonts for PDF output

pdf(file = "outputs/objective_2/l1_chord.pdf")
mig_chord(x = chord_data_l1,
          order = unique(chord_data_l1$orig)[c(2,4,6,1,7,3,9,5,8)],
          label_size = 1,
          axis_size = 0.73,
          axis_breaks = 1500000,
          gap.degree = 6,
          lab_bend1 = r %>%
            select(region, lab1) %>%
            deframe(),
          lab_bend2 = r %>%
            select(region, lab2) %>%
            deframe(),
          grid.col =  c( "#85C77E", "#38814E", "#D4E7B0", "#CA9146",
                         "#FBF65D", "#64B3D5", "#C8E6F8", "#FDE9AA", "#DCCA8F"))
dev.off()

p <- image_read_pdf("outputs/objective_2/l1_chord.pdf")
image_write(image = p, path = "outputs/objective_2/l1_chord.png")
file.show("outputs/objective_2/l1_chord.png")
options(scipen = 0)
################################################################################
############################## L3 ECOS #########################################
pacman::p_load(data.table, tidyverse, ggfittext, migest, magick,
               pdftools, extrafont, officer, fs)

chord_data_l3_all <-
  c("nwood_forest_change_data_all.csv", 
    "woody_encroach_change_data_all.csv", 
    "forest_change_data_all.csv", 
    "nwood_transitional_change_data_all.csv") %>% 
  map(~ fread(file.path("outputs", .x))) %>%
  bind_rows() %>%
  separate(change_class, c("class_01", "class_21"), sep = " to ") %>%
  mutate(class_01 =
           case_when(class_01 == "Herbaceous Wetlands" ~ "Emergent Herbaceous Wetlands",
                     TRUE ~ class_01))

rel_change_l3 <- chord_data_l3_all %>%
  filter(year %in% c(2001, 2021)) %>%
  group_by(us_l3name, year) %>%
  summarise(area_wc = sum(area_wc, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(us_l3name = str_replace_all(us_l3name, "/", " "),
         rel_change = (`2021` - `2001`) / `2001`)

chord_data_l3 <- chord_data_l3_all %>%
  group_by(us_l3name) %>%
  group_split() %>%
  set_names(map_chr(., ~ str_replace_all(unique(.x$us_l3name), "/", " "))) %>% 
  map(~ .x %>%
        pivot_wider(names_from = year, values_from = area_wc) %>%
        mutate(area_wc = .data[[as.character(2021)]] -
                 .data[[as.character(2001)]]) %>%
        group_by(class_01, class_21) %>%
        summarise(area_wc = sum(area_wc, na.rm = TRUE), .groups = "drop") %>%
        select(orig = class_01, dest = class_21, flow = area_wc))

loadfonts(device = "pdf")

chord_data_l3 %>%
  walk2(names(chord_data_l3), ~ {
    r <- .x %>%
      sum_region() %>%
      mutate(lab = str_wrap_n(string = region, n = 2)) %>%
      separate(col = lab, into = c("lab1", "lab2"), sep = "\n", remove = FALSE, fill = "right")
    us_l3name <- .y
    rel_change_value <- rel_change_l3 %>%
      filter(us_l3name == !!us_l3name) %>%
      pull(rel_change) %>%
      round(3)
    name <- to_snake_case(gsub("/", " ", us_l3name))
    pdf_path <- paste0("outputs/objective_2/chords/chord_", name, ".pdf")
    png_path <- paste0("outputs/objective_2/chords/chord_", name, ".png")
    total_flow <- scales::comma(round(sum(.x$flow), 0))
    
    pdf(file = pdf_path)
    mig_chord(x = .x,
              order = unique(.x$orig)[c(2,4,6,1,7,3,9,5,8)],
              label_size = 0.78,
              label_nudge = 0.1,
              no_axis = TRUE,
              gap.degree = 8,
              lab = r %>%
                select(region, lab) %>%
                deframe(),
              preAllocateTracks = list(track.height = 0.25),
              grid.col = c("#85C77E", "#38814E", "#D4E7B0", "#CA9146",
                           "#FBF65D", "#64B3D5", "#C8E6F8", "#FDE9AA", "#DCCA8F"))
    dev.off()
    p <- image_read_pdf(pdf_path, density = 200) %>%
      image_annotate(paste0("    ", us_l3name,
                           "\n    Total woody cover added = ",
                           total_flow, " ha",
                           "\n    Relative change = ",
                           (rel_change_value * 100), "%"), 
                     size = 12, color = "black", location = "+0+10") %>%
    image_write(image = ., path = png_path)
    unlink("outputs/objective_2/chords/*.pdf", recursive = TRUE)
  })

################################################################################
# make into a bar plot
bar_plot_data2_l1_results %>%
  write_csv("outputs/bar_plot_data2_l1_results.csv")

bar_plot_data2 <- bar_plot_data2_l1 %>%
  filter(year %in% c(2001, 2021)) %>%
  pivot_wider(names_from = year, values_from = area_wc) %>%
  mutate(diff_change = .data[[as.character(2021)]] -
           .data[[as.character(2001)]]) %>%
  filter(diff_change >= 0) %>%
  group_by(us_l3name, convers_type) %>%
  mutate(total_area_wc = sum(diff_change, na.rm = TRUE),
         perc_change = diff_change / total_area_wc) %>%
  ungroup()

fig_2a <-
  bar_plot_data2 %>%
  mutate(type = ifelse(convers_type == "Infilling", "Infilling", "Conversion"),
         convers_type = factor(convers_type,
                               levels = c("Infilling", "Conversion", "Early"))) %>%
  mutate(us_l3name = str_replace_all(us_l3name, 
                                     c("(?i)northeastern " = "NE ", "(?i)northwestern " = "NW ", 
                                       "(?i)southeastern " = "SE ", "(?i)southwestern " = "SW ", 
                                       "(?i)northcentral " = "NC ", "(?i)southcentral " = "SC ", 
                                       "(?i)eastcentral " = "EC ", "(?i)westcentral " = "WC ",
                                       "(?i)northern " = "N ", "(?i)southern " = "S ", 
                                       "(?i)eastern " = "E ", "(?i)western " = "W ", 
                                       "(?i)north " = "N ", "(?i)south " = "S ", 
                                       "(?i)east " = "E ", "(?i)west " = "W ",
                                       "(?i)central " = "C ", "(?i)interior " = "Int. ",
                                       "(?i)coastal " = "Coast. ", "(?i)atlantic " = "Atl. ",
                                       "(?i)middle " = "Mid "))) %>%
  mutate(us_l3name = sapply(us_l3name, replace_state_abbr),
         us_l3name = factor(us_l3name, levels = rev(sort(unique(us_l3name))))) %>%
  filter(type == "Infilling") %>%
  ggplot() +
  geom_col(aes(y = us_l3name, x = perc_change, fill = class_01),
           position = "stack", width = 0.5, show.legend = TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.box.background = element_rect(color = "black",
                                             linewidth = 0.3)) +
  labs(x = "Percent of woody cover area added by land use type",
       fill = NULL, color = NULL, y = NULL, title = "a. Infilling") +
  scale_y_discrete(labels = scales::wrap_format(35)) +
  scale_fill_manual(values = c("Deciduous Forest" = "#68ab5f",
                               "Evergreen Forest" = "#1c5f2c",
                               "Mixed Forest" = "#b5c58f"),
                    limits = c("Deciduous Forest", "Evergreen Forest",
                               "Mixed Forest")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2)) +
  coord_cartesian(xlim = c(0, 1)) +
  guides(fill = guide_legend(ncol = 2))

fig_2b <-
  bar_plot_data2 %>%
  mutate(type = ifelse(convers_type == "Infilling", "Infilling", "Conversion"),
         convers_type = factor(convers_type,
                               levels = c("Infilling", "Conversion", "Early"))) %>%
  mutate(us_l3name = str_replace_all(us_l3name, 
                                     c("(?i)northeastern " = "NE ", "(?i)northwestern " = "NW ", 
                                       "(?i)southeastern " = "SE ", "(?i)southwestern " = "SW ", 
                                       "(?i)northcentral " = "NC ", "(?i)southcentral " = "SC ", 
                                       "(?i)eastcentral " = "EC ", "(?i)westcentral " = "WC ",
                                       "(?i)northern " = "N ", "(?i)southern " = "S ", 
                                       "(?i)eastern " = "E ", "(?i)western " = "W ", 
                                       "(?i)north " = "N ", "(?i)south " = "S ", 
                                       "(?i)east " = "E ", "(?i)west " = "W ",
                                       "(?i)central " = "C ", "(?i)interior " = "Int. ",
                                       "(?i)coastal " = "Coast. ", "(?i)atlantic " = "Atl. ",
                                       "(?i)middle " = "Mid "))) %>%
  mutate(us_l3name = sapply(us_l3name, replace_state_abbr),
         us_l3name = factor(us_l3name, levels = rev(sort(unique(us_l3name))))) %>%
  filter(convers_type == "Conversion") %>%
  ggplot() +
  geom_col(aes(y = us_l3name, x = perc_change, fill = class_01),
           position = "stack", width = 0.5, show.legend = TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 0.5)) +
  labs(x = "Percent of woody cover area added by land use type",
       fill = NULL, color = NULL, y = NULL, title = "b. Conversion") +
  scale_y_discrete(labels = scales::wrap_format(35)) +
  scale_fill_manual(values = c("Cultivated Crops" = "#ab6c28",
                               "Grassland/Herbaceous" = "#dfdfc2",
                               "Emergent Herbaceous Wetlands" = "#6c9fb8",
                               "Pasture/Hay" = "#dcd939",
                               "Shrub/Scrub" = "#ccb879",
                               "Woody Wetlands" = "#b8d9eb"),
                    limits = c("Cultivated Crops", "Grassland/Herbaceous",
                               "Emergent Herbaceous Wetlands", "Pasture/Hay",
                               "Shrub/Scrub", "Woody Wetlands")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2)) +
  coord_cartesian(xlim = c(0, 1))

fig_2c <-
  bar_plot_data2 %>%
  mutate(type = ifelse(convers_type == "Infilling", "Infilling", "Conversion"),
         convers_type = factor(convers_type,
                               levels = c("Infilling", "Conversion", "Early"))) %>%
  mutate(us_l3name = str_replace_all(us_l3name, 
                                     c("(?i)northeastern " = "NE ", "(?i)northwestern " = "NW ", 
                                       "(?i)southeastern " = "SE ", "(?i)southwestern " = "SW ", 
                                       "(?i)northcentral " = "NC ", "(?i)southcentral " = "SC ", 
                                       "(?i)eastcentral " = "EC ", "(?i)westcentral " = "WC ",
                                       "(?i)northern " = "N ", "(?i)southern " = "S ", 
                                       "(?i)eastern " = "E ", "(?i)western " = "W ", 
                                       "(?i)north " = "N ", "(?i)south " = "S ", 
                                       "(?i)east " = "E ", "(?i)west " = "W ",
                                       "(?i)central " = "C ", "(?i)interior " = "Int. ",
                                       "(?i)coastal " = "Coast. ", "(?i)atlantic " = "Atl. ",
                                       "(?i)middle " = "Mid "))) %>%
  mutate(us_l3name = sapply(us_l3name, replace_state_abbr),
         us_l3name = factor(us_l3name, levels = rev(sort(unique(us_l3name))))) %>%
  filter(convers_type == "Early") %>%
  ggplot() +
  geom_col(aes(y = us_l3name, x = perc_change, fill = class_01),
           position = "stack", width = 0.5, show.legend = TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 0.5)) +
  labs(x = "Percent of woody cover area added by land use type",
       fill = NULL, color = NULL, y = NULL, title = "c. Early Signals") +
  scale_y_discrete(labels = scales::wrap_format(35)) +
  scale_fill_manual(values = c("Cultivated Crops" = "#ab6c28",
                               "Grassland/Herbaceous" = "#dfdfc2",
                               "Emergent Herbaceous Wetlands" = "#6c9fb8",
                               "Pasture/Hay" = "#dcd939",
                               "Shrub/Scrub" = "#ccb879",
                               "Woody Wetlands" = "#b8d9eb"),
                    limits = c("Cultivated Crops", "Grassland/Herbaceous",
                               "Emergent Herbaceous Wetlands", "Pasture/Hay",
                               "Shrub/Scrub", "Woody Wetlands")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2)) +
  coord_cartesian(xlim = c(0, 1))

bc <- fig_2b + fig_2c + plot_layout(ncol = 2, nrow = 1,
                                    guides = "collect", axis_titles = "collect") &
  theme(axis.text.y = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 0.3),
        axis.title.x = element_blank())

ragg::agg_png("outputs/objective_2/objective_2_bar_plots.png", res = 800, width = 9, height = 5, units = "in", bg = "white", scaling = 0.8)
fig_2a + bc +
  plot_layout(ncol = 2, nrow = 1, guides = "keep",
              axis_titles = "collect_x", widths = c(0.95, 2)) &
  theme(legend.position = "bottom", legend.spacing.x = unit(0.5, "cm"),
        axis.title.x = element_text(hjust = -5))
dev.off()
