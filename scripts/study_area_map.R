pacman::p_load(sf, terra, tidyterra, scales, tidyverse, skimr, paletteer,
               flextable, glue, RColorBrewer, ggspatial, ggrepel)

eco_code <- 8
ecorg <- vect("data/us_eco_l3/us_eco_l3.shp")

# Select only the Great Plains, which is NA_L1CODE 9
east_temp <- ecorg[ecorg$NA_L1CODE == eco_code, ]

bbox <- st_bbox(east_temp)
us <- st_read(glue("data/us_eco_l3_state_boundaries/",
                   "us_eco_l3_state_boundaries.shp")) %>%
  group_by(STATE_NAME) %>%
  summarise()

east_temp_plt <- east_temp %>%
  mutate(US_L3NAME = paste0(dense_rank(US_L3NAME), " ", US_L3NAME)) %>%
  mutate(rank_num = as.integer(str_extract(US_L3NAME, "^\\d+"))) %>%
  arrange(rank_num) %>%
  mutate(US_L3NAME = factor(US_L3NAME, levels = unique(US_L3NAME))) %>%
  select(-rank_num) %>%
  st_as_sf() %>%
  group_by(US_L3NAME) %>%
  summarise(geometry = st_union(geometry))

ggplot() +
  geom_sf(east_temp_plt,
                  mapping = aes(geometry = geometry, fill = US_L3NAME),
                  color = "black", show.legend = T) +
  geom_sf(data = us, fill = NA, color = "grey", show.legend = F) +
  geom_sf(east_temp_plt, mapping = aes(geometry = geometry),
                  fill = NA, color = "black") +
  geom_text_repel(data = east_temp_plt %>% distinct(US_L3NAME,
                                                    .keep_all = TRUE),
                  aes(label = str_extract(US_L3NAME, "^\\d+"),
                      geometry = geometry), show.legend = T,
    color = "white", bg.color = "grey30", bg.r = 0.08,
    stat = "sf_coordinates", point.size = NA, size = 3) +
  labs(fill = "Level III Ecoregions") +
  theme_void() +
  scale_fill_manual(values = paletteer_c("ggthemes::Temperature Diverging", 33),
                    labels = function(labels)
                      str_wrap(str_replace(labels, "^\\d+\\s", ""), width = 32)) +
  guides(fill = guide_legend(override.aes = list(color = "white",
                                                 color.stroke = "black",
                                                 label = c(1:33),
                                                 size = 2))) +
  theme(legend.text = element_text(size = 6),
        legend.key = element_rect(color = "white"),
        legend.key.size = unit(0.5, "cm")) +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = ggspatial::north_arrow_minimal()) +
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.6)

ggsave("outputs/study_region_map.jpg", width = 8, height = 5, dpi = 600)
