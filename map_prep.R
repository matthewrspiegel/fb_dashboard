library(tigris)
options(tigris_use_cache = FALSE)
options(tigris_class = "sf")


zips <- tigris::zctas(state = "FL")
zips_fl <- zips %>% 
  filter(ZCTA5CE10 %in% as.character(health_data$zip))


map_data <- health_data %>%
  left_join(zip_data, by = "zip")


map_data <- map_data %>% 
  gather("condition", "value", -c(patient_id:er_count, er_copay:zip)) %>% 
  filter(value == 1) %>% 
  select(
    zip,
    condition,
    hp_paid
  )

total_cost_zip <- map_data %>% 
  bind_rows(mutate(., condition = "All")) %>% 
  group_by(zip, condition) %>% 
  summarise(total_cost = sum(hp_paid) / n())

save(zips, total_cost_zip, file = "zips.Rdata")

sel_condition <- "cc_arthritis"


condition_zip <- total_cost_zip %>% 
  filter(condition == sel_condition) %>% 
  ungroup() %>% 
  mutate_at("zip", as.character)

map_condition <- zips %>% 
  inner_join(condition_zip, by = c("ZCTA5CE10" = "zip"))

library(leaflet)

pal <- colorNumeric(
  palette = "Reds",
  domain = map_condition$total_cost)

leaflet(map_condition) %>% 
  addPolygons(
    fillColor = ~pal(total_cost), 
    popup = ~glue::glue("Average cost per patient: {scales::dollar(total_cost)}"),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7
  ) %>% 
  addProviderTiles('CartoDB.Positron')



