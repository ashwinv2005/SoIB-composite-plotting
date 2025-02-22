library(tidyverse)

map = read.csv("SoIB_mapping_2023.csv")
taxmap = read.csv("eBird_taxonomy_mapping.csv")

past_main = read.csv("SoIB_main.csv") %>%
  rename(IUCN.Category.Old = IUCN.Category)

map = map %>%
  dplyr::select(eBird.English.Name.2023,
                IUCN.Category)

past_main = past_main %>%
  left_join(taxmap) %>%
  left_join(map)

test = past_main %>%
  distinct(eBird.English.Name.2022,IUCN.Category.Old,IUCN.Category) %>%
  filter(IUCN.Category.Old != IUCN.Category)

new_main = past_main %>%
  mutate(IUCN.Category.Old = IUCN.Category) %>%
  dplyr::select(-eBird.English.Name.2023,-IUCN.Category) %>%
  rename(IUCN.Category = IUCN.Category.Old)

write.csv(new_main,"SoIB_main.csv",row.names=F)
