library(tidyverse)

sst = read_csv("data/sst.csv")
sst %>%
  head()

my_countries = c("Greece", "Turkey", "Cyprus", "Lebanon", "Egypt", "Libya")

mapdata = map_data("world") %>%
  arrange(group) %>% 
  select(lon = long, lat, group, region = region) 

country.maps.labels <- mapdata %>%
  group_by(region) %>%
  filter(region %in% my_countries) %>%
  summarise(long = mean(lon), lat = mean(lat), group = first(group))

ggplot(mapdata, aes(lon, lat, group = group)) +
  geom_polygon(aes(group = group, fill = region), fill = "white", colour = "grey50") + 
  geom_point(aes(x = lon, y = lat, group = Type,
                 colour = Type),  data = sst, size=1.5) +
  geom_text(data = country.maps.labels, aes(x = long, y = lat, label = region, group = group)) +
  coord_fixed(ratio=1.6, xlim = c(23, 38), ylim = c(30,37.5))

ggsave("figures/mediterranean-sea.png", width=8, height=5.5)
