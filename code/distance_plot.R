#drawning maps
library(dplyr)
library(ggplot2)

# Data frame with the cities
data = rbind(
  Belo_Horizonte = c(-43.9386,-19.9191), 
  Bogota = c(-74.0836,4.6369), 
  Cidade_do_México = c(-99.1331,19.4335),
  Durham = c(-78.9391,36.0013),
  Santiago = c(-70.6121, -33.4643)) %>% 
  as.data.frame()
colnames(data) = c("long","lat")

#draw map
mapdata = map_data("world") %>%
  arrange(group) %>% 
  select(lon = long, lat, group, region = region) 


ggplot(mapdata, aes(lon, lat, group = group)) +
  geom_polygon(aes(group = group, fill = region), fill = "white", colour = "grey50") + 
  geom_point(aes(x = long, y = lat, group=NULL), data = data, size=1.5) +
  geom_line(aes(x = long, y = lat, group=1), data = data,) +
  coord_fixed(ratio=0.9, ylim=c(-80,80), xlim = c(-180,-10)) 

#graph straight line
ggsave("figures/line_straight.png", width=8, height=5.5)


# using groups to connect one city to all others
n = nrow(data) - 1
new_data = data.frame(X = c(rep(data$long[1], n), data$long[-1]),
                      Y = c(rep(data$lat[1], n), data$lat[-1]))
new_data$grp = as.factor(rep(1:n, times = 2))
new_data


ggplot(mapdata, aes(lon, lat, group = group)) +
  geom_polygon(aes(group = group, fill = region), fill = "white", colour = "grey50") + 
  geom_point(aes(x = X, y = Y, group=grp), data = new_data, size=1.5) +
  geom_line(aes(x = X, y = Y, group=grp), data = new_data,) +
  coord_fixed(ratio=0.9, ylim=c(-80,80), xlim = c(-180,-10)) 

ggsave("figures/line_one_city_to_all.png", width=8, height=5.5)
