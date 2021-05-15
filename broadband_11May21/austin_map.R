install.packages(c("tidyverse","ggmap","zipcodeR","tigris","osmdata"))

options(tigris_use_cache=TRUE)
library(osmdata)
library(ggtext)
library(tidyverse)
library(tigris)
library(zipcodeR)
library(sysfonts)
library(ggthemes)


## get TT data
tues<-tidytuesdayR::last_tuesday()
tt<-tidytuesdayR::tt_load(tues)

wifi_zip<-tt$broadband_zip %>%
  janitor::clean_names() %>%
  mutate(zipcode=as.character(postal_code))


## get zipcodes for austin
dc<-zipcodeR::search_city("Austin","TX")

dc %>% select(zipcode, population) %>% filter(!is.na(population)) %>% count()
length(unique(dc$zipcode))


## get zipcode shapefiles for austin
zips_dc <- zctas(cb = T, starts_with = c("733","787"), class = "sf")

zips_dc<-zips_dc %>% select(zip=ZCTA5CE10, geometry)

#fewer zips overall bc of PO boxes etc
length(unique(zips_dc$zip))
dc %>% selct(zipcode, population) %>% filter(!is.na(population)) %>% count()
#both 44 zuips with pop

zips_map_data<-zips_dc %>% 
  inner_join(wifi_zip, by=c("zip"="zipcode"))


#mostly but not all Travis county!
zips_map_data %>% 
  count(county_name)


## OSM BASEMAPS

austin_streets<-getbb("austin") %>% 
  opq() %>% 
  add_osm_feature(key="highway", value=c("primary", "secondary")) %>% 
  osmdata_sf()

## where is I35??
austin_streets$osm_lines %>% 
  filter(str_detect(name, "35"))

austin_35<-getbb("austin") %>% 
  opq() %>% 
  add_osm_feature(key="name", value=c("South Interstate 35","North Interstate 35")) %>% 
  osmdata_sf()




ggplot() +
  geom_sf(data = austin_streets$osm_lines, color="black") +
  geom_sf(aes(fill=broadband_usage), data = zips_map_data, alpha=0.7) +
  geom_sf(data = austin_35$osm_lines, color="orange", size=1.5) +
  theme_map() +
  theme(legend.position = "bottom", legend.justification = "center") +
  scale_fill_continuous(labels=scales::label_percent()) +
  guides(fill = guide_colorbar(title.position  = 'top', 
                               title = 'Broadband Usage',
                               title.hjust = .5,
         barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))




## see if i can find which ones are East of 35 and assess visually

ggplot() +
  geom_sf(data = austin_streets$osm_lines, color="black") +
  geom_sf(data = zips_map_data, alpha=0.7) +
  # geom_sf(data = zips_map_data %>% filter(east==1), fill="blue",alpha=0.7) +
  geom_sf_text_repel(data = zips_map_data, aes(label = postal_code), colour = "black") +
  geom_sf(data = austin_35$osm_lines, color="orange", size=1.5) +
  ggrepel::geom_label_repel(
    data = zips_map_data,
    aes(label = zip, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "magenta",
    segment.colour = "magenta"
  ) 

# couuuullllld do this with geospatial magic
# probably easier just to eyeball and add by hand
east_zips<-c(47,19,44,41,02,22,53,54,23,21,42,25,24) + 78700

east_zips<-as.double(east_zips)


zips_map_data<-zips_map_data %>% 
          mutate("east"=if_else(postal_code %in% east_zips, 1, 0))
##uncomment codechunk above to see if found all zips E of 35


## Analyze pop E of 35

zips_map_data %>% 
  left_join(dc, by=c("zip"="zipcode")) %>% 
  mutate(broadband_pop=as.numeric(population*broadband_usage)) %>% 
  group_by(east) %>% 
  summarize(pop_using_broadband=sum(broadband_pop)/sum(population))

## 78.6 % of W has access to broadband; 56.6% of E


# final map

library(colorspace)
library(ggtext)
# font_add_google("Ubuntu","ubuntu")


finalmap<-ggplot() +
  geom_sf(data = austin_streets$osm_lines, color="black") +
  geom_sf(aes(fill=broadband_usage), data = zips_map_data, alpha=0.6) +
  geom_sf(data = austin_35$osm_lines, color="seagreen", size=1.5) +
  theme_map() +
  theme(legend.position = "bottom", legend.justification = "center",
        plot.title = element_markdown(family = "ubuntu", face="bold"),
        plot.subtitle = element_text(family = "ubuntu"),
        plot.caption = element_markdown(family = "ubuntu"),
        plot.background = element_rect(fill="ghostwhite",color=NA),
        legend.background = element_rect(fill="ghostwhite",color=NA),
        panel.background = element_rect(fill="ghostwhite", color=NA)) +
  scale_fill_continuous_sequential(palette="Lajolla", trans="reverse",
                                   labels=scales::label_percent()) +
  # scale_fill_colorblind() +
  # scale_fill_gradient2(labels=scales::label_percent(),
  #                     low="darkorange2",high="turquoise2", ## diff color palette
  #                     midpoint = mean(zips_map_data$broadband_usage)) +
  labs(title="Broadband Usage in The City of Austin, Texas",
       subtitle=str_wrap(paste0("Estimated share of population using",
                                " fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps",
                                " as of October 2020, by zip code"), width=55),
       caption="*Data: Microsoft*") +
  guides(fill = guide_colorbar(title.position  = 'top', 
                               title = 'Broadband Usage',
                               title.hjust = .5,
                               reverse=TRUE,
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines')))

finalmap

## add some labels to the top
labw_html <- "West of I-35<br><b style='color:coral4;'>78.6%</b>"
labe_html <- "East of I-35<br><b style='color:orange4;'>56.6%</b>"
lab35_html <- "<b style='color:seagreen;'>I-35</b>"

finalmap +
  geom_richtext(aes(x = -97.76, y = 30.533, label = labw_html),
                stat = "unique", fill=NA, label.color=NA) +
  geom_richtext(aes(x = -97.635, y = 30.533, label = labe_html),
                stat = "unique", fill=NA, label.color=NA) +
  geom_richtext(aes(x = -97.69, y = 30.525, label = lab35_html),
                stat = "unique", fill=NA, label.color=NA)

## save trough preview