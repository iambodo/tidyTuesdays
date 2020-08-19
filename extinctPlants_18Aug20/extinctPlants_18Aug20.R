library(tidyverse)
#install.packages("skimr")
library(skimr)
library(xml2)
library(httr)
library(purrr)
#install.packages("ragg")
library(ragg)
#install.packages("systemfonts")
library(systemfonts)
# df<-system_fonts() 
# df$family %>% unique  


tidytuesdayR::tt_load(Sys.Date()-1)

##Explore
data$plants %>% 
  group_by(red_list_category)

map(data, skim)


#get the data
plants<-data$plants %>% 
  filter(year_last_seen!="Before 1900")

nrow(plants)

##Mapping this function proved difficult....
threatmatrix <- function(df, column){
  
  #cooocurrence matrix!!
temp<-df %>% 
  filter(continent==column) %>%
  select(contains("threat"), contains("action")) %>% 
  as.matrix() %>% 
  crossprod()  

#print(temp)
diag(temp) <- 0 # (you don't count co-occurrences of an aspect with itself)

data_long<-as.data.frame(temp) %>%
  rownames_to_column("threat") %>% 
  filter(str_detect(threat, "threat")) %>% 
  select(threat, contains("action")) %>% 
  pivot_longer(contains("action"))

#print(class(data_long))

return(data_long)

}


#lapply(continents, threatmatrix(continents, plants))

#so just created a little for loop

continents<-unique(plants$continent)

data_all<-list()

for (i in 1:length(continents)){
  
  c<-continents[i]
  
  data_long<-threatmatrix(plants, c) %>% 
    mutate(continent=c)
  
  data_all[[i]]<-data_long
  
}


#Heres our data!!

plantdata<-jsonlite::rbind_pages(data_all) %>% 
  as_tibble()


##One way to get the action and threat long names...
url<-"https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-18/readme.md"
#Reading the HTML code from the website
varnames <- xml2::read_html(url) %>% 
  rvest::html_nodes("table") %>% 
  rvest::html_table()

#varnames[[1]]



#order x and y axis by global count and better labels
threat_labs<-plants %>% 
    pivot_longer(contains("threat"), names_to="threat", values_to ="count") %>% 
    filter(count==1) %>% 
    group_by(threat) %>% 
    count() %>% 
    arrange(n) %>% 
    left_join(varnames[[1]], by=c("threat"="variable")) %>% 
    mutate(threat_lab=str_remove(description, "Threat: ")) %>% 
  select(contains("threat"))

  
action_labs<-plants %>% 
  pivot_longer(contains("action"), names_to="action", values_to ="count") %>% 
  filter(count==1) %>% 
  group_by(action) %>% 
  count() %>% 
  arrange(-n) %>% 
  left_join(varnames[[1]], by=c("action"="variable")) %>% 
  mutate(action_lab=str_remove(description, "Current action: ")) %>% 
  select(contains("action"))



## First createe total threats per continent sheet
plant_threats <- plants %>% 
  select(continent, contains("threat")) %>% 
  add_count(continent, name="total_plants") %>% 
  group_by(continent, total_plants) %>%
  pivot_longer(contains("threat"), names_to="threat") %>% 
  group_by(continent, total_plants, threat) %>% 
  summarize(threat_count=sum(value)) %>% 
  mutate(threat_percent=threat_count/total_plants) %>% 
  left_join(plantdata, by=c("threat", "continent")) %>%  #now merge it with cooccurence
  rename("action"=name, "action_to_threat"=value) %>% 
  mutate("action_to_threat_percent"=action_to_threat/threat_count) %>% 
  replace(., is.na(.), 0) %>% 
  left_join(action_labs, by=c("action")) %>% 
  left_join(threat_labs, by=c("threat")) %>% 
  mutate(threat_lab=factor(threat_lab, levels=threat_labs$threat_lab, ordered = T)) %>% 
  mutate(action_lab=factor(action_lab, levels=action_labs$action_lab, ordered = T)) %>% 
  ungroup() %>% #reorder the continents geographically!
  mutate(continent=factor(continent, levels=c("North America", "Europe","Asia",
                                              "South America","Africa","Oceania"), ordered = T))

  

##Going to have to create a separate annotation overlay....

# line1<-plant_threats %>% 
#   ungroup() %>% 
#   distinct(action_lab) %>% 
#   mutate(continent="South America")
# line1$action_lab
# threat_labs$threat_lab



pchart<-ggplot()+
  geom_tile(data=plant_threats,
            aes(x=action_lab, y=threat_lab, fill=threat_percent)) +
  geom_point(data=plant_threats,
             aes(x=action_lab, y=threat_lab, size=action_to_threat_percent), 
             fill="#518610", colour="grey20",stroke=0.3, shape=21)+
  scale_fill_gradient(high="#654321", low="tan",
                      limits=c(0.0, 1), 
                      labels=scales::label_percent(), 
                      guide = guide_colourbar(title.position = "top", 
                                              label.position = "bottom",
                                              nbin=10, barwidth = 15,
                      title="Species Extinct with Threat (%)")) +
  scale_size_continuous(range=c(0.05, 12), limits=c(0.01, 1), 
                        labels=scales::label_percent(),
                        guide = guide_legend(title.position = "top", 
                                             label.position = "bottom",
                                              title="Threats Followed by Action (%)")) +
  facet_wrap(~continent, ncol=3) +
  theme_light() +
  scale_x_discrete(position="top")+
  labs(y="", x="",
       title="The 370 Plant Species Extinct Since 1900",
       subtitle="By Threats and Threat-Action Co-occurrence",
       caption="Data: IUCN | Design: @iambodo") +
  theme(legend.position = "bottom", legend.box = "horizontal", 
        legend.justification = "left", legend.box.just = "center",
        legend.spacing.y = unit(0.02, "npc"),
        axis.text.x = element_text(angle = 45, vjust = 0, hjust=0),
        text=element_text(family = 'Century Schoolbook', size=24),
        title=element_text(size=25),
        strip.background = element_rect(fill="#6b0c40"),
        strip.text = element_text(size=30))

pchart

#theme_set(plot.background = element_rect(fill="ivory1"))

          

#had to really fiddle to get this right!

overlay<-ggplot()+
  theme_void() +
#horiz
  annotate("segment", x=0.65,y=0, xend=0.92,yend=0, size=2,arrow = arrow(length = unit(0.03, "npc")))+
  annotate("text", x=0.8, y= -0.02,label="Total global actions",size=8) +
  annotate("text", x=0.85, y=0.02,label="Less",size=8) +
  annotate("text", x=0.7, y=0.02,label="More",size=8) +
#vert
  annotate("segment", x=1, yend=0.67,xend=1,y=1,  size=2, arrow = arrow(length = unit(0.03, "npc")))+
  annotate("text", x=1.02, y=0.90, label="Total  global threats", angle=90,size=8) +
  annotate("text", x=0.97, y=0.8,label="Less", angle=90,size=8) +
  annotate("text", x=0.97, y=0.95,label="More", angle=90,size=8) +
  coord_cartesian(xlim = c(0,1.0), ylim = c(-0.1, 1))



#create layout

layout <- c(
  area(t = 0, l = 0, b = 18, r = 20),
  area(t = 0, l = 0, b = 19, r = 22)
)

final<-pchart + overlay + 
  plot_layout(design = layout)



#Finish!
library(ragg)
agg_png("p.png", width = 1700, height = 1400, units = 'px')
final
dev.off()



