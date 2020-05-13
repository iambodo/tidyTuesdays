library(readr)
library(tidyverse)
library(ggplot2)
library(gganimate)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

# View(tree_rings)
# View(eruptions)
# unique(events$event_type)
# eruptions %>% group_by(vei) %>% 
# summarize(n())

# 
# events %>% 
#   filter(eruption_start_year >= 1000) %>% 
#   group_by(volcano_name) %>% 
#   count() %>% 
#   arrange(desc(n))


top_volc <- eruptions %>% 
  filter(start_year >= 1000) %>% 
  group_by(volcano_number) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  left_join(volcano, by = "volcano_number") %>% 
  select("eruptions_total"=n, "volcano_number", "country","region","subregion",contains("pop"))



top_eruptions <- eruptions %>% 
  right_join(top_volc, by = "volcano_number") %>% 
  filter(start_year >= 1000) %>% 
  fill(vei) %>% 
  mutate(volcano_name=str_replace_all(volcano_name, "(.{6})", "\\1 "))
  


#make some corrections to the df to factor the volcano names
levels<-unique(top_eruptions$volcano_name)

top_eruptions<- top_eruptions %>% 
  mutate(volcano_name=factor(volcano_name, levels = levels)) %>% 
  mutate(start_year=as.integer(start_year)) 
#....and also change the min and max years for each (for the animation)

range2<-data.frame("volcano_name"=levels, id=1:10)
years<-data.frame(start_year=1000:2020,id=rep_len(1:10, length.out = 1021))
range<-left_join(range2, years) %>% select(-id)
top_eruptions<-bind_rows(top_eruptions, range) %>% 
  mutate(volcano_name=str_wrap(volcano_name, 6)) 


#static version to test
p0<-ggplot(top_eruptions, aes(x=start_year, y=0, group = start_year)) +
  geom_point(aes(size=vei), alpha=0.2) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~volcano_name, ncol=1, strip.position = "right") +
  theme(strip.text.y = element_text(angle=20)) +
  ylab("") +
  scale_y_discrete(breaks=NULL)+
  geom_vline(aes(xintercept=start_year))

#create some shapes for the volcanoes!
vshape<-data.frame(a=c(-10,0,10),b=c(950,1020,950))

#test
p0 +
  geom_polygon(data=vshape, aes(x=a, y=b), fill="brown", alpha=0.5)+
  theme_void()


#this one is horizontal...
p1<-ggplot(top_eruptions, aes(x=start_year, y=0)) +
  geom_point(aes(size=vei), alpha=0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept=start_year), color = "grey") +
  geom_point(aes(size=vei), alpha=0.4, color = "red") +
  theme_bw() +
  theme(strip.text.y = element_text(angle=180),
        legend.position = "left") +
  ylab("") +
  scale_y_discrete(breaks=NULL)+
  transition_time(start_year) +
  facet_wrap(~volcano_name, ncol=1, strip.position = "left") +
  shadow_mark(exclude_layer = c(3, 4))


# p2<-ggplot(top_eruptions, aes(y=start_year, x=0)) +
#   geom_point(aes(size=vei), alpha=0.2) +
#   geom_vline(aes(xintercept = 0)) +
#   geom_hline(aes(yintercept=start_year), color = "grey") +
#   geom_point(aes(size=vei), alpha=0.4, color = "red") +
#   theme_bw() +
#   theme(strip.text = element_text(angle=0),
#         legend.position = "left") +
#   xlab("") +
#   scale_x_discrete(breaks=NULL)+
#   transition_time(start_year) +
#   facet_wrap(~volcano_name, nrow=1, strip.position = "bottom") +
#   shadow_mark(exclude_layer = c(3, 4))
# 
# p2

p3<-ggplot(top_eruptions, aes(y=start_year, x=0)) +
  geom_point(aes(size=vei), alpha=0.2, color = "grey") +
  geom_hline(aes(yintercept=start_year)) +
  geom_point(aes(size=vei), alpha=0.4, color = "red")  +
  theme_bw()+
  scale_color_viridis_c() +
  theme(strip.text = element_text(angle=0),
        legend.position = "right",
        strip.background = element_blank(),
        panel.grid.major = element_blank())+  
  xlab("") +
  ylab("") +
  scale_x_discrete(breaks=NULL)+
  transition_time(start_year) +
  facet_wrap(~volcano_name, nrow=1, strip.position = "bottom") +
  shadow_mark(exclude_layer = c(2, 3))+
  geom_polygon(data=vshape, aes(x=a, y=b), fill="brown", alpha=0.5)+
  labs(title="Volcano Explosions by Year, 1000-{round(frame_time)}",
       subtitle="Top 10 Volcanoes with Most Explosions since 1000 A.D",
       caption="VEI classifies the size of eruption. Source: Smithsonian")
#p3

animate(
  plot = p3, 
  end_pause = 20
)

gganimate::anim_save("volcanoes.gif")

