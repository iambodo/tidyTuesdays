library(tidyverse)
library(tidytuesdayR)
library(tidygraph)
library(ggraph)
library(gganimate)

#friends data
tt<-tt_load("2020-09-08")

##check out emotions data
tt$friends %>% 
  left_join(tt$friends_emotions, 
            by=c("season", "episode", "scene", "utterance")) %>% 
  na.omit(emotion) %>% 
  group_by(speaker) %>% 
  mutate(n=n()) %>% 
  filter(n>1000) %>%   
  count(emotion) %>%
  mutate(percent=n/sum(n)*100) %>% View
  
#what about hugs and kisses?
k<-tt$friends %>% 
  filter(str_detect(text, c(" kiss| hug | hugs")) & speaker=="Scene Directions")


k %>% View
#and mentions of friends?
tt$friends %>% 
  filter(str_detect(text, "friend"))


tt$friends %>% 
  filter(str_detect(text, "kiss") & season==2) %>% View

tt$friends %>% 
  count(speaker) %>% 
  arrange(desc(n))


data <-tt$friends %>% 
  add_count(speaker, name="lines") %>%
  mutate(PDA=str_detect(text, 
                        c(" kiss| hug | hugs |hugg|making out|make out")) & 
           speaker=="Scene Directions") %>% 
  mutate(PDA=if_else(PDA, TRUE, NA)) %>% 
  group_by(season, episode, scene) %>% 
  tidyr::fill(PDA, .direction=c("updown")) %>% 
  ungroup() %>% 
  mutate(PDA=replace_na(PDA, FALSE)) %>% 
  filter(!str_detect(speaker,"ALL|Scene")) %>% 
  mutate(speaker = fct_lump(as.factor(speaker), n = 6),
         season= str_pad(season, 2, pad=0),
         id=str_c(season, "_", str_pad(episode, 2, pad=0), 
                  "_", str_pad(scene, 2, pad=0))) %>% 
  select(season, episode, scene, PDA, id, speaker)

#how many scenes have PDA
summary(data$PDA)



#now change the levels so its just the first names
levels(data$speaker) <- word(levels(data$speaker), 1)

#check out the season on season flow by speaker
data %>% 
  group_by(season, episode, speaker) %>% 
  summarize(n=n()) %>% 
  filter(speaker=="Monica") %>%
  ggplot()+
  geom_line(aes(x=episode, y=n)) +
  facet_wrap(~season)


#### First Graph - shared scenes by season ###


sharedscenes <-data %>% 
  select(id, speaker) %>% 
  group_by(id) %>% 
  left_join(data, by=c("id"), suffix=c("_from","_to")) %>% 
  ungroup() %>% 
  filter(speaker_from!=speaker_to) %>% 
  select(speaker_from, speaker_to, season, episode, scene, PDA) %>%
  distinct() %>% 
  group_by(season) %>% 
  mutate(maxep=max(episode)) %>% 
  ungroup() %>% 
  group_by(speaker_from, speaker_to, season, PDA) %>% 
  summarize(sc=n(), 
            mean_sc=sc/maxep) %>% 
  ungroup() %>% 
  group_by(season, PDA) %>% 
  arrange(desc(season, PDA)) %>% 
  mutate(topmean=max(mean_sc),   
         top = case_when(mean_sc==topmean ~ 1,
                         mean_sc <=topmean & mean_sc > topmean *0.8 ~ 2,
                         mean_sc <=topmean *0.8 & mean_sc > topmean *0.6 ~ 3,
                         mean_sc <=topmean *0.6 & mean_sc > topmean *0.4 ~ 4,
                         mean_sc <=topmean *0.4& mean_sc > topmean *0.2 ~ 5,
                         mean_sc <=topmean *0.2& mean_sc > 0 ~ 6)) %>% 
  mutate(top=as.character(top)) %>% 
  arrange(season) %>% 
  mutate(label=paste0("Season ", season)) %>% 
  ungroup()

# 
# data %>% 
#   filter(season==2 & PDA==TRUE)

scenesgraph <- as_tbl_graph(sharedscenes, directed = F) %>% 
  activate(edges) %>% 
  distinct()

#scenesgraph %>% activate(edges) %>% as_tibble() %>% View
  
##not run
# sharedscenes %>% 
#   mutate(new_col=1) %>% 
#   pivot_wider(names_from=PDA, 
#               names_prefix="pda_", 
#               values_from=new_col) %>% 
#   replace_na(list(pda_FALSE=0,pda_TRUE=0)) %>% 
#   pivot_longer(cols=starts_with("pda_"), names_to="scenesgraph",values_to="scenesgraph2")
# 

  
  # mutate(label=paste0("Season ", 
  #                     season, 
  #                     ", Episode ", 
  #                     str_pad(episode, 2, pad=0)))


scenesgraph %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  filter(season=="05")



#prepare graph labels

#colors()
cols<-RColorBrewer::brewer.pal(5, "Blues")
my_subtitle <- "Wider lines equal higher mean of shared scenes per episode in each season.\nBrighter lines mean a link has higher % of the season's shared scenes \n compared to other season relationships. \nThe duo with most shared scenes is highlighted yellow"


p<-scenesgraph %>% 
   ggraph(layout = 'circle') +
   geom_edge_link(aes(width=mean_sc, alpha=mean_sc, color=top)) +
#  geom_edge_fan(aes(edge_colour=PDA), width=0.8, alpha=0.4, strength=4) +
   scale_edge_color_manual(values=c("yellow",cols)) +
   scale_edge_alpha(range=c(0.3, 0.9)) +
   scale_edge_width(range=c(0.3, 4)) +
   geom_node_point(aes(color=name), size=30,
                  show.legend = F) +
   geom_node_text(aes(label=name), size=10, color="black") +
   scale_color_manual(values=c("#FF4238", "#FFDC00", "#42A2Db",
                             "#9A0006", "#FFF580", "#7d00ff", "#00c790")) +
   ggtitle('Friends Shared Scenes, Seasons 1-10', 
        subtitle = '{closest_state}') +
        # caption='Counts shared scenes with hug, kiss, or make out in stage directions') +
   theme_graph() + 
   theme(text=element_text(color="white"),
        plot.background = element_rect(fill = 'black'), 
        plot.title = element_text(color = '#F77976'),
        plot.subtitle = element_text(color = 'white'),
        legend.position = "bottom",
        strip.text=element_text(color='white')
        ) +
   transition_states(label, state_length = 2, 
                     transition_length = 1, wrap=FALSE) +
  enter_fade()+
  exit_fade()


animate(p, 100, duration=20, width=1000, height=700)

anim_save("friendsnet.gif")



#### Second Graph - shared scenes with PDA by season ###


sharedscenes <-data %>% 
  select(id, speaker) %>% 
  group_by(id) %>% 
  left_join(data, by=c("id"), suffix=c("_from","_to")) %>% 
  ungroup() %>% 
  filter(speaker_from!=speaker_to) %>% 
  select(speaker_from, speaker_to, season, episode, scene) %>%
  distinct() %>% 
  group_by(season) %>% 
  mutate(maxep=max(episode)) %>% 
  ungroup() %>% 
  group_by(speaker_from, speaker_to, season) %>% 
  summarize(sc=n(), 
            mean_sc=sc/maxep) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  arrange(desc(season)) %>% 
  mutate(topmean=max(mean_sc),   
         top = case_when(mean_sc==topmean ~ 1,
                         mean_sc <=topmean & mean_sc > topmean *0.8 ~ 2,
                         mean_sc <=topmean *0.8 & mean_sc > topmean *0.6 ~ 3,
                         mean_sc <=topmean *0.6 & mean_sc > topmean *0.4 ~ 4,
                         mean_sc <=topmean *0.4& mean_sc > topmean *0.2 ~ 5,
                         mean_sc <=topmean *0.2& mean_sc > 0 ~ 6)) %>% 
  mutate(top=as.character(top)) %>% 
  arrange(season) %>% 
  mutate(label=paste0("Season ", season)) %>% 
  ungroup()

#sharedscenes %>% filter(season=="01") %>% View()

scenesgraph <- as_tbl_graph(sharedscenes, directed = F) %>% 
  activate(edges) %>% 
  distinct()




p2<-scenesgraph %>% 
  ggraph(layout = 'circle') +
  geom_edge_link(aes(width=mean_sc, alpha=top, color=top)) +
  #  geom_edge_fan(aes(edge_colour=PDA), width=0.8, alpha=0.4, strength=4) +
  scale_edge_color_manual(name="Mean scenes per episode, as % of season's top link",
                          labels=c("100%","80%","60%","40%","0-20%"),
                          values=c("yellow",cols)) +
  scale_edge_alpha_manual(values=rev(c(0.2,0.4,0.5,0.7,0.8,0.99))) +
  scale_edge_width("Mean Shared Scenes Per Episode", 
                   range=c(0.3, 4)) +
  geom_node_point(aes(color=name), size=20,
                  show.legend = F) +
  geom_node_text(aes(label=name), size=4, color="black") +
  scale_color_manual(values=c("#FF4238", "#FFDC00", "#42A2Db",
                              "#9A0006", "#FFF580", "#7d00ff", "#00c790")) +
  ggtitle('Shared Scenes with PDA on Friends {closest_state}', 
          subtitle = my_subtitle) +
  labs(caption="Public Displays of Affection (PDA) defined as a hug, kiss, or make out within the shared scene.")+
  theme_graph() + 
  theme(text=element_text(color="white"),
        plot.background = element_rect(fill = 'black'), 
        plot.title = element_text(color = '#F77976', size=25, hjust=0.5, vjust=2),
        plot.subtitle = element_text(color = 'white', size=12, hjust=0.5),
        plot.caption= element_text(color="white", size=12),
        legend.position = "bottom",
        legend.box="vertical",
        strip.text=element_text(color='white', size=20)
  ) +
  guides(edge_width=guide_legend(override.aes = list(edge_color = "white")), 
         edge_alpha="none")+
  facet_edges(~PDA, strip.position = "bottom", labeller = label_both) +
  transition_states(label, state_length = 2, 
                    transition_length = 1, wrap=FALSE) +
  enter_fade()+
  exit_fade()

animate(p2, 100, duration=20, width=1200, height=800)
anim_save("friendsnet.gif")
