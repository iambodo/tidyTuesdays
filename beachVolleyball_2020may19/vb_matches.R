#load data

library(readr)
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)
#View(vb_matches)
library(tidyverse)


#explore!
vb_matches %>% group_by(w_p1_country) %>% select(w_p1_country) %>% summarise(n=n()) %>% arrange(desc(n))

# vb_matches %>% 
#   select(starts_with())
#   pivot_longer()

library(stringr)
library(stringi)

#want to get the sum of the scores
setscore_sum<-function(x){
sum(as.numeric(unlist(str_extract_all(x, "[0-9]+"))))
}


colnames(vb_matches)

test<-vb_matches %>% 
mutate(sum=sapply(score, setscore_sum))
head(test$sum) #ok

#faulty attempt
# test %>% 
#   select("gender", contains("player"), ends_with(c("country", "aces","serve_errors")), "sum") %>% 
#   pivot_longer(
#     cols=contains(c("w_player1","p1")),
#     names_to=c("player1","stat"),
#     values_to="name"
#   )
#   


chart_data<-test %>% 
  select("gender", contains("player"), ends_with(c("country", "aces","serve_errors")), "sum") %>% 
  select(-country) %>% 
  rownames_to_column() %>% 
  rename_at(vars(contains("tot_aces")), list(~ str_replace(.,"tot_a","a"))) %>% 
  rename_at(vars(contains("tot_serve")), list(~ str_replace(.,"tot_serve_errors","serve.errors"))) %>% 
  rename_at(vars(contains("player")), list(~ str_remove(.,"layer"))) %>% 
  rename_at(vars(contains("w_p1")), list(~ paste0("1_", str_remove(.,"w_p1_")))) %>% 
  rename_at(vars(contains("w_p2")), list(~ paste0("2_", str_remove(.,"w_p2_")))) %>% 
  rename_at(vars(contains("l_p1")), list(~ paste0("3_", str_remove(.,"l_p1_")))) %>% 
  rename_at(vars(contains("l_p2")), list(~ paste0("4_", str_remove(.,"l_p2_")))) %>% 
  rename_at(vars(contains("_p")), list(~ paste0(str_sub(., 1, 1), "_name"))) %>% 
  pivot_longer(
    cols = -c(rowname, gender, sum),
    names_to = c("p.level",".value"),
    names_sep = "_"
  ) %>%
  drop_na(c("aces","serve.errors"))  %>% 
  replace_na(list(sum=0, aces=0, serve.errors=0)) %>% 
  filter(!is.na(country)) %>% 
  group_by(gender, country) %>% 
  summarize(sum_points=sum(sum),
            sum_aces=sum(aces),
            sum_errors=sum(serve.errors)) %>% 
  mutate(aces_per_thousand=((sum_aces/sum_points)*1000),
         errors_per_thousand=((sum_errors/sum_points)*1000)) %>% 
  ungroup() %>% 
  mutate(gender=recode(gender, "M"="Men", "W"="Women"))

chart_data

#add an average country
chart_data_avg<-chart_data %>% 
  group_by(gender) %>% 
  select_if(is.numeric) %>%
  summarize_all(mean) %>% 
  mutate(country="Average") %>% 
  bind_rows(chart_data)

chart_data_avg  

#esquisse to set up the plot!
# install.packages("esquisse")
# library(esquisse)
# esquisse::esquisser()

library(ggplot2)


#update_geom_defaults("text", list(colour = "white"))

library(scales)
coolchart<-ggplot(chart_data) +
 aes(x = errors_per_thousand, y = aces_per_thousand) +
 geom_point(aes(fill=gender,  size = sum_points), colour="grey",pch=21) +
 scale_color_manual(values=c("#0bd3d3", "#f890e7"), aesthetics=c("colour","fill")) +
 scale_size_continuous(range = c(1,10), 
                       breaks = c(1000, 10000, 100000, 2000000),
                       labels = c("< 1,000", "> 1,000", "> 100,000", "> 2,000,000")) +
 labs(x = "Serve Errors", 
      y = "Aces", 
      title = "Everythin's Comin' Up Aces?", 
      subtitle = "Aces and Serve Errors per thousand points played, by player country",
      caption = "FIVB and AVP matches, 2000-2019",
      color = "Gender", size = "Total points played") +
  geom_text(data=subset(chart_data_avg, errors_per_thousand > 40 | aces_per_thousand > 23), 
            aes(label=country, color = gender), size =4, hjust = 1, nudge_y = -0.5, show.legend = FALSE) +
  geom_text(data=subset(chart_data_avg, errors_per_thousand < 20 & aces_per_thousand > 15), 
            aes(label=country, color = gender), size =4, hjust = 1, nudge_y = -0.5, show.legend = FALSE) +
  geom_text(data=subset(chart_data_avg, errors_per_thousand < 10 | aces_per_thousand == 0), 
            aes(label=country, color = gender), size =4, hjust = 0, nudge_y = 1, show.legend = FALSE) +
  geom_text(data=subset(chart_data_avg, country == "Brazil"), 
            aes(label=country, color = gender), size =4, hjust = 1, nudge_y = -1, show.legend=FALSE) +
  geom_text(data=subset(chart_data_avg, country == "United States"), 
            aes(label="USA", color = gender), size =4, hjust = 1, nudge_y = -2, nudge_x = -1, show.legend = FALSE, check_overlap = TRUE) +
  geom_text(data=subset(chart_data_avg, country == "Average" & gender=='Women'),
             aes(label="Women's Average"), color = "green", size =3, hjust = 0, nudge_y = 1, show.legend = FALSE) +  
  geom_text(data=subset(chart_data_avg, country == "Average" & gender=='Men'),
            aes(label="Men's Average"), color = "green", size =3, hjust = 0.7, nudge_y = 1, show.legend = FALSE) +  
  geom_point(data=subset(chart_data_avg, country == "Average"), pch = 24, fill = "green", size =1, show.legend = FALSE) +  
  geom_point(data=subset(chart_data_avg, country == "Average"), pch = 24, fill = "green", size =, show.legend = FALSE) +  
 theme_dark() +
 theme(legend.position = "bottom",
       title = element_text(family="Rock Salt"),
       text = element_text(family = "Sans"),
       plot.background = element_rect(fill="black"),
       panel.background = element_rect(fill="black"),
       axis.title = element_text(color="white"),
       plot.title = element_text(color="#0bd3d3", size = 16),
       plot.subtitle = element_text(color="#f890e7", size = 12),
       plot.caption = element_text(color="white"),
       axis.line = element_line(color = "white"),
       axis.ticks = element_line(color="white"),
       axis.text = element_text(color="white"),
       legend.background = element_rect(fill="black"),
       legend.text = element_text(color="white", size = 15),
       legend.title = element_text(color="white", size = 16),
       legend.key = element_rect(fill="black"),
       legend.box =  "vertical",
       legend.margin = margin())

#showchart!  
coolchart

#install.packages("showtext")

#Add cool font!
library(showtext)
font_add_google(name = "Rock Salt", family = "Rock Salt") 
showtext_auto()

ggsave("coolchart2.png", height = 4, width = 5)

