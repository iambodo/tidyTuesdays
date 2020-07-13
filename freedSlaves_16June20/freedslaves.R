library(jsonlite)
library(tidyverse)
library(httr)
library(ggplot2)
library(gganimate)
library(mice)
library(magick)


#Load the data
blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')

african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')
#african_names  %>% View()

# slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
# 
# slave_routes %>% View()

# sr<-read_csv("https://slavevoyages.org/documents/download/2016TSTD.csv")

# sr %>% select(contains("voyage"))
# blackpast %>% View()

df<-african_names %>% 
  select("voyageid"=voyage_id) %>% 
  inner_join(sr, by=c("voyageid")) %>% 
  distinct(voyageid, .keep_all = TRUE)

#not_all_na <- function(x) {!all(is.na(x))}
#df %>% select_if(not_all_na) %>% View()

colSums(is.na(african_names)/length(african_names))
length(filter(african_names, is.na(age)))

#View

african_names %>% 
  View()

# african_names %>%
#   mutate(noage=if_else(is.na(age),1,0)) %>%
#   count(noage)
# #1126/90364


#check for missing values
african_names %>% 
  filter(is.na(age)) %>% 
  count(year_arrival) 




#imput data - only 1%
# imputed_Data <- mice(african_names, m=5, maxit = 5, method = 'pmm', seed = 500)
# 
# completedData <- complete(imputed_Data,1)
# 
# test<-completedData 
# 
# completedData %>% 
#   filter(gender=="Boy")


test<-african_names%>% 
  filter(year_arrival<1859)

t2<-test %>% 
  group_by(year_arrival) %>% 
  summarise(mode = mode(age))

#length(unique(test$year_arrival))

# t2 %>% View()
# 
# t2 %>% 
#   mutate(over=if_else(mode>13, 1, 0)) %>% 
#   count(over)
# 
# mode <- function(codes){
#   which.max(tabulate(codes))
# }
# 
# install.packages("modeest")
# library(modeest)
# 
# apply(test, Mode)

years<-test %>% 
select(year_arrival)

freq_dist<-ggplot(test, aes(age, stat(density), color=year_arrival)) +
  geom_freqpoly(binwidth = 3, size=1.5) +
  scale_colour_gradient(low = "#132B43", high = "#56B1F7")+
  theme_minimal()+
  theme(legend.position = "none") +
  transition_time(year_arrival) +
  shadow_mark(colour = "grey40", size=0.5) +
  labs(title="Released Captives From \n Captured Slave Ships, 1808-1848",
       subtitle =  'Frequency Distribution of Age \n Ship Captured in Year {round(frame_time)}',
       caption =  '{round(frame_time)}')+
  theme(legend.position="none",
        rect = element_rect(fill="black"),
        panel.background =element_rect(fill="black"),
        plot.background =element_rect(fill="black"),
        text = element_text(color="white"),
        panel.grid=element_line(color="grey20"),
        title = element_text(color="white", size = 18),
        axis.text = element_text(color="white", size=13),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=40, vjust = 20, hjust=0.8, color="grey40")
  )


#freq_dist

animate(freq_dist, nframes=200, duration = 20, end_pause = 50,  height = 800, width = 400)


anim_save("freq_dist.gif")





p2<-test %>% 
  count("year"=year_arrival) %>%
  arrange(year) %>%
  mutate(cumulative=cumsum(n)) %>%
  mutate(order = 1:n()) %>% 
  ggplot(aes(x=year, y=cumulative)) +
  geom_line(position="identity", color="pink1") +
  geom_point(color="pink1")+
  scale_y_continuous()+
  theme_minimal()+
  labs(title="Cumulative Captives Released",
       subtitle = 'Year 1808 to {round(frame_along)}') +
  ylab("Total Persons") +
  xlim(NA, 1852)+
  theme(legend.position="none",
        rect = element_rect(fill="black"),
        panel.background =element_rect(fill="black"),
        plot.background =element_rect(fill="black"),
        text = element_text(color="white"),
        panel.grid=element_line(color="grey20"),
        title = element_text(color="white", size = 18),
        axis.text = element_text(color="white", size=13),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=40, vjust = 20, hjust=0.8, color="grey40") )+
  transition_reveal(year) +
  ease_aes('linear')

animate(p2, nframes=200, duration = 20, end_pause = 50,  height = 400, width = 400)

anim_save("p2.gif")


#Recode the gender and age
gender_age<-test %>% 
  mutate(genderage=case_when(
    gender == "Woman" & is.na(age) ~ "Female, 18+",
    gender == "Man" & is.na(age) ~ "Male, 18+",
    gender == "Girl" & is.na(age) ~ "Female, 0-18",
    gender == "Boy" & is.na(age) ~ "Male, 0-18",
    gender == "Man" & age < 18 ~ "Male, 0-18",
    gender == "Boy" & age >= 18 ~ "Male, 18+",
    gender == "Man" & age >= 18 ~ "Male, 18+",
    gender == "Boy" & age < 18 ~ "Male, 0-18",
    gender == "Girl" & age < 18 ~ "Female, 0-18",
    gender == "Woman" & age < 18 ~ "Female, 0-18",
    gender == "Girl" & age >= 18 ~ "Female, 18+",
    gender == "Woman" & age >= 18 ~ "Female, 18+",
    TRUE ~ "Missing Gender")) 

# View(gender_age)

#Check total
gender_age %>%
  group_by(genderage) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent = n/sum(n))

gender_age %>%
  mutate(adult=if_else(age > 18, "18", "0-18")) %>% 
  group_by(adult) %>% 
  na.omit() %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent = n/sum(n))

library(scales)
p3 <-gender_age %>% 
  group_by(genderage) %>% 
  count("year"=year_arrival) %>%
  arrange(year) %>%
  mutate(cumulative=cumsum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x=genderage)) +
  geom_bar(aes(y=cumulative, fill=genderage), stat="identity") +
  scale_y_continuous()+
  scale_fill_viridis_d()+
  theme_minimal()+
  scale_x_discrete(labels = label_wrap(10))+
  theme(legend.position="none",
        rect = element_rect(fill="black"),
        panel.background =element_rect(fill="black"),
        plot.background =element_rect(fill="black"),
        text = element_text(color="white"),
        panel.grid=element_line(color="grey20"),
        title = element_text(color="white", size = 18),
        axis.text = element_text(color="white", size=13),
        plot.subtitle = element_text(size=12))+
       labs(subtitle="Cumulative Persons Released, by Age and Gender",
       y="Total Persons",
       x="",
       caption="All data from slavevoyages.org")+
  transition_reveal(year) +
  ease_aes('linear')

p3

animate(p3, nframes=200, duration = 20, end_pause = 50,  height = 400, width = 400)

anim_save("p3.gif")


#animations prepare
animate(p2, nframes=200, duration = 20, end_pause = 50,  height = 400, width = 400)

anim_save("p2.gif")

animate(freq_dist, nframes=200, duration = 20, end_pause = 50,  height = 800, width = 400)

anim_save("freq_dist.gif")


###merging GIFS
library(magick)
mb_gif<-image_read("freq_dist.gif")
mc_gif<-image_read("p2.gif")
md_gif<-image_read("p3.gif")



i<-1
left <- image_append(c(mb_gif[i]))
right <- image_append(c(mc_gif[i], md_gif[i]), stack=TRUE)
merge<-image_append(image_append(c(left, right)))
new_gif<-image_append(c(merge))



#check gif
new_gif 

for(i in 2:200){
  print(i)
  left <- image_append(c(mb_gif[i]))
  right <- image_append(c(mc_gif[i], md_gif[i]), stack=TRUE)
  merge<-image_append(image_append(c(left, right)))
  new_gif<-c(new_gif, merge)
}

new_gif

image_write(new_gif, format="gif", path="animation2.gif")

