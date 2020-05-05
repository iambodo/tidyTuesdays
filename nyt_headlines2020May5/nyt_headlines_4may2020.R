library(httr)
library(jsonlite)
library(tidyverse)
library(stopwords)
library(tidytext)
library(lubridate)
library(stringr)



#set up URL for download from API - just the A1 headlines
my_key<-readLines("mynytkey.txt", warn=FALSE)
#GET YOUR OWN KEY AT DEVELOPER.NYTIMES.COM


url<-paste0("https://api.nytimes.com/svc/search/v2/articlesearch.json?",
            "begin_date=20191201&end_date=20200501&",
            "fq=print_section:A AND print_page:1&",
            "api-key=",my_key)
payload<-jsonlite::fromJSON(url, flatten = TRUE)

#api only gets us 10 pages at a time
maxPages <- round((payload$response$meta$hits[1] / 10)-1) 

#set loop for pagination thru API
pages <- list()
for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(url, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(6) 
}

#combine outputs by row
nyt_fp <-rbind_pages(pages) 

nyt_fp2 <-nyt_fp %>% select(starts_with("response.docs")) %>% 
  rename_all(funs(stringr::str_replace(.,"response.docs.", ""))) %>% 
  distinct(uri, .keep_all = TRUE)

View(head(nyt_fp2))




library(lubridate)
#just the headlines and dates
headlines<-nyt_fp2 %>% select(headline.main, pub_date) %>% 
  mutate(date=lubridate::ymd(as.Date(pub_date))) %>% 
  mutate(month=lubridate::month(date,label=TRUE)) %>% 
  mutate(year=year(date)) %>% 
  filter(month!="May")



library(tidytext)
library(stopwords)
headline_words <- headlines %>%
  unnest_tokens(word, headline.main) %>%    #count words
  filter(str_detect(word, "\\w+"),
         !word %in% stop_words$word) %>% 
  mutate(word = str_remove(word, "\\’s"))
  

top_words<-headline_words %>% 
  group_by(word) %>% 
  summarise("total"=n()) %>% 
  arrange(desc(total)) %>%
  ungroup() %>% 
  filter(word!="it") %>% 
  head(24)


top_words_monthly <-headline_words %>% 
#we join so we can include N in the facet name
  right_join(top_words, by="word") %>% 
  mutate("word_n" = paste0(word, " (n=",total,")")) %>% 
#  filter(word %in% top_words$word) %>% 
#create filter to get order right
  mutate(month = factor(month, levels= c("Dec","Jan","Feb","Mar","Apr"))) %>% 
  group_by(month, word_n, .drop=FALSE) %>% 
  count(word) %>%
  ungroup() 


#an inelegant hack for the facet names with N
in_apr<-top_words_monthly %>% 
  filter(month=="Apr")

in_dec<-top_words_monthly %>% 
  filter(month=="Dec")

top_words_monthly<-top_words_monthly %>% 
  mutate("Old News?"=if_else(!word %in% in_apr$word, "0 mentions in April",
                    if_else(!word %in% in_dec$word, "0 mentions in December", 
                            "April and December mentions")))


#View(top_words_monthly)


# top_words_monthly %>% 
#   ggplot(aes(x=month, y=n, fill=`Old News?`))+ 
#   geom_col()+
#   facet_wrap(~word_n, nrow=6) +
#   labs(title="NYT Headlines last 5 months") +
#   theme(legend.position = "bottom")


top_words_monthly %>% 
  ggplot(aes(x=month, y=n,  fill=`Old News?`))+ 
  geom_col()+
  facet_wrap(~word_n, nrow=6) +
  theme_minimal() +
  labs(title="nytimes.com Headlines, December 2019 - April 2020",
       subtitle="Monthly word count of top 24 words in web edition headlines, page A1 stories",
       caption = "Source: developer.nytimes.com") +
  ylab("count") +
  theme(legend.direction = "horizontal",
        legend.title.align = 0,
        legend.position = "bottom",
        legend.key.size =  unit(.5, "cm"),
        legend.box = "vertical",
        legend.text = element_text(size = 6),
        legend.title = element_text(face="bold.italic", size=8))


ggsave("nyt_web.jpg")


