library(tidyverse)
library(tidytext)
library(words)
# install.packages("words")
words::words %>% 
  filter(word_length==5) %>% 
  #give ID to each word
  rowid_to_column() %>%
  #split into characters
  unnest_tokens(letter, word, token="characters") %>% 
  add_count(letter, name="total_count") %>% 
  #create index for the position in word
  group_by(rowid) %>% 
  mutate(ind=row_number()) %>% 
  group_by(ind) %>% 
  #count letter within index
  count(letter) %>% 
  mutate(percent=n/sum(n)) %>% 
  group_by(letter) %>% 
  mutate(total_count=sum(n), #total obs of letter in all 5-letter words
         vowel=if_else(letter %in% c("a","e","i","o","u","y"),1,0)) %>% 
  ungroup() %>% 
  mutate(rank=26-ntile(total_count,25)) %>% #rank by frequency (1 letter missing?)
  group_by(ind) %>% 
  slice_max(n, n=10) %>% #top 10 letters per index level
  mutate(type=case_when(
    vowel==1 ~ "Vowel",
    vowel==0 & rank < 12 ~ "Top 5 Consonant",
    TRUE ~ "Other Consonant"
  ), type=fct_rev(fct_infreq(type))) %>% 
  arrange(desc(percent)) %>%
  ggplot(aes(x=reorder_within(letter, percent, ind), y=percent, fill=type)) +
  geom_bar(stat="identity")+
  scale_fill_viridis_d(option="viridis") +
  scale_x_reordered() +
  scale_y_continuous(labels=scales::percent) +
  coord_flip() +
  facet_wrap(~ind, nrow=1, scales="free_y") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text=element_text(family="ubuntu"),
        legend.title = element_blank(),
        strip.background = element_rect(fill="grey90",color="grey80"),
        strip.text = element_text(color="grey20")) +
  labs(title="Frequency of Letters By Position in 5-Letter English Words",
       subtitle="From 9,330 words in Scrabble Dictionary", 
       caption="Source: scrabbleplayers.org | Chart: @1ambodo",
    x="",y="Frequency") 


