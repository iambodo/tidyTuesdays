library(tidyverse)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
cocktails %>% distinct(measure) %>% View()
  filter(str_detect(measure, " oz")) %>% 
  mutate(lower=tolower(ingredient)) %>% 


valid_measures<-c(" oz", " cl", " dl", "shot")
valid_measures<-paste(valid_measures, collapse = "|")

library(dplyr)
topdrinks<-cocktails %>% 
  mutate(ingredient=tolower(ingredient)) %>% 
  filter(str_detect(measure, valid_measures)) %>%
  filter(!str_detect(ingredient, "juice")) %>% 
  group_by(ingredient) %>% 
  tally(sort=TRUE) %>% 
  head(16) #####  %>%   View()

topdrinks

####Set up API Loops
#create search list (API wont allow spaced names in query)
searchterms<-topdrinks %>% 
  mutate(term=if_else(!is.na(word(ingredient, 2)), word(ingredient, 2), ingredient)) %>% 
  select(term)

#create filter list 
filterterms<-topdrinks %>% 
  select(term=ingredient)


####get vinmonopolet data
library(httr)
library(jsonlite)

key<-read_lines("key.txt")
header<-paste0("Ocp-Apim-Subscription-Key = ",key)
output<-list()

searchterms
filterterms

#start loop
for (i in 1:length(searchterms$term)){
  searched<-searchterms$term[i]
  filtered<-filterterms$term[i]
  
  api_url<-paste0("https://apis.vinmonopolet.no/products/v0/",
                  "details-normal?productShortNameContains=", searched,
                  "&maxResults=5000")

  payload<-fromJSON(content(GET(api_url,add_headers("Ocp-Apim-Subscription-Key" = key)), type="text"), flatten = TRUE)

  #unlist prices and bind to other data
  prices<-bind_rows(payload$prices)
  df<-bind_cols(payload,prices)


  
  #filter down to full name containing the filter term
  df1<-df %>% 
    filter(str_detect(basic.productLongName, paste0(" (?i)",filtered)) | 
            str_detect(classification.productTypeName, paste0(" (?i)",searched)) |
             str_detect(basic.productLongName, paste0("(?i)",searched))) %>% 
    mutate(type=filtered)

  output[[i]]<-df1
  
}


#reshape the vino dataset
final<-bind_rows(output)

#make sure everything is classified right with Norwegian details
#baileys
#rum

#baileys
baileys<-final %>% filter(type == "bailey's irish cream" & 
                                 !str_detect(basic.productLongName, "Baileys"))

liquors <- final %>%
  anti_join(baileys, by = c("basic.productId")) %>% 
  left_join(topdrinks, by = c("type"="ingredient")) %>% 
  select("type","cocktails"=n, everything()) %>% 
  mutate(type = if_else((str_detect(type, "rum")), "rum", type)) %>% 
  mutate(type = if_else((type == "sweet vermouth" | type == "dry vermouth"), "vermouth", type)) %>% 
  filter(classification.mainProductTypeName=="Brennevin"| str_detect(type, "vermouth")) %>% 
  group_by(type) %>% 
  filter(type != "blue curacao")

total_tails<-liquors %>% 
  group_by(type, cocktails) %>% 
  summarize() %>% 
  ungroup() %>% 
  group_by(type) %>% 
  summarize(cocktails=sum(cocktails))

  
liquor_stats <- liquors %>%
  summarize(product_count=n(),
            median_cost_prLiter=median(salesPricePrLiter),
            median_alcohol=median(basic.alcoholContent)) %>% 
  left_join(total_tails, by = "type") %>% 
  mutate(max_cocktails=max(cocktails))
  
liquor_stats

# liquor_stats  %>% 
#   arrange(cocktails) %>%    
#   mutate(type=factor(type, levels=type)) %>%  
#   ggplot(aes(x=type, y=cocktails)) +
#   geom_bar(stat="identity")+ 
#   coord_flip()

am_legend<-liquor_stats %>% filter(type=="amaretto")
ann_text1 <- data.frame(x = am_legend$median_cost_prLiter + 700,y = 500,lab = "<-- Median Cost",
                       type = "amaretto")
ann_text2 <- data.frame(x = 2000,y = -2600,lab = "^_Cocktails",
                        type = "amaretto")


coolchart<-liquors %>% 
  filter(salesPricePrLiter< 3000) %>% 
  ggplot() +
  geom_histogram(aes(x = salesPricePrLiter, y = stat(count*20)), color="black", fill="white") +
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  scale_fill_brewer(type="qual", palette = "Set3") +
  geom_rect(mapping=aes(xmin=0, xmax=3000, ymin=0, ymax = -3000), 
            color="black", alpha=0.001) +
  geom_rect(data = liquor_stats, 
            mapping=aes(fill = type,
                        xmin=100, xmax=2900, 
                        ymin=-2900, ymax = -2900+(2800*cocktails/max_cocktails))) +
  geom_segment(data=liquor_stats, aes(label=median_cost_prLiter,
                                      x=median_cost_prLiter,
                                      xend= median_cost_prLiter,
                                      y= 800, yend=-3000, lineend="butt"),
               color="blue", linetype="dashed", size=1) +
  geom_text(data=liquor_stats,
             aes(label=paste0(round(median_cost_prLiter/22)," NOK"),
                 x=median_cost_prLiter, 
                 y= 1200,
                 position = "identity"),
                 size=3) +
  geom_text(data=liquor_stats,
            aes(label=paste0(product_count," varietes"), 
                x=2500, 
                y= 200,
                position = "identity"),
            fontface = "italic",
            size=2) +
    theme_void() +
  geom_text(data=ann_text1, mapping=aes(x=x, y=y, label=lab), size = 2)+
  geom_text(data=ann_text2, mapping=aes(x=x, y=y, label=lab), size = 2)+
  geom_label(data=liquor_stats,
            aes(label=cocktails,
                 x=1500, 
                 y= -2900+(2800*cocktails/max_cocktails),
                 position = "identity"),
                 size=3, nudge_y=100) +
  facet_wrap(~type, ncol=4, strip.position = "bottom") +
  theme(legend.position = "none")+
  labs(title=" 'Hva Drikker Du?' The Cost of a Drink in Norway",
       subtitle=" Median Retail Price Per Shot with Number of Possible Cocktails",
       caption="Displays most frequent liquor or liqueur ingredients.\nRetail price data from Vinmonopolet API. Cocktails data from Kaggle/TidyTuesdays\n 10 NOK = 1.01 USD")

#coolchart  

#install.packages("svglite")  
ggsave(filename = "shots.svg",coolchart, device="svg",
       dpi=320, width = 20, height = 20, units = "cm")
