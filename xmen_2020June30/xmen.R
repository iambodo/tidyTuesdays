#load data
library(tidyverse)

remotes::install_github("malcolmbarrett/claremontrun")

library(claremontrun)

claremontrun::characters %>% colnames

library(dplyr)

#Network diagram? Hmmm...
claremontrun::characters %>%
  select(character, shared_undress, matches("which", "whom","shared")) %>% 
  filter_at(vars(-character),  any_vars(!is.na(.)))
  filter_at(vars(14:30), any_vars(!is.na(.))) %>% 
  select(14:20)
  
  
#Bechdel test is easier
claremontrun::xmen_bechdel %>%
  count(pass_bechdel) %>% 
  filter(!is.na(pass_bechdel)) %>% 
  mutate(freq = n / sum(n))




# What about Avengers?
claremontrun::comic_bechdel %>% 
  group_by(series) %>% 
 # filter(series=="Avengers") %>% 
  count(pass_bechdel) %>% 
  mutate(freq = n / sum(n))




#Lets compare using a comic cover as guide

#install.packages("countcolors")
library(countcolors)

kmeans.clusters <- colordistance::getKMeanColors("cover127.jpg", n = 3, plotting = FALSE)
colordistance::extractClusters(kmeans.clusters)

colordistance::plotPixels("cover127.jpg", lower = NULL, upper = NULL, n = 5000)

colordistance::plotClusters()

# Read the image into the R environment
cover127 <- jpeg::readJPEG("cover127.jpg")

center.spherical <- c(0.726, 0.316, 0.445)

#nice maroon color
#rgb(0.726, 0.316, 0.445)



# Find all the pixels within a 31% radius

#this is a hack to get the dominant color matching the bechdel fail -- tinker with "radius" value
cover127.spherical <- countcolors::sphericalRange(cover127, center = center.spherical, 
                                                  radius = 0.312, target.color=center.spherical,
                                                  plotting = TRUE)

#rect(0,0,1,1,col=rgb(0.726, 0.316, 0.445))

#this might not match the exact value for pixel color change in next step
cover127.spherical$img.fraction


converted<-countcolors::changePixelColor(cover127, cover127.spherical$pixel.idx, 
                                         target.color=center.spherical, 
                                         return.img = TRUE)
#write output
jpeg::writeJPEG(converted, "converted.jpg")


#load output to assess color ranges
img127_converted <-colordistance::loadImage("converted.jpg")

#color space is now fractured into dominant color!
colordistance::plotPixels(img127_converted, lower=NULL, upper=NULL)



#upper and lower limits for background
lower <- c(0.99, 0.99, 0.99)
upper <- c(1, 1, 1)

#check color histogram. The dominant bin should be color of replacement above
hist127 <- colordistance::getImageHist(img127_converted, bins=c(2, 1, 3), lower=lower, upper=upper)

hist127

#create dataset from color histogram output
data<-hist127 %>%
  mutate(bechdel_fail=if_else(Pct==max(Pct), "Fail", "Pass")) %>%
  mutate(rgb=str_c(r, g, b)) %>% 
  group_by(bechdel_fail)


#now create barplot with those colors
plot<-ggplot(data, aes(fill=rgb(r, g, b), y=Pct, x=bechdel_fail)) + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal()+
  scale_color_identity(aesthetics = "fill") +
 scale_y_continuous(labels=scales::percent_format()) +
  geom_label(data=filter(data, bechdel_fail=="Fail"), 
            aes(label=round(100*Pct, 2), y=Pct+0.003), hjust=0)+
  theme(plot.title  = element_text(hjust = 0.5, size=20),
        plot.subtitle  = element_text(hjust = 0.5, size=9),
        axis.text.y = element_text(size=16))+
  coord_flip() +
  labs(title="How many X-Men comics pass the Bechdel test?",
       subtitle="Bechdel Pass = 'features at least two women who talk to each other about something other than a man' \n",
       y = "\n % of issues (n=544) \n & \n % pixels of this color at right",
       caption = "Data source: @claremontrun \n \n Image source: 13thdimension.com",
       x="")

plot

ggsave("barplot.png")  

library(magick)

#load images
header<-magick::image_read("barplot.png")
original<-magick::image_read("cover127.jpg")
cover<-magick::image_read("converted.jpg")


#rescale images
cover<-image_scale(cover, "400")
original<-image_scale(original, "400")
header<-image_scale(header, "800")

cover
header
both_covers<-image_append(c(original, cover))
final <- image_append(c(header, both_covers), stack=TRUE)

image_write(final, "xmen_output.jpg")





#### Now do avengers

kmeans.clusters <- colordistance::getKMeanColors("cover_avengers.jpg", n = 3, plotting = FALSE)
colordistance::extractClusters(kmeans.clusters)

colordistance::plotPixels("cover_avengers.jpg", lower = NULL, upper = NULL, n = 5000)


# Read the image into the R environment
cover_avengers <- jpeg::readJPEG("cover_avengers.jpg")

center.spherical <- c( 0.7144035, 0.3417976, 0.2896556)

#rgb()
#nice maroon color
#rgb(0.726, 0.316, 0.445)



# Find all the pixels within a 0% radius

cover_avengers.spherical <- countcolors::sphericalRange(cover_avengers, center = center.spherical, 
                                                  radius = 0.33, target.color=center.spherical,
                                                  plotting = TRUE)

#rect(0,0,1,1,col=rgb(0.726, 0.316, 0.445))


cover_avengers.spherical$img.fraction


converted<-countcolors::changePixelColor(cover_avengers, cover_avengers.spherical$pixel.idx, 
                                         target.color=center.spherical, 
                                         return.img = TRUE)

jpeg::writeJPEG(converted, "converted.jpg")



img_ave_converted <-colordistance::loadImage("converted.jpg")

colordistance::plotPixels(img_ave_converted, lower=NULL, upper=NULL)



#upper and lower limits for background
lower <- c(0.99, 0.99, 0.99)
upper <- c(1, 1, 1)


histave <- colordistance::getImageHist(img_ave_converted, bins=c(2, 2, 2), lower=lower, upper=upper)
histave

data<-histave %>%
  mutate(bechdel_fail=if_else(Pct==max(Pct), "Fail", "Pass")) %>%
  mutate(rgb=str_c(r, g, b)) %>% 
  group_by(bechdel_fail)

data

plot<-ggplot(data, aes(fill=rgb(r, g, b), y=Pct, x=bechdel_fail)) + 
  geom_bar(position="stack", stat="identity")+
  theme_minimal()+
  scale_color_identity(aesthetics = "fill") +
  scale_y_continuous(labels=scales::percent_format(),
                     expand=expand_scale(add=0.07)) +
  geom_label(data=filter(data, bechdel_fail=="Fail"), 
             aes(label=round(100*Pct, 1), y=Pct+0.003), hjust=0)+
  theme(plot.title  = element_text(hjust = 0.5, size=20),
        plot.subtitle  = element_text(hjust = 0.5, size=9),
        axis.text.y = element_text(size=16))+
  coord_flip() +
  labs(title="How many Avengers comics pass the Bechdel test?",
       subtitle="Bechdel Pass = 'features at least two women who talk to each other about something other than a man' \n",
       y = "\n % of issues (n=45) \n & \n % pixels of this color at right",
       caption = "Data source: @claremontrun \n \n Image source: coverbrowser.com",
       x="")

plot

ggsave("barplot2.png")  

library(magick)

#load images
header<-magick::image_read("barplot2.png")
original<-magick::image_read("cover_avengers.jpg")
cover<-magick::image_read("converted.jpg")


#rescale images
cover<-image_scale(cover, "400")
original<-image_scale(original, "400")
header<-image_scale(header, "800")

cover
original
header
both_covers<-image_append(c(original, cover))
final <- image_append(c(header, both_covers), stack=TRUE)

image_write(final, "avengers.jpg")







