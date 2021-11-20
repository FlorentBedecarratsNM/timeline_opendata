# Très inspiré et partiellement copié de 
# https://www.themillerlab.io/post/timelines_in_r/

# On installe le package ethercalc qui permet de lire un framapad
# si pas déjà installé
if (!is.element("ethercalc", installed.packages()[,1])) {
  devtools::install_github("hrbrmstr/ethercalc")
}

# On charge les packages requis
library(ethercalc)
library(tidyverse)
library(scales)
library(lubridate)

# On récupère les données
tl <- ec_read("opendata_nm_cd44_rpdl-9qxk", 
                ec_host = "https://lite.framacalc.org")

tl <- tl %>%
  mutate(type = as_factor(type),
         date = as.Date(date, origin = "1899-12-30"),
         position = position * direction)

Event_type_levels <- c("Evolution fonctionnelle", "COPIL", "Institutionnel", "Sous-domaines", "Prix") 
## These hashtagged codes represent the colors (blue, green, yellow, red) as hexadecimal color codes.
Event_type_colors <- c("#C00000", "#FFC000",  "#00B050", "brown", "#0070C0")

month_buffer <- 6
text_offset <- 0.2 
month_date_range <- seq(min(tl$date) - months(month_buffer), max(tl$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b') 
month_df <- data.frame(month_date_range, month_format)
year_date_range <- seq(min(tl$date) - months(month_buffer), max(tl$date) + months(month_buffer), by='year')
# We will only show the years for which we have a december to january transition.
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")),  
  origin = "1970-01-01") 
# We want the format to be in the four digit format for years.
year_format <- format(year_date_range, '%Y') 
year_df <- data.frame(year_date_range, year_format)


# Let's use the absolute value since we want to add the text_offset and increase space away from the scatter points 
absolute_value<-(abs(tl$position)) 
text_position<- absolute_value + text_offset

# Let's keep the direction above or below for the labels to match the scatter points
tl$text_position <- text_position * tl$direction 


# Create timeline coordinates with an x and y axis
tl_plot<- ggplot(tl, aes(x=date,y= position, 
                                  col=type, label = tl$label)) +
  labs(col=element_blank()) + 
  scale_color_manual(values=Event_type_colors, 
                     labels=Event_type_levels, 
                     drop = FALSE) +
  theme_classic() + 
  geom_hline(yintercept=0,
             color = "black", 
             size=0.3) +
  geom_segment(data = tl, 
               aes(y=tl$position, yend = 0, xend = tl$date), 
               color='black', size=0.2) +
  geom_point(aes(y = tl$position), size = 3) +
  theme(axis.line.y=element_blank(),
         axis.text.y=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.ticks.y=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x =element_blank(),
         axis.line.x =element_blank(),
         legend.position = "bottom") +
  # geom_text(data = month_df, 
  #             aes(x = month_date_range, y = -0.15, label = month_format),
  #             size = 3.5, vjust = 0.5, color= 'black', angle = 90) +
  geom_text(data = year_df, 
            aes(x = year_date_range, y = -0.07, 
                label = paste("I\n", year_format), fontface = "bold"), 
            size = 3.5, color='black') +
  geom_label(aes(y = tl$text_position, 
                label = str_wrap(tl$label, 20)), 
            size = 3, vjust=0.6, label.size = 0, 
            label.padding = unit(0.1, "lines"),
            show.legend = FALSE)



# Print plot
tl_plot



