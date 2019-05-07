#lubianat may 2019
# csv files were manually downloaded for pubmed searches by each disease title.


library(data.table)
library(tidyverse)
library(gganimate)
temp = list.files(pattern="*inflammatory.csv")
temp_names <- gsub('.csv', '\\1', temp)
myfiles = read.csv(temp)
full_disease_table <- myfiles
full_disease_table$year <- as.numeric(gsub('.*([12].*)', '\\1', as.character(full_disease_table$year) ))
full_disease_table$counts <- as.numeric(full_disease_table$sum_count)
# CODE BASED ON Nitish @ stackoverflow (https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other)
full_disease_table$disease <- full_disease_table$Target
full_disease_table$counts_lbl <- as.numeric(full_disease_table$counts)

full_disease_table_transformed <- full_disease_table %>%
  filter(year > 1970) %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-counts) * 1,
         counts_rel = counts/counts[rank==1],
         counts_lbl = paste0(" ",counts)) %>%
  ungroup()



p <- ggplot(full_disease_table_transformed, aes(rank, group = disease, 
                                                fill = as.factor(disease), color = as.factor(disease))) +
  geom_tile(aes(y = counts/2,
                height = counts,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(disease, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=counts,label = counts_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='{closest_state}', x = "", y = "Watson articles about inflammatory diseases",
       caption = "Sources: Watson") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')
anim <- animate(p, 200, fps = 10, duration = 20, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))
anim_save(filename = 'basic_inflammatory_race.gif' )
