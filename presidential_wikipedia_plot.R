library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(scales)


########################
# Load the data
########################

data = read.csv('/users/nickbecker/Downloads/candidate_data_long_normalized.csv')

head(data)
glimpse(data)
str(data)


########################
# Clean label names
########################

presidential_data = data %>%
  filter(candidate_name %in% c('Donald_Trump', 'Hillary_Clinton', 'Gary_Johnson')) %>%
  mutate(Candidate = gsub("_", " ", candidate_name))
  


########################
# Raw Views Plot
########################

plot_raw_data = function() {
  ggplot(presidential_data, aes(date, view_count,
                              group = Candidate,
                              color = Candidate)) + 
    geom_line(size = 1.5) +
    
  labs(title = "Views of Presidential Candidates' Wikipedia Pages\nAround the 2016 Election",
       x = "Date",
       y = "Page Views") +
  
  scale_y_continuous(labels = comma) +

  theme(panel.grid.minor = element_blank(),
        legend.title = element_text(size = 25, face = 'bold'),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, size = 25),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 45)
        )
}


png('/users/nickbecker/Documents/R workspace/presidential_wiki_views.png',
    height = 1200, width = 1500)
plot_raw_data()
dev.off()



########################
# Normalized View
########################

ggplot(presidential_data, aes(date, normalized_view_count,
                              group = candidate_name,
                              color = candidate_name)) + 
  geom_line() 

