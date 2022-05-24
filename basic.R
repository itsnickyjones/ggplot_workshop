rm(list = ls(all = TRUE)) #clear the workspace
invisible(gc()) #clear out any garbage in memory
options(scipen = 999) #don't use scientific notation

library(tidyverse)

# import data
survey_data <- readRDS("data/survey_data.Rds")

# basic plots in Base R
hist(survey_data$offline_bb)
plot(survey_data$offline_bb, survey_data$life_satisfaction,
     xlab="Social capital", ylab="Life satisfaction", pch=19) 

# add fit lines
abline(lm(survey_data$offline_bb ~ survey_data$life_satisfaction), col="red") # regression line (y~x) 
lines(lowess(survey_data$offline_bb,survey_data$life_satisfaction), col="blue") # lowess line (x,y) 


######################################################################
# Some excellent resources of making plots http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# Flat plots (one variable): http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
# Polishing your plots! https://ggplot2-book.org/polishing.html
# Find hex codes for your favorite colors: https://htmlcolorcodes.com/


######################################################################
# SCATTER PLOT
######################################################################
gg <- ggplot(survey_data, aes(x=offline_bb, y=life_satisfaction)) + 
  geom_point() + 
  #geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="lm", se=F) + 
  labs(y="Life satisfaction", 
       x="Offline social capital") 
gg

# Scatter plot (prettier; color by gender)
gg <- ggplot(survey_data, aes(x=offline_bb, y=life_satisfaction)) + 
  geom_point(aes(col=gender)) + 
  geom_smooth(method="lm", se=F) + 
  labs(y="Life satisfaction", 
       x="Offline social capital") +
  theme_light()
gg

# Scatter plot (prettier; color by gender; line by gender)
gg <- ggplot(survey_data, aes(x=offline_bb, y=life_satisfaction)) + 
  geom_point(aes(color=gender)) + 
  geom_smooth(aes(color=gender), method="lm", se=F) + 
  labs(y="Life satisfaction", 
       x="Offline social capital") +
  theme_light()
gg

######################################################################
# COLUMN CHART 
######################################################################
# summarize the data you want to display
col_chart <- survey_data %>%
  group_by(gender) %>%
  summarize(counts = n())
col_chart

# generate the column chart
ggplot(col_chart, aes(x = gender, y = counts)) +
  geom_col() +
  geom_text(aes(label = counts), vjust = -0.3)

#some options to add
#--------------------
# ylim(0, 41)
# labs(y="Count", 
#      x="Gender")
# geom_col(fill="#0773a2")
# theme_light()
######################################################################
# CORRELOGRAM
######################################################################
#devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)

corr <- round(cor(survey_data[3:13]), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"),
           ggtheme=theme_bw)

######################################################################
# AREA CHART
######################################################################
rm(list = ls(all = TRUE)) #clear the workspace
invisible(gc()) #clear out any garbage in memory
library(tidyverse)

# import data
state_data <- read_csv("data/state_data.csv")

# graph incident rate across states
ggplot(state_data, aes(x=Date, y=Incident_Rate, fill = RegionName2)) +
  geom_area(aes(colour = RegionName2,
                fill= fct_reorder(RegionName2, Incident_Rate, .desc = T)), 
            position = 'identity', alpha=0.6, size=.25, colour="black") + 
  theme(legend.position = "none") +
  ylab("Incidence per 100,000 people\n") +
  theme(text = element_text(size = 11))

# graph death rate across states
ggplot(state_data, aes(x=Date, y=Mortality_Rate, fill = RegionName2)) +
  geom_area(aes(colour = RegionName2,
                fill= fct_reorder(RegionName2, Incident_Rate, .desc = T)), 
            position = 'identity', alpha=0.6, size=.25, colour="black") + 
  #theme(legend.position = "none") +
  ylab("Mortality per 100,000 people\n") +
  theme(text = element_text(size = 11))

# FILTER the states of interest and plot
state_filtered <- state_data %>%
  filter(RegionName2 == "CA" | RegionName2 == "TX")

# graph death rate across states
ggplot(state_filtered, aes(x=Date, y=Mortality_Rate, fill = RegionName2)) +
  geom_area(aes(colour = RegionName2,
                fill= fct_reorder(RegionName2, Incident_Rate, .desc = T)), 
            position = 'identity', alpha=0.6, size=.25, colour="black") + 
  #labs(y="Mortality per 100,000 people\n", x = "Month", fill="State") +
  #facet_wrap(~RegionName2) + 
  #facet_wrap(~RegionName2, ncol= 1) +
  theme(text = element_text(size = 11)) 

#Note that theme(text = element_text(size = 11)) is a way to control the 
#size of all the text in the plot
