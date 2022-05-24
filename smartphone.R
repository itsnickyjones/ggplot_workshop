rm(list = ls(all = TRUE)) #clear the workspace
invisible(gc()) #clear out any garbage in memory
options(scipen = 999) #don't use scientific notation

library(tidyverse)
library(tidytext)

###PROFILE DATA
appdata <- read_csv(file="data/AppUsage.csv")

#find app types
appdata$app<- tolower(appdata$app)

social <- scan(file="dict/sm_apps.txt", what="character", skip=0)
appdata$social <- sapply(appdata$app, function(x) sum(str_count(x, pattern=social)))

prod <- scan(file="dict/prod_apps.txt", what="character", skip=0)
appdata$prod <- sapply(appdata$app, function(x) sum(str_count(x, pattern=prod)))

dating <- scan(file="dict/dating_apps.txt", what="character", skip=0)
appdata$dating <- sapply(appdata$app, function(x) sum(str_count(x, pattern=dating)))

message <- scan(file="dict/messaging.txt", what="character", skip=0)
appdata$messaging <- sapply(appdata$app, function(x) sum(str_count(x, pattern=message)))

# a more basic approach to tagging string matches if all you care about 
# is a word match (and not a word match *count*)
entertainment <- paste(unlist(scan(file="dict/enter_apps.txt", what="character", skip=0)),collapse="|")
appdata$entertainment <- as.numeric(grepl(pattern=entertainment, appdata$app))

#generate categorical variable of app_type used in the data
appdata <- appdata %>%
  mutate(app_type = case_when(social == 1 ~ "Social",
                               prod == 1 ~ "Productivity",
                               dating == 1 ~ "Dating",
                               entertainment == 1 ~ "Entertainment",
                               messaging == 1 ~ "Messaging",
                              TRUE ~ "Other"))
table(appdata$app_type)

#extract hour of day
appdata$hour <- as.factor(str_sub(appdata$open, -5, -4))

#aggregate
appdata <- appdata %>%
  group_by(hour, app_type) %>%
  mutate(sec2 = mean(sec)/60) %>%
  ungroup()

# trim the data for plotting
plot_data <- appdata %>%
  distinct(hour, app_type, .keep_all = T)

#BAR CHART
ggplot(plot_data, aes(x = hour, y = sec2, fill = app_type)) + 
  geom_col() +
	labs(x = "Hour of day", y = "average minutes / hour") +
	labs(fill='Application type') +
	ggtitle("Average smartphone app usage by hour") +
	labs(caption = "(n = 1; data collapsed across 3-months)") +
	theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0))

#LINE PLOT
ggplot(plot_data, aes(x = hour, y = sec2, color = app_type)) + 
 	geom_point() +
 	geom_smooth(aes(group=app_type), method="loess", size=1, se=F) +
 	labs(x = "Hour of day", y = "average minutes / hour") +
	labs(color='Application type') +
	ggtitle("Average smartphone app usage by hour") +
	labs(caption = "(n = 1; data collapsed across 3-months)") +
	theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0)) +
  theme_light()

########################################################################
# app usage data from a study
appdata <- read_csv("data/appuse_study_data.csv")
table(appdata$mac)

appdata <- appdata %>%
  group_by(hour_of_day, app_type) %>%
  mutate(min = mean(Raw.Duration.Seconds.)/60) %>%
  ungroup()

# trim the data for plotting
plot_data <- appdata %>%
  distinct(hour_of_day, app_type, .keep_all = T) %>%
  filter(app_type != "Gaming")

#BAR CHART
ggplot(plot_data, aes(x = hour_of_day, y = min, fill = app_type)) + 
  geom_col() +
  labs(x = "Hour of day", y = "average minutes / hour") +
  labs(fill='Application type') +
  ggtitle("Average smartphone app usage by hour") +
  labs(caption = "(n = 25; data collapsed across 3-months)") +
  theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0))

#LINE PLOT
ggplot(plot_data, aes(x = hour_of_day, y = min, color = app_type)) + 
  geom_point() +
  geom_smooth(aes(group=app_type), method="loess", size=1, se=F) +
  labs(x = "Hour of day", y = "average minutes / hour") +
  labs(color='Application type') +
  ggtitle("Average smartphone app usage by hour") +
  labs(caption = "(n = 25; data collapsed across 2-weeks)") +
  theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0)) +
  theme_light()

	