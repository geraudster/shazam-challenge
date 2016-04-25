dataForGraph <- melt(trainset, c('idSound', 'class', 'cluster'))

ggplot(dataForGraph[dataForGraph$variable %in% c('bands_1', 'bands_2', 'bands_3', 'bands_4', 'bands_5', 'bands_6'),]) + geom_boxplot(aes(class, value, fill = class)) +
  facet_wrap(~ variable) + coord_flip() + theme(legend.position='none')

## Plot leaderboard
jsonFile <- 'ShazamLeaderboard-5a8728fa6ef0.json'
secrets <- jsonlite::fromJSON(jsonFile)

service_token <- oauth_service_token(
  endpoint = oauth_endpoints("google"),
  secrets = secrets,
  scope = 
    "https://spreadsheets.google.com/feeds")

try(gs_auth(service_token))

doc <- gs_url('https://docs.google.com/spreadsheets/d/1D-T8NGO8gFWqbg2jGi5XPSRARCo03UgOLQYNZRiwz8k/edit?usp=sharing')
scores <- gs_read(doc, ws = 'Scores')

library(lubridate)
library(dplyr)
library(ggplot2)
qplot(parse_date_time(scores$Timestamp, '%m/%d/%y %H:%M:%S'), scores$Score, col = scores$User, geom = 'line') +
  xlab('Time') + ylab('Score') +
  theme(legend.position='none')

by_user <- group_by(scores, User)
scores_by_user <- summarise(by_user, max(Score))
scores_by_user[order(scores_by_user$`max(Score)`, decreasing = TRUE)]
