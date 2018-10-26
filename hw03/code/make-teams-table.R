# Title: HW 03 
#Description: 
#   Clean and prepare data for HW03
#
# Input: nba2017-roster.csv nba2017-stats.csv
# Output: teams-summary.txt nba2017-teams.csv efficiency-summary.txt
# Author: Owen McGrattan
# Date: 10-05-2017


# packages
library(readr) #importing data
library(dplyr) #data wrangling 
library(ggplot2) #data vis

# roster and stats csvs
roster <- read_csv("~/stat133/stat133-hws-fall17/hw03/data/nba2017-roster.csv")
pl_stat <- read_csv("~/stat133/stat133-hws-fall17/hw03/data/nba2017-stats.csv")

# create additional variables 
pl_stat <- mutate(pl_stat, missed_ft = points1_atts - points1_made, 
       missed_fg = field_goals_atts - field_goals_made,
       points = points1_made + (points2_made *2) + (points3_made * 3), 
       rebounds = off_rebounds + def_rebounds, 
       efficiency = (points + rebounds + assists + steals + blocks
                    - missed_fg - missed_ft - turnovers) / games_played
)

# sink output to text file
sink("~/stat133/stat133-hws-fall17/hw03/output/efficiency-summary.txt")
summary(pl_stat$efficiency)
sink()

# merge both data tables
nba <- merge(roster, pl_stat, by = "player")

# aggregate data to make nba2017-teams.csv (totals)
teams <- select(nba, team, experience, salary, points3_made, points2_made, points1_made,
                points,off_rebounds, def_rebounds, assists, steals, blocks, turnovers, fouls,
                efficiency) %>%
  group_by(team) %>%
  summarize(
    experience = sum(experience),
    efficiency = sum(efficiency),
    salary = round(sum(salary) / 1000000 ,digits = 2),
    points3 = sum(points3_made),
    points2 = sum(points2_made),
    free_throws = sum(points1_made),
    points = sum(points),
    off_rebounds = sum(off_rebounds),
    def_rebounds = sum(def_rebounds),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    turnovers = sum(turnovers),
    fouls = sum(fouls)
  )

# sink teams output summary to text file
sink("~/stat133/stat133-hws-fall17/hw03/output/teams-summary.txt")
summary(teams)
sink()

# write teams table into csv file (store in data folder)
write_csv(teams, path = "~/stat133/stat133-hws-fall17/hw03/data/nba2017-teams.csv")
    
# create plots of teams
pdf("~/stat133/stat133-hws-fall17/hw03/images/teams_star_plot.pdf")
stars(teams[,-1],labels = teams$team)
dev.off()

a <- ggplot(data = teams, aes(experience, salary)) + geom_point(color = "blue2") + 
  geom_text(aes(label = teams$team),hjust = 0, vjust = 0)
ggsave(a,filename = "~/stat133/stat133-hws-fall17/hw03/images/experience_salary.pdf")
  

