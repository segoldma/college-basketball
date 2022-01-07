library(ncaahoopR)
library(ggplot2)

# Team of interest
team <- "Maryland"

# Query pbp data
pbp <- ncaahoopR::get_pbp(
  team = team,
  season = "2021-22"
) 

# Reshape data to refer to the team, 
# regardless of home/away 
pbp_decorated <- pbp %>% 
  mutate(
    "team_score_diff" = case_when(
      home == team ~ score_diff,
      away == team ~ -1 * score_diff),
    
    "team_favored_by" = case_when(
      home == team ~ home_favored_by,
      away == team ~ -1 * home_favored_by),
    
    "opponent" = case_when(
      home == team ~ away,
      away == team ~ home),
    
    "mins_remaining" = round(
      secs_remaining / 60,
      2
    )
  )

# Plot Score Differential by Time Remaining by Game
pbp_decorated %>% 
  ggplot(
    aes(
      x = -mins_remaining, 
      y= team_score_diff, 
      color = factor(half)
      )
    )+
  geom_line()+
  geom_hline(
    aes(
      yintercept = team_favored_by
      ),
    linetype = "dashed"
    )+
  geom_hline(
    yintercept = 0,
    linetype = "dotted")+
  facet_wrap(~opponent)+
  theme(legend.position = "none")+
  ylab("Score Differential")+
  xlab("")+
  labs(title = paste0(team," Score Differential by Time Remaining"),
       subtitle = "Dashed Line = Team Favored By",
       caption = paste0("Run on ", Sys.Date()))
  

ggsave("plots/score-differential.png",
       device = "png",
       scale = 4)


  