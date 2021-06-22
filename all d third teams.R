library(tidyverse)
all_def<-read_csv("all-def.csv") %>%
  pivot_longer(cols=first_fwd:second_g_2,names_to="points",values_to="player") %>% 
  mutate(player=gsub("\\(.*","",player)) %>% 
  mutate(player=gsub("--.*","",player)) %>% mutate(player=str_trim(player)) %>% 
  separate(player,into=c("last","first"),sep=", ",convert = TRUE) %>%
  unite("player",c(first,last),sep=" ",na.rm=TRUE) %>% 
  mutate(points_given=case_when(
    str_detect(points,"first")~2,
    TRUE~1)
    ) %>%
  mutate(pos=case_when(
    str_detect(points,"_fwd")~"forward",
    str_detect(points,"_g")~"guard",
    TRUE~"center")
    ) %>%
  select(-points)

vote_totals=all_def %>% 
  group_by(year,player,pos) %>% 
  summarize(pos_pts=sum(points_given)) %>% 
  ungroup() %>% 
  group_by(year,player) %>% 
  mutate(tot_pts=sum(pos_pts)) %>% 
  filter(min_rank(desc(pos_pts))==1) %>% 
  ungroup()

third_tm_center=vote_totals %>% filter(pos=="center") %>% group_by(year) %>% 
  distinct(tot_pts,player,pos) %>%
  arrange(desc(tot_pts)) %>% slice(3)

third_tm_forwards=vote_totals %>% filter(pos=="forward") %>% group_by(year) %>% 
  distinct(tot_pts,player,pos) %>%
  arrange(desc(tot_pts)) %>% slice(5:6)

third_tm_guards=vote_totals %>% filter(pos=="guard") %>% group_by(year) %>% 
  distinct(tot_pts,player,pos) %>%
  arrange(desc(tot_pts)) %>% slice(5:6)

third_teams=bind_rows(third_tm_center,third_tm_forwards,third_tm_guards)

write_csv(third_teams,"All-D Third Teams.csv")

write_csv(vote_totals,"All-D Vote Totals.csv")

View(third_teams %>% group_by(player) %>% count())
