files <- list.files(path = "C:/Users/benny/Documents/My Resps/PD-Internal-Audit-IMRP/PD Rosters", pattern = "*.xlsx", full.names = T)
PD_Roster_Full <- sapply(files, read_excel, simplify=FALSE)

PD_Roster_Full <-  rbindlist(PD_Roster_Full, idcol = 'id', fill=TRUE)

PD_Roster_Full  <- PD_Roster_Full  %>%
  rename("Source" = id) %>%
  mutate(Source = gsub("C:/Users/benny/Documents/My Resps/PD-Internal-Audit-IMRP/PD Rosters/PD Roster ","", Source)) %>%
  mutate(Source = gsub(".xlsx","", Source)) %>%
  mutate(Source = gsub("_","/", Source)) %>% 
  mutate(Faction = "SAPD")

PD_Roster_Full$Source <- dmy(PD_Roster_Full$Source)

PD_Roster_Full_Cleaned <- PD_Roster_Full %>% 
  filter(!is.na(Rank)) %>% 
  filter(Name != "Name")

rm(PD_Roster_Full) ### Removes roster full

PD_Roster_Full_Cleaned <- PD_Roster_Full_Cleaned |> 
  mutate(Name = gsub("  🔒","", Name)) 

PD_Roster_Full_Cleaned <- PD_Roster_Full_Cleaned %>% 
  mutate(Rank = str_replace(Rank, "^\\S* ", ""))

PD_Roster_Full_Cleaned <- PD_Roster_Full_Cleaned %>% 
  clean_names() %>% 
  mutate(playtime_2_weeks = gsub("hours","",playtime_2_weeks))|> 
  mutate(playtime_2_weeks = gsub(" ","",playtime_2_weeks))

PD_Roster_Full_Cleaned$playtime_2_weeks <- as.numeric(as.character(PD_Roster_Full_Cleaned$playtime_2_weeks))

PD_Roster_Full_Cleaned <- PD_Roster_Full_Cleaned |>
  mutate(
    Activity_Zero = playtime_2_weeks == 0 ,
    Activity_Bare_Minimum = playtime_2_weeks > 0 & playtime_2_weeks <= 20,
    Activity_Well = playtime_2_weeks >20 & playtime_2_weeks <=40,
    Activity_Great = playtime_2_weeks > 40
  )

#Given Critera to search for when considering activity boundaries

PD_Roster_Full_Cleaned <- PD_Roster_Full_Cleaned |>
  mutate(Activity_Type = case_when(
    Activity_Zero == TRUE ~ "Inactive",
    Activity_Bare_Minimum == TRUE ~ "Needs Improvement",
    Activity_Well == TRUE ~ "Good",
    Activity_Great == TRUE ~ "Very Good"
  ))

### Removes columns related to conditions for activity

PD_Roster_Full_Cleaned <- PD_Roster_Full_Cleaned |>
  select(-Activity_Zero) |>
  select(-Activity_Bare_Minimum)|>
  select(-Activity_Well)|>
  select(-Activity_Great)


### Total number of SAPD members

SAPD_Total_Members <- PD_Roster_Full_Cleaned %>%
  select(source, faction) %>% 
  group_by(source, faction) %>%
  summarise(count=n(), .groups = 'drop')

### Total number of SAPD members by activity type group


### Total for activity type for Tier Dataset
SAPD_Total_Members_Activity_Type_All <- PD_Roster_Full_Cleaned %>% 
  select(source, faction, Activity_Type) %>% 
  group_by(source,faction, Activity_Type) %>% 
  summarise(count=n(), .groups = 'drop') %>% 
  mutate(tier = "All") %>% 
  select(source, faction, Activity_Type, tier, count)

### Total for activity type based on Tier for Tier Dataset
SAPD_Total_Members_Activity_Type_Tier <- PD_Roster_Full_Cleaned %>% 
  select(source, faction, Activity_Type, tier) %>% 
  group_by(source,faction, Activity_Type, tier) %>% 
  summarise(count=n(), .groups = 'drop') 

SAPD_Tier_Activity_Complete <- bind_rows(SAPD_Total_Members_Activity_Type_All, SAPD_Total_Members_Activity_Type_Tier)

rm(SAPD_Total_Members_Activity_Type_All, SAPD_Total_Members_Activity_Type_Tier)

### Count of number of members by rank on X day 

### This fixes rare case where Roster Displays l and i as similar, and allocates rank correctly to right group now.

PD_Roster_Full_Rank_Cleaned <- PD_Roster_Full_Cleaned %>% 
  mutate(rank = gsub("l", "I", rank))

Rank_Member_Count <- PD_Roster_Full_Rank_Cleaned %>% 
  select(source, rank) %>% 
  group_by(source, rank) %>% 
  summarise(count=n(), .groups = 'drop')

### Activity type data prep for graphs

Activity_Count <- PD_Roster_Full_Rank_Cleaned %>% 
  select(source, Activity_Type) %>% 
  group_by(source, Activity_Type) %>% 
  summarise(count=n(), .groups = 'drop')


#### Markdown Prep for Graphs

line_graph_theme <- theme(axis.title = element_text(colour="#3A3776", family = "sans"),
                       axis.text.x = element_text(size=14, colour = "black"),
                       axis.text.y = element_text(size=14, colour = "black"),
                       axis.title.x = element_text(size=18),
                       axis.title.y = element_text(size=18),
                       panel.background = element_blank(),
                       panel.grid.major.y = element_line(colour = "light grey"),
                       panel.grid.major.x = element_blank(),
                       axis.line.x = element_line(colour="black"),
                       axis.line.y = element_line(colour="black"),
                       plot.margin = margin(.5,1,.5,.5, "cm"))

bar_graph_theme <- theme(axis.title = element_text(colour="#3A3776", family = "sans"),
                      axis.text.x = element_text(size=14, colour = "black"),
                      axis.text.y = element_text(size=14, colour = "black"),
                      axis.title.x = element_text(size=18),
                      axis.title.y = element_text(size=18),
                      panel.background = element_blank(),
                      panel.grid.major.x = element_line(colour = "light grey"),
                      panel.grid.major.y = element_blank(),
                      axis.line.x = element_line(colour="black"),
                      axis.line.y = element_line(colour="black"),
                      plot.margin = margin(.5,1,.5,.5, "cm"))

###### Figure 1

full_member_graph <- SAPD_Total_Members %>% 
  ggplot(aes(x = source, y = count, group = faction, color=faction)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 11),  # y-axis breaks
                     limits = c(0, 300),  # Set y-axis limits
                     expand = c(0, 0)) +
  labs(x = "Date of Roster",
       y = "Number of Members") +
  line_graph_theme


###### Figure 2 Data Prep and Graph

member_count_tier <- PD_Roster_Full_Rank_Cleaned %>% 
  select(source, tier) %>% 
  group_by(source, tier) %>% 
  summarise(count=n(), .groups = 'drop')

member_count_tier_graph <- member_count_tier %>% 
  ggplot(aes(x = source, y = count, group = tier, color= tier)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 11),  # y-axis breaks
                     limits = c(0, 200),  # Set y-axis limits
                     expand = c(0, 0)) +
  labs(x = "Date of Roster",
       y = "Number of Members") +
  line_graph_theme



##### Figure 3 Data Prep and Graph

activity_graph <- Activity_Count %>% 
  ggplot(aes(x = source, y = count, group = Activity_Type, color=Activity_Type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 11),  # y-axis breaks
                     limits = c(0, 300),  # Set y-axis limits
                     expand = c(0, 0)) +
  labs(x = "Date of Roster",
       y = "Number of Members") +
  line_graph_theme


ggsave("test.png",
       plot = member_count_tier_graph,
       height = 7.5,
       width = 18,
       dpi = 300)