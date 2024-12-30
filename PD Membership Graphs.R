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

SAPD_Total_Members_Activity_Type_All <- PD_Roster_Full_Cleaned %>% 
  select(source, faction, Activity_Type) %>% 
  group_by(source,faction, Activity_Type) %>% 
  summarise(count=n(), .groups = 'drop') %>% 
  mutate(tier = "All") %>% 
  select(source, faction, Activity_Type, tier, count)

SAPD_Total_Members_Activity_Type_Tier <- PD_Roster_Full_Cleaned %>% 
  select(source, faction, Activity_Type, tier) %>% 
  group_by(source,faction, Activity_Type, tier) %>% 
  summarise(count=n(), .groups = 'drop') 

SAPD_Tier_Activity_Complete <- bind_rows(SAPD_Total_Members_Activity_Type_All, SAPD_Total_Members_Activity_Type_Tier)