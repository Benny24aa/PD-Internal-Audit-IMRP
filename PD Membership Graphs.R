files <- list.files(path = "C:/Users/benny/Documents/My Resps/PD-Internal-Audit-IMRP/PD Rosters", pattern = "*.xlsx", full.names = T)
PD_Roster_Full <- sapply(files, read_excel, simplify=FALSE)

PD_Roster_Full <-  rbindlist(PD_Roster_Full, idcol = 'id', fill=TRUE)

PD_Roster_Full  <- PD_Roster_Full  %>%
  rename("Source" = id) %>%
  mutate(Source = gsub("C:/Users/benny/Documents/My Resps/PD-Internal-Audit-IMRP/PD Rosters/PD Roster ","", Source)) %>%
  mutate(Source = gsub(".xlsx","", Source)) %>%
  mutate(Source = gsub("_","/", Source)) %>% 
  mutate(Faction = "SAPD")

PD_Roster_Full$Source <- as.Date(PD_Roster_Full$Source)
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

  