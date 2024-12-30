files <- list.files(path = "C:/Users/benny/Documents/My Resps/PD-Internal-Audit-IMRP/PD Rosters", pattern = "*.xlsx", full.names = T)
PD_Roster_Full <- sapply(files, read_excel, simplify=FALSE)

PD_Roster_Full <-  rbindlist(PD_Roster_Full, idcol = 'id', fill=TRUE)

PD_Roster_Full  <- PD_Roster_Full  %>%
  rename("Source" = id) %>%
  mutate(Source = gsub("C:/Users/benny/Documents/My Resps/PD-Internal-Audit-IMRP/PD Rosters/PD Roster ","", Source)) %>%
  mutate(Source = gsub(".xlsx","", Source)) %>%
  mutate(Source = gsub("_","/", Source))