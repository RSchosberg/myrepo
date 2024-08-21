#Discovery curve of all animals

library("tidyverse")
library("plyr")
library("lubridate")
library("Rcapture")
library("kableExtra")

# load iMatch exports

sightings_raw <-  list.files(path = "//pccs-server/Aerial Survey/data/RW Plus - all years/Progress Reports/Annual Prog Report/July 1 2021 - June 30 2024/Figures", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  rbind.fill()


# filter for main season, standardize column names
sightings <- sightings_raw %>% 
  filter(nchar(Field_EGNO) >0 & Field_EGNO != "UNASSIGNED" & Field_EGNO != " " & Field_EGNO != "?") %>% 
  mutate(DATE = as.Date(paste(Year, Month, Day, sep = "-"), "%Y-%m-%d")) %>%
  filter((Month >= 1 & Month <=5) & Year > 2020) %>% 
  #filter(Platform == "A") %>% 
 #filter(Area != "SNE" & Area != "ACK")

sightings <- egnos %>%  
  filter(nchar(Field_EGNO) >0 & Field_EGNO != "UNASSIGNED" & Field_EGNO != " " & Field_EGNO != "?") %>%
  add_column(Year = 2022) %>% 
  mutate(DATE = as.Date(paste(Year, Month, Day, sep = "-"), "%Y-%m-%d"))

#function for discovery curve- can import all years
disco<-function(x){
  
  freqcap<-x%>%
    distinct(DATE,Field_EGNO)%>%
    group_by(DATE,Field_EGNO)%>%
    tally()%>%
    tidyr::spread(DATE,n)%>%
    dplyr::select(-Field_EGNO)%>%
    as.data.frame()
  freqcap[is.na(freqcap)] <- 0 
  desc<-Rcapture::descriptive(freqcap)
  desc.df<-as.data.frame(desc$base.freq, row.names = FALSE)
  desc.df<-desc.df%>%
    mutate(date = names(freqcap),
           survey = yday(names(freqcap)),
           disc = cumsum(ui),
           order = 1:nrow(desc.df),
           year = year(date))
  desc.df$survey<-as.factor(desc.df$survey)
  desc.df$disc<-as.numeric(desc.df$disc)
  
  desc.df
}

CCSdisco<-disco(sightings)


#######Discovery curve broken down by year

CCSdisco_year<-sightings%>%
  distinct(DATE,Field_EGNO)%>%
  group_by(DATE,Field_EGNO)%>%
  tally()%>%
  mutate(Year = year(DATE))

CCSdisco_year_ls<-split(CCSdisco_year, CCSdisco_year$Year)

freqcap_ls<-lapply(CCSdisco_year_ls, function(x){
  freqcap<-x%>%
    tidyr::spread(DATE,n)%>%
    dplyr::select(-Field_EGNO,-Year)%>%
    as.data.frame()
  
  freqcap[is.na(freqcap)] <- 0
  freqcap
})

desc<-lapply(freqcap_ls, function(x){
  
  desc<-Rcapture::descriptive(x)
  
  desc.df<-as.data.frame(desc$base.freq, row.names = FALSE)
  
  desc.df<-desc.df%>%
    mutate(date = names(x),
           disc = cumsum(ui),
           csid = cumsum(vi),
           order = 1:nrow(desc.df),
           year = year(date))
  
  desc.df$disc<-as.numeric(desc.df$disc)
  
  desc.df })

CCSdisco_year<-do.call("rbind", desc)
CCSdisco_year<-CCSdisco_year%>%
  mutate(survey = yday(date))
CCSdisco_year$year<-as.factor(CCSdisco_year$year)


###plotting discovery curve by year, need to customise/fix (and incorporate december)

n_dc <-ggplot()+
  ggtitle("Cape Cod Bay and Adjacent Waters Aerial Sightings \n2022 Discovery Curve")+
  geom_point(aes(x=CCSdisco_year$survey, y = CCSdisco_year$disc, colour = CCSdisco_year$year), alpha = 0.5, size = 3)+
  geom_line(aes(x=CCSdisco_year$survey, y = CCSdisco_year$disc, colour = CCSdisco_year$year), alpha = 0.5, size = 1)+
  theme_bw()+
  scale_color_brewer(palette = 'Set3', name = '')+
  xlab("")+
  ylab("Individual Right Whales")+
  theme(panel.grid.minor = element_blank(),
        legend.position="bottom", plot.title = element_text(hjust = 0.5))+
  #You're going to want to do your own x-axis breaks
  scale_x_continuous(breaks = c(-31,0,32,60,92,121,152), labels = c("Dec 1", "Jan 1","Feb 1","Mar 1","Apr 1","May 1","June 1")) +
  expand_limits(x = -30)

n_dc

#effort per year
CCSeff<-sightings%>%
  dplyr::distinct(DATE, Year)%>%
  dplyr::group_by(Year)%>%
  dplyr::summarise(`Survey Days` = n(), First=min(DATE),Last=max(DATE))%>%
  as.data.frame()

CCSeff$First<-format(CCSeff$First,"%d %B")
CCSeff$Last<-format(CCSeff$Last,"%d %B")

kable(CCSeff, "latex", booktabs = T, linesep = "") %>%
  kable_styling("striped", full_width = F)

