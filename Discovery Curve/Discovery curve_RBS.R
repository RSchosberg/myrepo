
library("plyr")
library("tidyverse")
library("Rcapture")

#Load iMatch Exports

sightings_raw <-  list.files(path = "C:/Users/RW Aerial/Documents/myrepo/Discovery Curve/Data_DiscoveryCurve", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  rbind.fill()

#glimpse(sightings_raw)

# filter for main season, standardize column names
sightings <- sightings_raw %>% 
  filter(nchar(Field_EGNO) >0 & Field_EGNO != "UNASSIGNED" & Field_EGNO != " " & Field_EGNO != "?") %>% 
  mutate(DATE = as.Date(paste(Year, Month, Day, sep = "-"), "%Y-%m-%d")) %>%
  filter((Month >= 1 & Month <=5)) %>%
  #filter((Month >= 1 & Month <=5) | Month == 12) %>%
  #filter(Platform == "A") %>% 
  filter(Area != "SNE" & Area != "ACK")

#glimpse(sightings)
#unique(sightings$Month)

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

n_dc <-ggplot()+
  ggtitle("Cape Cod Bay and Adjacent Waters Aerial Sightings \n2022-2024 Discovery Curves")+
  geom_point(aes(x=CCSdisco_year$survey, y = CCSdisco_year$disc, colour = CCSdisco_year$year), alpha = 0.5, size = 3)+
  geom_line(aes(x=CCSdisco_year$survey, y = CCSdisco_year$disc, colour = CCSdisco_year$year), alpha = 0.5, size = .7)+
  theme_bw()+
  scale_color_brewer(palette = 'Set1', name = '')+
  xlab("")+
  ylab("Individual Right Whales (n)")+
  theme(panel.grid.minor = element_blank(),
        legend.position="bottom", plot.title = element_text(hjust = 0.5))+
  #You're going to want to do your own x-axis breaks
  scale_x_continuous(breaks = c(-31,1,15,31,45,60,74,91,105,121,135,152), labels = c("Dec 1", "Jan 1","Jan 15","Feb 1","Feb 14","Mar 1","Mar15","Apr 1","Apr 15","May 1","May 15","June 1")) +
  expand_limits(x = -.5)
  #expand_limits(x = -31)
n_dc

# Export:
ggsave(filename = "discovery_curve_2021_2024.png", 
       plot = n_dc, 
       width = 10, 
       height = 6, 
       dpi = 300)