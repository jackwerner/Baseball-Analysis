#find batter ID from bm dataframe
id = 641355	
beginning = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2021%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&batters_lookup%5B%5D="
end = paste(id,"&hfFlag=&hfBBT=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details&",sep="")
path = paste(beginning,end,sep="")
pitchhistory = read.csv(path)

library(ggplot2)
library(dplyr)
library(scales)

arsenal = data.frame(matrix(ncol=3))
colnames(arsenal) = c("Pitch","Percentage_Strike", "Percentage_of_HIP")

for (pitch in unique(pitchhistory$pitch_name)){
  current = data.frame(pitchhistory %>% filter_at(vars("pitch_name"), any_vars(. %in% pitch)))
  kcurrent = data.frame(current %>% filter_at(vars("description"), any_vars(. %in% c('called_strike','swinging_strike','swinging_strike_blocked'))))
  hitcurrent = data.frame(current %>% filter_at(vars("description"), any_vars(. %in% c('hit_into_play'))))
  allhit = data.frame(pitchhistory %>% filter_at(vars("description"), any_vars(. %in% c('hit_into_play'))))
  percent = label_percent()(nrow(kcurrent)/nrow(current))
  p2 = label_percent()(nrow(hitcurrent)/nrow(allhit))
  packet = c(pitch,percent,p2)
  arsenal = rbind(arsenal,packet)
}
arsenal = arsenal[-1,]
arsenal = arsenal[order(arsenal$Percentage_Strike),]
print(arsenal)

ggplot(pitchhistory, aes(fill=description, y = c(1:nrow(pitchhistory)), x=pitch_name)) + 
  geom_bar(position="stack", stat="identity")+ggtitle(pitchhistory$batter)


#render field
library(png)
mypng = download.file('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRfXgEMr84PMU2lUjk2smi748EEOzRX31eI0A&usqp=CAU', destfile = 'field.png', mode = 'wb')
field = readPNG('field.png')

#home plate at (126,204)
ggplot(pitchhistory, aes(x=hc_x, y = -hc_y, color=pitch_name))+
  annotation_raster(field, ymin = -247, ymax= 0, xmin = -8,xmax = 260)+
  theme_bw()+
  theme(element_blank())+
  xlim(0,250)+
  ylim(-204,0)+
  geom_point()+geom_point(x=126,y=-204,color="black",size=4)+
  stat_ellipse(type = "norm", level = .9)

#pitch of interest
POI = "4-Seam Fastball"
view = data.frame(pitchhistory %>% filter_at(vars("pitch_name"), any_vars(. %in% POI)))
ggplot(view, aes(x=hc_x, y = -hc_y,color=release_speed))+
  annotation_raster(field, ymin = -247, ymax= 0, xmin = -8,xmax = 260)+
  theme_bw()+
  theme(element_blank())+
  xlim(0,250)+
  ylim(-204,0)+
  stat_density2d_filled(alpha=.8,show.legend = FALSE)+
  geom_point()+geom_point(x=126,y=-204,color="black",size=4)+
  scale_color_gradient(low = "black", high = "red")

#pitcher of interest
pitcherID = "657277"
view2 = data.frame(pitchhistory %>% filter_at(vars("pitcher"), any_vars(. %in% pitcherID)))
ggplot(view2, aes(x=hc_x, y = -hc_y, color=pitch_name))+
  annotation_raster(field, ymin = -247, ymax= 0, xmin = -8,xmax = 260)+
  theme_bw()+
  theme(element_blank())+
  xlim(0,250)+
  ylim(-204,0)+
  geom_point()+geom_point(x=126,y=-204,color="black",size=4)+
  stat_ellipse(type = "norm", level = .9)
