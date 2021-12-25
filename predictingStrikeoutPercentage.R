ppred = read.csv("https://baseballsavant.mlb.com/leaderboard/custom?year=2021,2020,2019&type=pitcher&filter=&sort=1&sortDir=desc&min=300&selections=p_game,pa,p_k_percent,p_bb_percent,p_foul,on_base_percent,p_called_strike,woba,z_swing_percent,z_swing_miss_percent,oz_swing_percent,oz_swing_miss_percent,oz_contact_percent,iz_contact_percent,whiff_percent,pitch_hand,n,n_fastball_formatted,fastball_avg_speed,fastball_avg_spin,fastball_avg_break,fastball_avg_break_x,fastball_avg_break_z,fastball_range_speed,n_breaking_formatted,breaking_avg_speed,breaking_avg_spin,breaking_avg_break,breaking_avg_break_x,breaking_avg_break_z,breaking_range_speed,n_offspeed_formatted,offspeed_avg_speed,offspeed_avg_spin,offspeed_avg_break_x,offspeed_avg_break_z,offspeed_avg_break,offspeed_range_speed,&chart=false&x=p_k_percent&y=p_k_percent&r=no&chartType=beeswarm&csv=true")
ppred$called_strike_percent = 100*ppred$p_called_strike/ppred$n
ppred$p_foul = 100*ppred$p_foul/ppred$n

nppred = ppred[3:ncol(ppred)] %>%
  group_by(player_id) %>%
  summarise(across(everything(), list(mean)))
colnames(nppred) = colnames(ppred[3:ncol(ppred)])
ppred = merge(filter(ppred,year==2021)[1:3],nppred, how = 'inner', on = 'player_id')

up = filter(ppred,p_k_percent > whiff_percent)
up = up[,!(names(up) %in% c("pitch_hand","X"))]
up = up[complete.cases(up),]
down = filter(ppred,p_k_percent < whiff_percent)
down = down[,!(names(down) %in% c("pitch_hand","X"))]
down = down[complete.cases(down),]
comp = round(colMeans(up[5:ncol(up)])-colMeans(down[5:ncol(down)]),2)
model = lm(p_k_percent~oz_swing_miss_percent+z_swing_miss_percent+oz_swing_percent+z_swing_percent+n_offspeed_formatted+n_breaking_formatted,data=ppred)
summary(model)
plot(model)
cc = ppred[,!(names(ppred) %in% c("pitch_hand","X"))]
cc = cc[complete.cases(cc),]
ggplotly(ggplot(data=cc,aes(predict(model,cc),p_k_percent))+
           geom_point(aes(text=paste(first_name,last_name)))+
           geom_abline()+#this is a y=x line to compare the fit
           theme_bw()+
           theme(element_blank()), tooltip = "text")