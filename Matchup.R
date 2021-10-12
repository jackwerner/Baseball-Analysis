#Pitchers: https://baseballsavant.mlb.com/leaderboard/custom?year=2021&type=pitcher&filter=&sort=0&sortDir=asc&min=50&selections=p_total_pa,p_home_run,p_k_percent,p_bb_percent,&chart=false&x=p_total_pa&y=p_total_pa&r=no&chartType=beeswarm
#Batters: Name, Team, PA, HR, SO, BB for batters
  #https://www.baseball-reference.com/leagues/MLB/2021-standard-batting.shtml

pm = read.csv("https://baseballsavant.mlb.com/leaderboard/custom?year=2021&type=pitcher&filter=&sort=0&sortDir=asc&min=50&selections=p_total_pa,p_home_run,p_k_percent,p_bb_percent,&chart=false&x=p_total_pa&y=p_total_pa&r=no&chartType=beeswarm&csv=true")
bm = read.csv("https://baseballsavant.mlb.com/leaderboard/custom?year=2021&type=batter&filter=&sort=1&sortDir=desc&min=50&selections=b_total_pa,b_home_run,b_strikeout,b_walk,&chart=false&x=b_total_pa&y=b_total_pa&r=no&chartType=beeswarm&csv=true")
#bm = bm[2:nrow(bm),]
bm = data.frame(bm)
#bm$Name = sub("\\\\.*", "", bm$Name)
bm$b_k_percent = bm$b_strikeout / bm$b_total_pa
bm$b_bb_percent = bm$b_walk / bm$b_total_pa
bm$b_hr_percent = bm$b_home_run / bm$b_total_pa
bm[is.na(bm)] = 0
pm$p_hr_percent = pm$p_home_run / pm$p_total_pa

PITCHER = "Hendricks"
TEAM = "NYM"
#DOESN'T WORK -- NEED ROSTER INFO
teammatchup = function(){
  require(dplyr)
  roster = filter(bm,Tm == TEAM)
  roster = filter(roster,PA > 40)
  View(roster)
  L = mean(c((mean(pm$p_k_percent)/100),mean(bm$b_k_percent))) 
  Lw = mean(c(mean(pm$p_bb_percent)/100,mean(bm$b_bb_percent)))
  Lhr = mean(c(mean(pm$p_hr_percent),mean(bm$b_hr_percent)))
  report = data.frame(matrix(nrow = 0, ncol = 4))
  colnames(report) = c("Name","SO%","BB%","HR%")
  
  
  for (i in 1:nrow(roster)){
    BATTER = as.character(roster$Name[i])
    B = bm$b_k_percent[match(BATTER,bm$last_name)]
    P = pm$p_k_percent[match(PITCHER,pm$last_name)]/100
    
    Bw = bm$b_bb_percent[match(BATTER,bm$last_name)]
    Pw = pm$p_bb_percent[match(PITCHER,pm$last_name)]/100
    
    HRB = bm$b_hr_percent[match(BATTER,bm$last_name)]
    HRP = pm$p_hr_percent[match(PITCHER,pm$last_name)]
    
    matchup = ((B*P) / L) / ( ((B*P)/L) + (1-B)*(1-P)/(1-L) )
    wmatchup = ((Bw*Pw) / Lw) / ( ((Bw*Pw)/Lw) + (1-Bw)*(1-Pw)/(1-Lw) )
    hrmatchup = ((HRB*HRP) / Lhr) / ( ((HRB*HRP)/Lhr) + (1-HRB)*(1-HRP)/(1-Lhr) )
    print(matchup)
    report[nrow(report) + 1, ] <- c(BATTER,matchup,wmatchup,hrmatchup)
  }
  report = report[order(report$`SO%`,decreasing=FALSE),]
  View(report)

}
teammatchup()



#SINGLE PLAYER MATCHUP
matchupcalc = function(){
  L = mean(c(mean(pm$p_k_percent/100),mean(bm$b_k_percent))) 
  B = bm$b_k_percent[match(BATTER,bm$player_id)]
  P = pm$p_k_percent[match(PITCHER,pm$player_id)]/100
  
  Lw = mean(c(mean(pm$p_bb_percent/100),mean(bm$b_bb_percent))) 
  Bw = bm$b_bb_percent[match(BATTER,bm$player_id)]
  Pw = pm$p_bb_percent[match(PITCHER,pm$player_id)]/100
  
  matchup = ((B*P) / L) / ( ((B*P)/L) + (1-B)*(1-P)/(1-L) )
  wmatchup = ((Bw*Pw) / Lw) / ( ((Bw*Pw)/Lw) + (1-Bw)*(1-Pw)/(1-Lw) )
  
  print(paste("Batter SO%: ",B,", Pitcher SO%: ",P,", SO%: ",matchup))
  print(paste("Batter BB%: ",Bw,", Pitcher BB%: ",Pw,", BB%: ",wmatchup))
}

PITCHER = "622072"
BATTER = "605141"
matchupcalc()



