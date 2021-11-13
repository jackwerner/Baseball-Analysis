#https://www.baseball-reference.com/leagues/MLB/2021.shtml
#https://www.baseball-reference.com/leagues/MLB/2021-standard-batting.shtml
#   ^for bm dataframe

TEAM1 = "Atlanta Braves" #Batting info uses TEAM1
TEAM2 = "Houston Astros"
tTEAM1 = "ATL"

main = function(TEAM1,TEAM2){
  print("***LOW RANK == GOOD***")
  batting = read.csv("/Users/jackwerner/Documents/baseball/leaguebatting.csv")
  pitching = read.csv("/Users/jackwerner/Documents/baseball/leaguepitching.csv")
  fielding = read.csv("/Users/jackwerner/Documents/baseball/leaguefielding.csv")
  
  #Batting
  battingreport = data.frame(batting$Tm,batting$R.G,batting$PA/batting$G,batting$HR/batting$G,batting$SO/batting$PA,batting$BB/batting$PA,batting$LOB/batting$G,stringsAsFactors = FALSE)
  colnames(battingreport) = c("Team","R/G","PA/G","HR/G","K%","BB%","LOB/G")
  
  battingranks = function(currentreport,bTEAM1,bTEAM2) {
    
    NUM_METRICS = length(currentreport)
    NUM_PLAYERS = nrow(currentreport)
    branks = data.frame(matrix(nrow = NUM_PLAYERS, ncol = NUM_METRICS),stringsAsFactors = FALSE)
    branks[1] = currentreport$Team
    
    for (c in 2:(NUM_METRICS)){#each column
      for (i in 1:NUM_PLAYERS){#each row
        branks[,c][i] = as.integer(rank(currentreport[,c])[i])
      }
    }
    colnames(branks) = colnames(currentreport)
    branks$`R/G` = (NUM_PLAYERS+1)-branks$`R/G`
    branks$`PA/G` = (NUM_PLAYERS+1)-branks$`PA/G`
    branks$`HR/G` = (NUM_PLAYERS+1)-branks$`HR/G`
    branks$`BB%` = (NUM_PLAYERS+1)-branks$`BB%`
    library(dplyr)
    branks = filter(branks,Team == bTEAM1 | Team == bTEAM2)
    
    library(reshape2)
    rt = melt(branks, id.vars=1)
    colnames(rt) = c("Team","Metric","Rank")

    library(tidyr)
    library(ggplot2)
    rt[3] <- lapply(rt[3], trimws)
    numerics = c()
    for(i in (1:nrow(rt[3]))){
      numerics[i] = as.numeric(rt[i,3])
    }
    rt[3] = numerics
    battingplot <<- ggplot(rt, aes_string(x = names(rt)[2], y = names(rt)[3], fill = names(rt)[1])) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      ggtitle("Batting Ranks")
    
    print(branks)
    rm(branks)
    rm(rt)
    rm(currentreport)
    rm(numerics)
  }
  print("Batting:")
  battingranks(battingreport,TEAM1,TEAM2)
  
  
  
  #Defense
  defensereport = data.frame(pitching$Tm,pitching$RA.G,pitching$SO/pitching$BF,pitching$BB/pitching$BF,pitching$HR/pitching$G,pitching$FIP,pitching$SO.W,pitching$LOB/pitching$G,pitching$W.L.,fielding$DefEff,fielding$E/fielding$G,fielding$Fld.,stringsAsFactors = FALSE)
  colnames(defensereport) = c("Team","R/G","K%","BB%","HR/G","FIP","K/BB","LOB/G","Win%","DefEff","E/G","Fielding%")
  
  defenseranks = function(currentreport,dTEAM1,dTEAM2) {
    
    NUM_METRICS = length(currentreport)
    NUM_PLAYERS = nrow(currentreport)
    dranks = data.frame(matrix(nrow = NUM_PLAYERS, ncol = NUM_METRICS), stringsAsFactors = FALSE)
    dranks[1] = currentreport$Team
    
    for (c in 2:(NUM_METRICS)){#each column
      for (i in 1:NUM_PLAYERS){#each row
        dranks[,c][i] = as.integer(rank(currentreport[,c])[i])
      }
    }
    colnames(dranks) = colnames(currentreport)
    dranks$`K%` = (NUM_PLAYERS+1)-dranks$`K%`
    dranks$`K/BB` = (NUM_PLAYERS+1)-dranks$`K/BB`
    dranks$`LOB/G` = (NUM_PLAYERS+1)-dranks$`LOB/G`
    dranks$`Win%` = (NUM_PLAYERS+1)-dranks$`Win%`
    dranks$`DefEff` = (NUM_PLAYERS+1)-dranks$`DefEff`
    dranks$`Fielding%` = (NUM_PLAYERS+1)-dranks$`Fielding%`
    
    library(dplyr)
    dranks = filter(dranks,Team == dTEAM1 | Team == dTEAM2)
    library(reshape2)
    rt = melt(dranks, id.vars=1)
    colnames(rt) = c("Team","Metric","Rank")

    library(tidyr)
    library(ggplot2)
    rt[3] <- lapply(rt[3], trimws)
    numerics = c()
    for(i in (1:nrow(rt[3]))){
      numerics[i] = as.numeric(rt[i,3])
    }
    rt[3] = numerics
    defensiveplot <<- ggplot(rt, aes_string(x = names(rt)[2], y = names(rt)[3], fill = names(rt)[1])) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      ggtitle("Defensive Ranks")
    
    print(dranks)
    rm(dranks)
    rm(rt)
    rm(currentreport)
    rm(numerics)
  }
  
  print("Defense:")
  defenseranks(defensereport,TEAM1,TEAM2)
  
  
  battingroster = function(team){
    bm = read.csv("/Users/jackwerner/Documents/baseball/bmWithTeams.csv")
    bm = bm[2:nrow(bm),]
    bm = data.frame(bm)
    bm$Name = sub("\\\\.*", "", bm$Name)
    bm$b_k_percent = bm$SO / bm$PA
    bm$b_bb_percent = bm$BB / bm$PA
    bm$b_hr_percent = bm$HR / bm$PA
    bm$woba = (.691*bm$BB + .722*bm$HBP + .885*(bm$H - bm$X2B - bm$X3B - bm$HR) + 1.260*bm$X2B + 1.598*bm$X3B + 2.060*bm$HR) / (bm$PA - bm$SB)
    bm[is.na(bm)] = 0
    require(dplyr)
    roster = filter(bm,Tm == team)
    roster = filter(roster,PA > 50)
    strikeoutplot <<- ggplot(roster, aes(x = reorder(Name, b_k_percent), y = b_k_percent)) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      ggtitle("Strikeout Percent") + 
      theme(axis.text.x = element_text(angle = 90, size = 10))
    walkplot <<- ggplot(roster, aes(x = reorder(Name, b_bb_percent), y = b_bb_percent)) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      ggtitle("Walk Percent") + 
      theme(axis.text.x = element_text(angle = 90, size = 10))
    wobaplot <<- ggplot(roster, aes(x = reorder(Name, woba), y = woba)) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      ggtitle("wOBA") + 
      theme(axis.text.x = element_text(angle = 90, size = 10))
  }
  battingroster(tTEAM1)
}

main(TEAM1,TEAM2)
defensiveplot
library(gridExtra)
grid.arrange(battingplot,strikeoutplot,walkplot,wobaplot,nrow=2)
# battingplot
# strikeoutplot
# walkplot
# wobaplot

