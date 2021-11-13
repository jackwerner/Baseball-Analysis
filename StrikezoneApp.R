#install.packages('shiny','ggplot2','dplyr','gridExtra','factoextra')
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(factoextra)

beginning = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2021%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&batters_lookup%5B%5D="
end = paste(501659,"&hfFlag=&hfBBT=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details&",sep="")
path = paste(beginning,end,sep="")
alb = read.csv(path)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Strikezone Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput("fullname",h3("Batter Full Name"),"Abraham Almonte", placeholder="Firstname Lastname"),width=3
    ),
  
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    bm = read.csv("https://baseballsavant.mlb.com/leaderboard/custom?year=2021&type=batter&filter=&sort=1&sortDir=desc&min=50&selections=b_total_pa,b_home_run,b_strikeout,b_walk,&chart=false&x=b_total_pa&y=b_total_pa&r=no&chartType=beeswarm&csv=true")
    
    firstname = strsplit(input$fullname, " ")[[1]][1]
    lastname = strsplit(input$fullname, " ")[[1]][2]
    
    temp = bm %>% filter_at(vars("first_name"), any_vars(. %in% paste(" ",firstname,sep="")))
    temp = temp %>% filter_at(vars("last_name"), any_vars(. %in% lastname))
    
    id = unique(temp$player_id)
    
    beginning = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2021%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&batters_lookup%5B%5D="
    end = paste(id,"&hfFlag=&hfBBT=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details&",sep="")
    path = paste(beginning,end,sep="")
    alb = read.csv(path)
    
    
    
    alb=data.frame(c(alb[1],alb[3:5],alb[10],alb[15],alb[18:19],alb[22],alb[45:52],alb[56:58],alb[90]))
    alb = alb[order(alb$pitch_type),]
    alb$pitch_type=as.numeric(unclass(alb$pitch_type))
    alb = alb[order(alb$description),]
    alb$description=as.numeric(unclass(alb$description))
    alb = alb[order(alb$stand),]
    alb$stand=as.numeric(unclass(alb$stand))
    alb = alb[order(alb$p_throws),]
    alb$p_throws=as.numeric(unclass(alb$p_throws))
    alb = alb[order(alb$type),]
    alb$type=as.numeric(unclass(alb$type))
    
    #1,2,3
    #4,5,6
    #7,8,9
    alb = alb %>%
      mutate(zx = case_when(
        zone=="1" | zone=="4" | zone=="7" ~ 2,
        zone=="2" | zone=="5" | zone=="8" ~ 4,
        zone=="3" | zone=="6" | zone=="9" ~ 6,
        zone=="11"| zone=="13"~1,
        zone=="12"| zone=="14"~6,
      ), zy=case_when(
        zone=="1" | zone=="2" | zone=="3" ~ 6,
        zone=="4" | zone=="5" | zone=="6" ~ 4,
        zone=="7" | zone=="8" | zone=="9" ~ 2,
        zone=="11"| zone=="12"~6,
        zone=="13"| zone=="14"~1,
        
      ))
    
    #1 is ball
    #2 is strike
    #3 is ball in play
    alb$type = recode_factor(alb$type, "1"="ball","2"="strike","3"="ball in play")
    #3 called strike
    #4 foul strike
    #5 foul tip strike
    #7 swinging strike
    strikes = filter(alb,type=="strike")
    strikes$description = recode_factor(strikes$description, "3"="called strike","4"="foul strike","5"="foul tip strike","7"="Strike BIP","8"="swinging strike","9"="swinging strike blocked")
    strikes = filter(strikes,description %in% c("called strike","foul strike","foul tip strike","swinging strike"))
    
    heart1 = filter(strikes,as.numeric(zone) < 10)
    #corners = filter(strikes,as.numeric(zone) > 10)
    s = ggplot()+
      geom_bin2d(data = heart1, aes(x = zx, y=zy),binwidth = c(2,2))+
      xlim(-1, 7) + ylim(-1, 7)+ 
      scale_fill_viridis_c(option = "plasma")+
      ggtitle("Strikes")
    
    
    inplay = filter(alb,type=="ball in play")
    heart2 = filter(inplay,as.numeric(zone) < 10)
    #corners = filter(strikes,as.numeric(zone) > 10)
    i = ggplot()+
      geom_bin2d(data = heart2, aes(x = zx, y=zy),binwidth = c(2,2))+
      xlim(-1, 7) + ylim(-1, 7)+
      scale_fill_viridis_c(option = "plasma")+
      ggtitle("BIPs")
    
    grid.arrange(s, i, nrow = 1)
    
  },width=900,height=700)
  
}

shinyApp(ui = ui, server = server)
