#install.packages('shiny','ggplot2','dplyr','gridExtra','factoextra')
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(factoextra)
library(dplyr)
beginning = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=FF%7CFT%7CFC%7CSI%7CFS%7CSL%7CCH%7CCU%7CKC%7CCS%7CKN%7CFO%7CEP%7CSC%7CIN%7CPO%7CAB%7CUN%7C&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2021%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfBBT=&pitchers_lookup%5B%5D="
end = paste(543037,"&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details&",sep="")
path = paste(beginning,end,sep="")
sp = read.csv(path)
l=data.frame()

ui <- fluidPage(
  
  # App title ----
  titlePanel("Strikezone Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput("fullname",h3("Pitcher Full Name"),"Gerrit Cole", placeholder="Firstname Lastname"),width=3
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
    bm = read.csv("https://baseballsavant.mlb.com/leaderboard/custom?year=2021&type=pitcher&filter=&sort=1&sortDir=desc&min=50&selections=b_total_pa,b_home_run,b_strikeout,b_walk,&chart=false&x=b_total_pa&y=b_total_pa&r=no&chartType=beeswarm&csv=true")
    
    firstname = strsplit(input$fullname, " ")[[1]][1]
    lastname = strsplit(input$fullname, " ")[[1]][2]
    
    temp = bm %>% filter_at(vars("first_name"), any_vars(. %in% paste(" ",firstname,sep="")))
    temp = temp %>% filter_at(vars("last_name"), any_vars(. %in% lastname))
    
    id = unique(temp$player_id)
    
    beginning = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=FF%7CFT%7CFC%7CSI%7CFS%7CSL%7CCH%7CCU%7CKC%7CCS%7CKN%7CFO%7CEP%7CSC%7CIN%7CPO%7CAB%7CUN%7C&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2021%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfBBT=&pitchers_lookup%5B%5D="
    end = paste(id,"&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details&",sep="")
    path = paste(beginning,end,sep="")
    sp = read.csv(path)
    
    for (i in 0:3){
      for(j in 0:2){
        c = filter(sp,balls==i,strikes==j)
        #print(c(i,j))
        li = data.frame(summary(c$pitch_name)/nrow(c),paste(i,j))
        li <- tibble::rownames_to_column(li, "Pitch")
        print(li)
        l = rbind(l,li)
      }
    }
    
    colnames(l) = c("Pitch","Percentage","Count")
    
    
    ggplot(l, aes(Pitch,Count)) +
      geom_tile(aes(fill = Percentage)) + 
      geom_text(aes(label = round(Percentage, 2)),size=7) +
      scale_fill_gradient(low = "lightblue", high = "red")+
      theme(text = element_text(size=25))
    
    
    
  },width=900,height=700)
  
}

shinyApp(ui = ui, server = server)
