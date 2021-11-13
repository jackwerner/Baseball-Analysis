install.packages('shiny','ggplot2','dplyr','scales','png')
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(png)
beginning = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2021%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&batters_lookup%5B%5D="
end = paste(501659,"&hfFlag=&hfBBT=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details&",sep="")
path = paste(beginning,end,sep="")
pitchhistory = read.csv(path)
mypng = download.file('https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRfXgEMr84PMU2lUjk2smi748EEOzRX31eI0A&usqp=CAU', destfile = 'field.png', mode = 'wb')
field = readPNG('field.png')

ui <- fluidPage(
  
  # App title ----
  titlePanel("Mid-Game Batter Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      textInput("fullname",h3("Batter Full Name"),"Abraham Almonte", placeholder="Firstname Lastname"),
      
      # Input: Slider for the number of bins ----
      selectInput("POI", h3("Pitch Name"),selected=pitchhistory$pitch_name,
                  choices = pitchhistory$pitch_name,multiple = TRUE),
      
      radioButtons("hand", h3("Pitcher Handedness"),
                   choices = list("Both" = 1, "Lefty" = "L",
                                  "Righty" = "R"), selected = 1),
      selectInput("event", h3("Events"),selected=c("single","double","triple","home_run"),
                  choices = pitchhistory$events,multiple = TRUE)
      
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
    pitchhistory = read.csv(path)
    
    view = data.frame(pitchhistory %>% filter_at(vars("pitch_name"), any_vars(. %in% input$POI)))
    if(input$hand != 1){
      view = data.frame(view %>% filter_at(vars("p_throws"), any_vars(. %in% input$hand)))
    }
    view=data.frame(view %>% filter_at(vars("events"), any_vars(. %in% input$event)))
 
    ggplot(view, aes(x=hc_x, y = -hc_y,color=release_speed),height=500,width=500)+
      annotation_raster(field, ymin = -247, ymax= 0, xmin = -8,xmax = 260)+
      theme_bw()+
      theme(element_blank())+
      xlab("X-Coord")+
      ylab("Y-Coord")+
      xlim(0,250)+
      ylim(-204,0)+
      stat_density2d_filled(alpha=.8,show.legend = FALSE)+
      geom_point()+geom_point(x=126,y=-204,color="black",size=4)+
      scale_color_gradient(low = "black", high = "red")
    
  },width=800,height=700)
  
}

shinyApp(ui = ui, server = server)
