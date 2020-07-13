#Last Updated on July 13 2020

#Loading Libraries
library(shiny)
library(shinythemes)
library(stringr)
library(readr)
library(ggplot2)
library(gdata)
library(dplyr)
    
#Farmers Data
data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/farmer/getdata.php") 

#Keeping Data after August 2nd
data.all <- data.all %>% mutate(Date = str_sub(Date, 1, 10))
data.all$Date <- as.Date(data.all$Date, format = "%Y-%m-%d")
data.all <- data.all %>% filter(Date >= as.Date("08/02/2019", format = "%m/%d/%Y"))

#Changing Mystery to Tomato
data.all$Crop[data.all$Crop == "Mystery"] <- "Tomatoes"
data.all$PriorHarvest[data.all$PriorHarvest == "Mystery"] <- "Tomatoes"
data.all$PriorHarvest[data.all$PriorHarvest == "Tomato"] <- "Tomatoes"

#Filtering Data
data.all <- filter(data.all, GameNum > 1400)
data.all <- mutate(data.all, NitrateLevel = ifelse(NitrateLevel < 500, NitrateLevel, 500))
data.all <- filter(data.all, TotalWater > 0, TotalWater < 70)
data.all <- filter(data.all, Crop %in% c("Corn", "Beans", "Tomatoes"))


#Converting to Columns to Factor/Character
data.all$Plot <- as.factor(data.all$Plot)
data.all$GroupID <- as.character(tolower(data.all$GroupID))
data.all$PlayerID <- as.character(tolower(data.all$PlayerID))
data.all$Season <- as.factor(data.all$Season)

#Making Insect Binary
data.all$Insects <- as.factor(data.all$Insects)

#Creating a Categorical Nitrate Column
data.all$Nitrate <- NA

for(i in 1:nrow(data.all)){
  
  #Low Nitrate Level
  if(data.all$NitrateLevel[i] <= 200){
    data.all$Nitrate[i] <- "Low"
    
    #Medium Nitrate Level  
  } else if(data.all$NitrateLevel[i] > 200 &
            data.all$NitrateLevel[i] <= 350){
    data.all$Nitrate[i] <- "Medium"
    
    #High Nitrate Level  
  } else{
    data.all$Nitrate[i] <- "High"
  }
  
}

#Ordering Categorical Nitrate Column
data.all$Nitrate <- factor(data.all$Nitrate, levels = c("Low", "Medium", "High"))


#Rounding Yield (Ask Kuiper)
#data.all$Yield <- round(data.all$Yield, 0)


#Creating Y Variable Columns

#Revenue/Costs/Profit
data.all <- data.all %>% mutate(Revenue = Yield * SellPrice,
                                Costs = BuyPrice + WaterAdded + PesticidesAdded*5,
                                Profit = Revenue - Costs)

#For UI Inputs
all_groups <- sort(unique(data.all$GroupID))
#all_players <- sort(unique(data.all$PlayerID))



##UI
ui <- fluidPage(
  theme = shinytheme("journal"),
  
  titlePanel("Farmers Data Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  all_groups,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "ftest"),
      
      uiOutput(outputId = "player_input"),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("TotalWater", "NitrateLevel"),
                  selected = "TotalWater",
                  multiple = FALSE),
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("Yield", "Revenue", "Costs", "Profit"),
                  selected = "Yield",
                  multiple = FALSE),
      
      selectInput(inputId = "crop2",
                  label = "Select Crop",
                  choices = c("Corn", "Beans", "Tomatoes"),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Corn", "Beans", "Tomatoes")),
      
      selectInput(inputId = "color",
                  label = "Color by:",
                  choices = c("Crop", "Season", "Plot", "Insects", "PlayerID", "TotalWater", "NitrateLevel"),
                  selected = "Crop",
                  multiple = FALSE),
      
      selectInput(inputId = "facets",
                  label = "Facet by:",
                  choices = c("None", "Crop", "Season", "Plot", "Insects", "PlayerID", "Nitrate"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "model",
                  label = "Statistical Model:",
                  choices = c("None", "Linear", "Quadratic", "Cubic", "Smoother"),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "None"),
      
      checkboxInput(inputId = "interaction",
                    label = "Remove Interaction Terms",
                    value = FALSE),
      
      sliderInput("sliderN", label = "Nitrate Levels:", min = 0, 
                  max = 500, value = c(0, 500)),
      
      uiOutput(outputId = "limits"),
      
      
      downloadButton('downloadData', label = "Farmer Data")
      
    ),
    
    mainPanel(
      plotOutput(outputId = "Plot"),
      verbatimTextOutput(outputId = "Model_Out"))
    
    
  ))


##Server
server <- function(input, output,session) {
  
  #Reactive Data
  plotDataR <- reactive({
    
    #Require
    req(input$limits_in)
    
    #Filtering by Group ID
    data <- data.all %>% filter(GroupID %in% input$groupID)
    
    #Filtering by Remove Player ID
    data <- data %>% filter(!(PlayerID %in% input$playerID))
    
    #Filtering by Crop
    data <- data %>% filter(Crop %in% input$crop2)
    
    #Filtering by Nitrate Level
    data <- data %>% filter(NitrateLevel >= input$sliderN[1],
                            NitrateLevel <= input$sliderN[2])
    
    #Filtering by X Axis Limits
    if(input$xvar == "TotalWater"){
      data <- data %>% filter(TotalWater >= input$limits_in[1],
                              TotalWater <= input$limits_in[2])
      
    } else if(input$xvar == "NitrateLevel"){
      data <- data %>% filter(NitrateLevel >= input$limits_in[1],
                              NitrateLevel <= input$limits_in[2])
    }
    
  })
  
  
  ##Dynamic Inputs
  
  #Dynamic Remove Player Input
  output$player_input <- renderUI({
    
    input_data <- data.all %>% filter(GroupID %in% input$groupID)
    players <- sort(unique(input_data$PlayerID))
    
    selectInput(inputId = "playerID",
                label = "Remove Player ID:",
                choices =  players,
                multiple = TRUE,
                selectize = TRUE)
    
    
  })
  
  #Dynamic X Axis Limits Input
  output$limits <- renderUI({
    
    if(input$xvar == "TotalWater"){
      
      sliderInput(inputId = "limits_in",
                  label = "X Axis Limits:",
                  min = min(data.all$TotalWater),
                  max = max(data.all$TotalWater),
                  value = c(min(data.all$TotalWater), max(data.all$TotalWater)))
      
    }
    
    else if(input$xvar == "NitrateLevel"){
      
      sliderInput(inputId = "limits_in",
                  label = "X Axis Limits:",
                  min = min(data.all$NitrateLevel),
                  max = max(data.all$NitrateLevel),
                  value = c(min(data.all$NitrateLevel), max(data.all$NitrateLevel)))
    }
  })
  
  
   
  ##Creating Visualization
  output$Plot <- renderPlot({
    
    #Require Inputs
    req(input$groupID)
    
    #Reactive Data
    plotData <- plotDataR()
    
    #There has to be data
    if(nrow(plotData) > 0){
      
      #Setting Up
      XVariable <- plotData %>% pull(input$xvar)
      YVariable <- plotData %>% pull(input$yvar)
      ColorVariable <- plotData %>% pull(input$color)
      
      
      #If model option is NOT none or NOT Smoother
      if(input$model != "None" & input$model != "Smoother"){
        
        #More than one level for color is needed to run the model
        if(nlevels(drop.levels(ColorVariable)) > 1){
          
          #Remove Interaction checkbox is selected
          if(input$interaction == TRUE){
            
            #Facet option is none
            if(input$facets == "None"){
              
              if(input$model == "Linear"){
                myModel <- lm(YVariable ~ XVariable + ColorVariable)
                
              } else if(input$model == "Quadratic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable)
                
                
              } else if(input$model == "Cubic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable)
              }
              
              #Adding predicted values column for Linear/Quadratic/Cubic
              plotData <- cbind(plotData, predict(myModel, interval = "confidence"))     
              
              #Facet option is NOT none 
            } else{
              
              #Pulling Facet Variable
              FacetVariable <- plotData %>% pull(input$facets)
              
              #More than one level for facet variable is needed to run the model
              if(nlevels(drop.levels(FacetVariable)) > 1){
                
                if(input$model == "Linear"){
                  myModel <- lm(YVariable ~ XVariable + ColorVariable + FacetVariable)
                  
                } else if(input$model == "Quadratic"){
                  myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + FacetVariable)
                  
                  
                } else if(input$model == "Cubic"){
                  myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable + FacetVariable)
                }
                
                #Adding predicted values column for Linear/Quadratic/Cubic
                plotData <- cbind(plotData, predict(myModel, interval = "confidence"))     
              }
            }
            
            #Remove Interaction checkbox it NOT selected
          } else{
            
            #Facet option is none
            if(input$facets == "None"){
              
              if(input$model == "Linear"){
                myModel <- lm(YVariable ~ (XVariable + ColorVariable + XVariable*ColorVariable))
                
              } else if(input$model == "Quadratic"){
                
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + XVariable*ColorVariable +
                                I(XVariable^2) * ColorVariable)
                
              } else if(input$model == "Cubic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable +
                                XVariable*ColorVariable +  I(XVariable^2)*ColorVariable +
                                I(XVariable^3)*ColorVariable)
              }
              
              #Adding predicted values column for Linear/Quadratic/Cubic
              plotData <- cbind(plotData, predict(myModel, interval = "confidence"))     
              
              #Facet option is NOT none 
            } else{
              
              #Pulling Facet Variable
              FacetVariable <- plotData %>% pull(input$facets)
              
              #More than one level for facet variable is needed to run the model
              if(nlevels(drop.levels(FacetVariable)) > 1){
                
                if(input$model == "Linear"){
                  myModel <- lm(YVariable ~ XVariable + ColorVariable + FacetVariable + XVariable*ColorVariable +
                                  XVariable*FacetVariable + ColorVariable*FacetVariable)
                  
                } else if(input$model == "Quadratic"){
                  myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + FacetVariable +
                                  XVariable*ColorVariable + XVariable*FacetVariable +
                                  I(XVariable^2)*ColorVariable + I(XVariable^2)*FacetVariable +
                                  ColorVariable*FacetVariable)
                  
                  
                } else if(input$model == "Cubic"){
                  myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable + FacetVariable +
                                  XVariable*ColorVariable + XVariable*FacetVariable +
                                  I(XVariable^2)*ColorVariable + I(XVariable^2)*FacetVariable +
                                  I(XVariable^3)*ColorVariable + I(XVariable^3)*FacetVariable +
                                  ColorVariable*FacetVariable)
                }
              
              #Adding predicted values column for Linear/Quadratic/Cubic 
              plotData <- cbind(plotData, predict(myModel, interval = "confidence"))     
              
              } 
            }
          }
          
        } 
        
        #If model option is none or smoother
      } else{
        myModel <- ""
      }
      
    
      
    
      #Default Plot
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
        geom_point() +
        labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) 
      
      
      #Facet option is selected
      if(input$facets != "None"){
        myplot <- myplot +
          facet_wrap(as.formula(paste("~", input$facets))) +
          labs(title = paste("Plot of",input$yvar, "by", input$xvar, "and Colored by", input$color, "and Faceted by", input$facets)) +
          theme(strip.text = element_text(size = 16)) 
      }
      
      
      #If remove interaction checkbox is not selected
      if(input$interaction == FALSE){
      
        #None or Smoother
        if(input$model %in% c("None", "Smoother")){
          
          #Model Option - None
          if(input$model == "None"){
            myplot <- myplot
        
          #Model Option - Smoother
          } else if(input$model == "Smoother"){
             myplot <- myplot +
              stat_smooth(se = FALSE)
          }
      
        #Linear/Quadratic/Cubic
        } else if(input$model %in% c("Linear", "Quadratic", "Cubic")){
          
          #Color variable has more than one level
          if(nlevels(drop.levels(ColorVariable)) > 1){
            
            #Facet Option is None
            if(input$facets == "None"){
              
              #Model Option - Linear
              if(input$model == "Linear"){
                myplot <- myplot + 
                  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
              
              #Model Option - Quadratic
              } else if(input$model == "Quadratic"){
                myplot <- myplot +
                  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE)
                
              #Model Option - Cubic
              } else if(input$model == "Cubic"){
                myplot <- myplot +
                  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)
            } 
            
          #Facet Option is NOT None
            } else{
  
              #Pulling Facet Variable
              FacetVariable <- plotData %>% pull(input$facets)
              
              #Facet Variable has more than one level
              if(nlevels(drop.levels(FacetVariable)) > 1){
              
                #Model Option - Linear
                if(input$model == "Linear"){
                  myplot <- myplot + 
                    stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
                  
                  #Model Option - Quadratic
                } else if(input$model == "Quadratic"){
                  myplot <- myplot +
                    stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE)
                  
                  #Model Option - Cubic
                } else if(input$model == "Cubic"){
                  myplot <- myplot +
                    stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)
                } 
              }
            }
          }
        }
       
      #If remove interaction checkbox is selected
      } else{
        
        #None or Smoother
        if(input$model %in% c("None", "Smoother")){
          
          #Model Option - None
          if(input$model == "None"){
            myplot <- myplot
            
            #Model Option - Smoother
          } else if(input$model == "Smoother"){
            myplot <- myplot +
              stat_smooth(se = FALSE)
          }
        
        #Linear/Quadratic/Cubic
       } else if(input$model %in% c("Linear", "Quadratic", "Cubic")){
        
         #Color variable has more than one level
         if(nlevels(drop.levels(ColorVariable)) > 1){
           
           #Facet Option is None
           if(input$facets == "None"){
               myplot <- myplot + 
                 geom_line(aes(y = fit), size = 1)
      
            #Facet Option is NOT None
             } else{
        
              #Pulling Facet Variable
              FacetVariable <- plotData %>% pull(input$facets)
           
              #Facet Variable has more than one level
              if(nlevels(drop.levels(FacetVariable)) > 1){
                 myplot <- myplot + 
                    geom_line(aes(y = fit), size = 1)
           }
         }
       }
     }
    }    
      
      return(myplot)
      
      
    }
  })
  
  
  
  ##Model
  output$Model_Out <- renderPrint({
    
    #Reactive Data
    plotData <- plotDataR()
    
    #Require
    req(input$groupID)
    
    
    #There has to be data
    if(nrow(plotData) > 0){
      
      #Setting Up
      XVariable <- plotData %>% pull(input$xvar)
      YVariable <- plotData %>% pull(input$yvar)
      ColorVariable <- plotData %>% pull(input$color)
     
      
      #If model option is NOT none or NOT Smoother
      if(input$model != "None" & input$model != "Smoother"){
        
        #More than one level for color is needed to run the model
        if(nlevels(drop.levels(ColorVariable)) > 1){
        
        #Remove Interaction checkbox is selected
        if(input$interaction == TRUE){
          
          #Facet option is none
          if(input$facets == "None"){
            
              if(input$model == "Linear"){
                myModel <- lm(YVariable ~ XVariable + ColorVariable)
                
              } else if(input$model == "Quadratic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable)
                
                
              } else if(input$model == "Cubic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable)
              }
              
              return(summary(myModel))
            
            #Facet option is NOT none 
          } else{
            
            #Pulling Facet Variable
            FacetVariable <- plotData %>% pull(input$facets)
            
            #More than one level for facet variable is needed to run the model
            if(nlevels(drop.levels(FacetVariable)) > 1){
          
              if(input$model == "Linear"){
                myModel <- lm(YVariable ~ XVariable + ColorVariable + FacetVariable)
                
              } else if(input$model == "Quadratic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + FacetVariable)
                
                
              } else if(input$model == "Cubic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable + FacetVariable)
              }
              return(summary(myModel))
             
          
          #Error message if there is only one level for facet variable    
            } else {
              "More than one level is needed to run the model. Please select a different Facet Variable."}
              }
          
          #Remove Interaction checkbox it NOT selected
        } else{
          
          #Facet option is none
          if(input$facets == "None"){
            
              if(input$model == "Linear"){
                myModel <- lm(YVariable ~ (XVariable + ColorVariable + XVariable*ColorVariable))
                
              } else if(input$model == "Quadratic"){
                
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + XVariable*ColorVariable +
                                I(XVariable^2) * ColorVariable)
                
              } else if(input$model == "Cubic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable +
                                XVariable*ColorVariable +  I(XVariable^2)*ColorVariable +
                                I(XVariable^3)*ColorVariable)
              }
              
              return(summary(myModel))
    
          
            #Facet option is NOT none 
          } else{
            
            #Pulling Facet Variable
            FacetVariable <- plotData %>% pull(input$facets)
            
            #More than one level for facet variable is needed to run the model
            if(nlevels(drop.levels(FacetVariable)) > 1){
          
              if(input$model == "Linear"){
                myModel <- lm(YVariable ~ XVariable + ColorVariable + FacetVariable + XVariable*ColorVariable +
                                XVariable*FacetVariable + ColorVariable*FacetVariable)
                
              } else if(input$model == "Quadratic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + FacetVariable +
                                XVariable*ColorVariable + XVariable*FacetVariable +
                                I(XVariable^2)*ColorVariable + I(XVariable^2)*FacetVariable +
                                ColorVariable*FacetVariable)
                
                
              } else if(input$model == "Cubic"){
                myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable + FacetVariable +
                                XVariable*ColorVariable + XVariable*FacetVariable +
                                I(XVariable^2)*ColorVariable + I(XVariable^2)*FacetVariable +
                                I(XVariable^3)*ColorVariable + I(XVariable^3)*FacetVariable +
                                ColorVariable*FacetVariable)
              }
              
              return(summary(myModel))
            
        
        #Error message if there is only one level for facet variable    
          } else{
            "More than one level is needed to run the model. Please select a different Facet Variable."
            }
          }
        }
        
    #Error message if there is only one level for color  
      } else{
        "More than one level is needed to run the model. Please select a different Color Variable."
        }
        
        
  #If model option is none or smoother
    } else{
      myModel <- ""
      return(invisible(myModel))
    }
  }
    
  })
  
  
  #Download Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
    })
  
  #Closes server   
}

#Running Shiny App
shinyApp(ui = ui, server = server)
