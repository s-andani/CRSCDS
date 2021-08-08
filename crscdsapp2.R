####################################
# CRS CDS TOOL                     #
# Sameer Andani                    #
# http://github.com/s-andani       #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html


# Load R packages
library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)
library(dplyr)
#import ML model
modelurl <- "https://github.com/s-andani/CRSCDS/blob/main/crscdsmodelprime.rds?raw=true"
crsmodel <- readRDS(url(modelurl,method="libcurl"))
#import test data set
cdstesturl <- "https://github.com/s-andani/CRSCDS/raw/main/CDSin.csv"
cdstest <- read.csv(url(cdstesturl,method="libcurl"))
#Change test data set into factors
cdstest$SEX<- as.factor(cdstest$SEX)
cdstest$CRS_POLYPS<- as.factor(cdstest$CRS_POLYPS)
cdstest$ASTHMA<- as.factor(cdstest$ASTHMA)
cdstest$SMOKER<- as.factor(cdstest$SMOKER)
cdstest$DIABETES<- as.factor(cdstest$DIABETES)
cdstest$ALCOHOL<- as.integer(cdstest$ALCOHOL)

  # Define UI
  ui <- fluidPage(theme = shinytheme("yeti"),
    navbarPage(
      "Chronic Rhinosinusitis Clinical Decision Support Tool",
      tabPanel("Surgical Outcome Calculator",
               sidebarPanel(
                 tags$h3("Enter Patient Data:"),
                 numericInput("Age", 
                              label = "Age",
                              min = 1, max = 100, value = 40),
                 radioButtons("SEX", 
                              label = "SEX",
                              c("Male" = "Male",
                                "Female" = "Female")),
                 numericInput("PREVIOUS_SURGERY", "Prior Surgeries",
                              min = 0, max = 6, value = 0),
                 radioButtons("CRS_POLYPS", "Polyps",
                              c("No" = "0",
                                "Yes" = "1")),
                 radioButtons("ASTHMA", "Asthma",
                              c("No" = "0",
                                "Yes" = "1")),
                 radioButtons("SMOKER", "Smoker",
                              c("No" = "0",
                                "Yes" = "1")),
                 numericInput("ALCOHOL", "Alcohol use",
                              min = 0, max = 600, value = 0),
                 radioButtons("DIABETES", "Diabetes",
                              c("No" = "0",
                                "Type 1" = "1",
                                "Type 2" ="2")),
                 sliderInput("BLN_CT_TOTAL", "Lund-Mackay Score",
                             min = 0, max = 24, value = 0),
                 sliderInput("BLN_ENDOSCOPY_TOTAL", "Nasal Endoscopy Score",
                             min = 0, max = 12, value = 0),
                 sliderInput("SNOT22_BLN_TOTAL", "Current SNOT-22 Score",
                             min = 0, max = 110, value = 0),
                 actionButton("submitbutton", "Submit", 
                              class = "btn btn-primary")
               ), # sidebarPanel
               mainPanel(
                            h3("For more information on this tool, or Chronic Rhinosinusitis, click the tabs above"),
                            h2(strong(textOutput('score'))),
                            h3(textOutput('explanation')),
                            tableOutput('tabledata'), # Prediction results table
                            plotOutput('importancePlot')
                            
               ), # mainPanel

      ), 
      tabPanel("Info",
               mainPanel(
                 h3("Learn more about the tool below"),
                 img(src = "https://github.com/s-andani/CRSCDS/blob/main/cdsexplanationpicture.png?raw=true"))
               ), #End of Info Panel
      tabPanel("Export to EHR", 
               mainPanel(
                 h3("After hitting submit on the main page, an EHR export will be generated below"),
                 h3(verbatimTextOutput('emr')),
                 ) #End of EHR panel
      )
    ) # navbarPage
  ) # fluidPage

  
  
  ####################################
  # Server                           #
  ####################################
  
  server<- function(input, output, session) {
    
    # Input Data
    
    userinput <- reactive({  
      
      df <- data.frame(
        Name = c("Age",
                 "SEX",
                 "PREVIOUS_SURGERY",
                 "CRS_POLYPS",
                 "ASTHMA",
                 "SMOKER",
                 "ALCOHOL",
                 "DIABETES",
                 "BLN_CT_TOTAL",
                 "BLN_ENDOSCOPY_TOTAL",
                 "SNOT22_BLN_TOTAL",
                 "SNOT22_FU_TOTAL_6MONTH"),
        Value = as.character(c(input$Age,
                               input$SEX,
                               input$PREVIOUS_SURGERY,
                               input$CRS_POLYPS,
                               input$ASTHMA,
                               input$SMOKER,
                               input$ALCOHOL,
                               input$DIABETES,
                               input$BLN_CT_TOTAL,
                               input$BLN_ENDOSCOPY_TOTAL,
                               input$SNOT22_BLN_TOTAL,
                               "0")),
        stringsAsFactors = FALSE)

      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      #add the inputs to a csv called test
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      #change inputs from test csv to match training csv
      test$SEX<- as.factor(test$SEX)
      test$CRS_POLYPS<- as.factor(test$CRS_POLYPS)
      test$ASTHMA<- as.factor(test$ASTHMA)
      test$SMOKER<- as.factor(test$SMOKER)
      test$DIABETES<- as.factor(test$DIABETES)
      #Change formatting to match training dataset, the code below adds a row from the original dataset then removes it
      #str(test)
      test <- bind_rows(cdstest[1, ], test)
      #str(test)
      test <- test[-1,]
      #run the data against the model
      Output <- predict(crsmodel,test)
      print(Output)
      
    })
    # Text Output
    
    score <- isolate(userinput)
    output$score <- renderText({
      if (input$submitbutton>0) { 
        paste("With surgical intervention, your SNOT-22 score of ", input$SNOT22_BLN_TOTAL, "could decrease to", round(score(), digits =1))
      } 
    })
    output$emr <- renderText({
      if (input$submitbutton>0) { 
        paste("The patient and clincian inserted the patient's data into a clinical decision making tool developed at the University of Colorado School of Medicine. The tool uses a clinically validated machine learning algorithm [reference] to predict SNOT-22 score improvement after surgery. The patient's initial SNOT-22 score was predicted to decrease after surgical intervention from", input$SNOT22_BLN_TOTAL, "to", round(score(), digits =1))
      } 
    })
    output$explanation <- renderText({
      if (input$submitbutton>0) { 
        paste("In the charts below, you can see which factors rank highest in this calculation. The higher the rank on the x-axis, the greater that factor contributes to your score. Modifiable risk factors include Smoking, Alcohol Consumption and managing your diabetes!")
      } 
    })

    # Importance Plot
    output$importancePlot <- renderPlot({
      if (input$submitbutton>0){
        varImpPlot(crsmodel)
      }
    })

    
  }
  # Create Shiny object
  shinyApp(ui = ui, server = server)
