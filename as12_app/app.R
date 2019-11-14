library(shiny)
library(ggplot2)
library(dplyr)

dataset1 <- readr::read_csv("bc_10.csv") 
dataset2 <- readr::read_csv("bc_15.csv") 
dataset3 <- readr::read_csv("bc_20.csv") 
dataset4 <- readr::read_csv("bc_25.csv") 
dataset5 <- readr::read_csv("bc_31.csv") 
dataset6 <- readr::read_csv("jac_31.csv") 

ui <- navbarPage("Assignment 12",
    tabPanel("exercise 1",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("kmer", "kmer size:",
                                 c("10 bp" = "dataset1",
                                   "15 bp" = "dataset2",
                                   "20 bp" = "dataset3",
                                   "25 bp" = "dataset4",
                                   "31 bp" = "dataset5"))
                 ),
                 mainPanel(
                     # Output: dendrogram
                     plotOutput("dendro")
                 )
             )
    ) # end tab panel1
    ,tabPanel("Exercise 2",
              sidebarLayout(
                  sidebarPanel(
                      selectInput("distance", "distance metric:",
                                  c("Bray-curtis" = "bray",
                                      "Jaccard" = "jaccard"))
                  ),
                  mainPanel(
                      # Output: dendrogram
                      plotOutput("dendro2")
                  )
              )
    )
)

server <- function(input, output) {
    #server for exercise 1
    choice <- reactive({
        if (input$kmer == "dataset1") {choice_dataset<-dataset1}
        else if(input$kmer == "dataset2"){choice_dataset<-dataset2}
        else if(input$kmer == "dataset3"){choice_dataset<-dataset3}
        else if(input$kmer == "dataset4"){choice_dataset<-dataset4}
        else if(input$kmer == "dataset5"){choice_dataset<-dataset5}
        return(choice_dataset)
    })
     output$dendro <- renderPlot({  
         dataset <- choice() %>% select(-X1) %>% rename(stool1 =ID1, stool2=ID2, tongue1=ID3, tongue2=ID4, dental=ID5, stool3=ID6)
         matrix <- as.dist(dataset, diag = TRUE)
         fit<-hclust(matrix, method="ward.D")
         plot(fit) # display dendogram
     }) 
     
     #server for exercise 2
     choice2 <- reactive({
         if (input$distance == "bray") {choice_dataset2<-dataset5}
         else {choice_dataset2<-dataset6}
         return(choice_dataset2)
     })
     output$dendro2 <- renderPlot({  
         dataset <- choice2() %>% select(-X1) %>% rename(stool1 =ID1, stool2=ID2, tongue1=ID3, tongue2=ID4, dental=ID5, stool3=ID6)
         matrix <- as.dist(dataset, diag = TRUE)
         fit<-hclust(matrix, method="ward.D")
         plot(fit) # display dendogram
     }) 
    
    
}
shinyApp(ui = ui, server = server)