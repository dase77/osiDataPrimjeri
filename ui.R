library(shiny)

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Primjeri proba"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroup", 
                         label = h3("Razredi"), 
                         choices = list("Šesti" = "šesti", "Sedmi" = "sedmi", "Osmi"="osmi","Deveti"="deveti"),
                         selected = "šesti"),
      selectInput("var", 
                  label = "Odaberi",
                  choices = list("Negativni oblici ponašanja"=8, 
                                 "Univerzalne vrijednosti"=10,
                                 "Partikularizacija"=12),
                  selected =8),
      
      selectInput("var1", 
                  label = "Odaberi NPP",
                  choices = list("Svi"="svi", 
                                 "Bosanski"="B",
                                 "Srpski"="S",
                                 "Hrvatski"="H"),
                  selected ="svi"),
      checkboxInput("checkbox", label = "Pregled po predmetima", value = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('grafikoni',plotOutput("plotosi"),plotOutput("plot1osi")),
        tabPanel('tabele',verbatimTextOutput("tableosi"),verbatimTextOutput("table1osi")),
        tabPanel('primjeri',tableOutput("primjeriosi"))
        
      
        
        
        
      )
        
      )
  )
))
