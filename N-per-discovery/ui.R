library(shiny)
library(shinythemes)

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      icon("question")
    )
  )
}


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("spacelab"),
                  
                  title = "Maximizing Scientific Discovery Return on Investment",
                  
                  h2(HTML("What research approach leads to more true discoveries per total resources?")),
                  
                  br(),
                  p(HTML("Instructions: Use presets or change parameters manually to identify research 
                         approach that maximizes true scientific discovery return on investment.
                         For detailed explanation and conceptual background, see LeBel, Campbell, & Loving 
                         (in press, JPSP), Table 3.")),
                  
                  # Sidebar with a slider input for the number of bins
                  fluidRow(
                    column(4,
                           
                           selectInput("preset","Presets:", c(
                             "---"="---",
                             "1: Status quo (high Type I, low power)"="p1",
                             "2: Open science (low Type I, high power)"="p2",
                             "3: Error balance (Type I = Type II = .05)"="p3"
                           )),
                           
                           h3(HTML("Main variables")),
                           
                           p(HTML("Type I error rate:"), style = "font-style: italic; font-size: 0.85em; color:black"),
                           sliderInput("alpha", label = NULL, min = 0.01, max = 0.5, value = 0.05, step = 0.01),
                           
                           p(HTML("Power level of original studies:"), style = "font-style: italic; font-size: 0.85em; color:black"),
                           sliderInput("power", label = NULL, min = 0.06, max = 0.99, value = 0.25, step = 0.01),
                           
                           h3(HTML("Assumptions")),
                           
                           p("Base rate of true hypotheses:", style = "font-style: italic; font-size: 0.85em; color:black"),    
                           sliderInput("percTrue", label = NULL, min = 0.01, max = .99, value = .10, step = .01),
                           
                           p(HTML("Individual researcher subject pool resources (N):"), style = "font-style: italic; font-size: 0.85em; color:black; line-height:30%"),
                           sliderInput("indResources", label = NULL, min = 1, max = 10000, value = 5000, step = 100),
                           
                           p(HTML("Number of replications per positive results:"), style = "font-style: italic; font-size: 0.85em; color:black; line-height:30%"),
                           sliderInput("repsPerPositiveResults", label = NULL, min = 1, max = 10, value = 2, step = 1),
                           
                           p(HTML("Power of replication studies:"), style = "font-style: italic; font-size: 0.85em; color:black; line-height:30%"),
                           sliderInput("powerOfReps", label = NULL, min = .06, max = .99, value = .95, step = .01),
                           
                           p(HTML("Average social psychology effect size (Cohen's d):"), style = "font-style: italic; font-size: 0.85em; color:black; line-height:30%"),
                           sliderInput("avgEffectSize", label = NULL, min = .01, max = 2.5, value = .41, step = .01)
                    ),
                    column(8,
                           htmlOutput("res")
                    )			
                  ),
                  
                  HTML("This app is an extension of Zehetleitner and Felix Schönbrodt's (2016) positive predictive value <a href='http://87.106.45.173:3838/felix/PPV'>app</a>. Source code for this app available <a href='https://github.com/eplebel/shiny-apps/tree/master/N-per-discovery'>here</a>.")
))