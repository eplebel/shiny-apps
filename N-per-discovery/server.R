library(shiny)
require(pwr)
N.per.discovery.fxn = function(power.to.detect.avg.effect, baserate=.1,
                           alpha=.05,No.reps.per.positive.results=2,
                           power.of.reps=.95,subject.pool.resources=5000,avg.effect.size=.41){
  proportion.true.positives = power.to.detect.avg.effect * baserate
  proportion.false.positives = alpha * (1 - baserate)
  true.discovery.rate = proportion.true.positives / (proportion.true.positives + proportion.false.positives)
  N.original.studies = pwr.t.test(d=avg.effect.size, sig.level=.05, power=power.to.detect.avg.effect, type = c("two.sample"))$n * 2
  no.original.studies = subject.pool.resources / N.original.studies
  no.studies.yielding.positive.results = no.original.studies * (proportion.true.positives + proportion.false.positives)
  no.true.discoveries = true.discovery.rate * no.studies.yielding.positive.results
  no.replication.studies.required = no.studies.yielding.positive.results * No.reps.per.positive.results
  N.of.reps = pwr.t.test(d=avg.effect.size, sig.level=.05, power=power.of.reps, type = c("two.sample"))$n * 2
  total.N.of.replications = no.replication.studies.required * N.of.reps
  value = total.N.of.replications / no.true.discoveries
  value
}

presetselection <- new.env()
presetselection$sel <- "p1"

# Define server logic required to execute calculations
shinyServer(function(input, output, session) {
  
  # compute N.per.discovery etc only once for all outputs--> reactive values
  compute.N.per.discovery <- reactive({ 
    
    # if user changes a parameter: reset preset selector to "---"
    isolate({updateSelectInput(session, inputId = "preset", selected=presetselection$sel)})
    presetselection$sel <- "---"
    
    alpha <- input$alpha # Alpha level
    power <- input$power  # power
    baserate <- input$percTrue # base rate of true hypotheses
    subject.pool.resources <- input$indResources
    No.reps.per.positive.results <- input$repsPerPositiveResults
    power.of.reps <- input$powerOfReps
    avg.effect.size <- input$avgEffectSize
    
    N.original.studies <- pwr.t.test(d=avg.effect.size, sig.level=.05, power=power, type = c("two.sample"))$n * 2
    
    proportion.true.positives = power * baserate
    proportion.false.positives = alpha * (1 - baserate)
    true.discovery.rate = proportion.true.positives / (proportion.true.positives + proportion.false.positives)
    no.original.studies = subject.pool.resources / N.original.studies
    no.studies.yielding.positive.results = no.original.studies * (proportion.true.positives + proportion.false.positives)
    no.true.discoveries = true.discovery.rate * no.studies.yielding.positive.results
    no.replication.studies.required = no.studies.yielding.positive.results * No.reps.per.positive.results
    N.of.reps = pwr.t.test(d=avg.effect.size, sig.level=.05, power=power.of.reps, type = c("two.sample"))$n * 2
    total.N.of.replications = no.replication.studies.required * N.of.reps
    N.per.discovery = total.N.of.replications / no.true.discoveries
    
    return(list(N.per.discovery=N.per.discovery, true.discovery.rate=true.discovery.rate, 
                no.original.studies=no.original.studies, N.original.studies=N.original.studies, 
                no.studies.yielding.positive.results=no.studies.yielding.positive.results,
                no.true.discoveries=no.true.discoveries, total.N.of.replications=total.N.of.replications))
  })
  
  
  output$res <- renderUI({
    N.per.discovery <- compute.N.per.discovery()
    
    
    return(list(
      h6(HTML(paste0("RESULTS (for currently selected parameter values):<br><br>
        No. of original studies conducted (each with N = ", round(N.per.discovery$N.original.studies), "): ", round(N.per.discovery$no.original.studies, digits=1), "<br>
          No. studies yielding positive results: ", round(N.per.discovery$no.studies.yielding.positive.results, digits=1), "<br>
          No. of true discoveries: ", round(N.per.discovery$no.true.discoveries, digits=1), "<br>
          Total N of replication studies required to distinguish true from false discoveries: ", round(N.per.discovery$total.N.of.replications), "<br><br>
          <b>True discovery rate (TDR)</b>: ", round(N.per.discovery$true.discovery.rate, digits=3), "<br>
				 <b>Total resources (N) per discovery</b>: ", round(N.per.discovery$N.per.discovery),
                     "<hr>" ))),
      renderPlot({
        #PANEL A: Resources per discovery (N) of three approaches as a function of base rate values
        x <- seq(from = .01, to = .50, by = .01)
        y1 <- N.per.discovery.fxn(power.to.detect.avg.effect = .25, alpha = .35, baserate = x, 
                                  subject.pool.resources = input$indResources, 
                                  No.reps.per.positive.results = input$repsPerPositiveResults,
                                  power.of.reps = input$powerOfReps,
                                  avg.effect.size = input$avgEffectSize)
        y2 <- N.per.discovery.fxn(power.to.detect.avg.effect=.80, alpha=.05, baserate=x, 
                                  subject.pool.resources = input$indResources, 
                                  No.reps.per.positive.results = input$repsPerPositiveResults,
                                  power.of.reps = input$powerOfReps,
                                  avg.effect.size = input$avgEffectSize)
        y3 <- N.per.discovery.fxn(power.to.detect.avg.effect=.95, alpha=.05, baserate=x, 
                                  subject.pool.resources = input$indResources, 
                                  No.reps.per.positive.results = input$repsPerPositiveResults,
                                  power.of.reps = input$powerOfReps,
                                  avg.effect.size = input$avgEffectSize)
        plot(c(0,.5), c(0,3), type="n", xlab="Base rate of true hypotheses", 
             ylab="Resources per discovery (N)", ylim=c(0,.8*max(y1,y2,y3)), xaxs="i", yaxs="i"
             , main = "Comparing Research Approaches as a Function of Base Rate of True Hypotheses")
        lines(x, y1, col="red", lwd=2)
        lines(x, y2, col="blue", lwd=2)
        lines(x, y3, col="green", lwd=2)
        legend("topright", inset=.05, cex = 1,
               c("Status quo (high Type I, low power)", "Open science (low Type I, high power)", "Error equivalence (Type I = Type II = .05)"),
               horiz=FALSE, lty=c(1,1), lwd=c(2,2), col=c("red","blue", "green"))
        text(.4, .60*max(y1,y2,y3), paste("Researcher resources (N) = ", input$indResources), cex = 1)
        text(.4, .55*max(y1,y2,y3), paste("Number of reps per positive results = ", input$repsPerPositiveResults), cex = 1)
        text(.4, .50*max(y1,y2,y3), paste("Power of replication studies = ", input$powerOfReps), cex = 1)
        text(.4, .45*max(y1,y2,y3), paste("Average social psych effect size = ", input$avgEffectSize), cex = 1)
      }, height=600)
      
    ))
    
  })
  
  
  # ---------------------------------------------------------------------
  # Load demo data
  observeEvent(input$preset, {
    switch(input$preset,
           "p1" = {				
             updateSliderInput(session, inputId = "alpha", value = 0.35)
             updateSliderInput(session, inputId = "power", value = 0.25)
             updateSliderInput(session, inputId = "percTrue", value = 0.1)
             updateSliderInput(session, inputId = "indResources", value = 5000)
             updateSliderInput(session, inputId = "repsPerPositiveResults", value = 2)
             updateSliderInput(session, inputId = "powerOfReps", value = 0.95)
             updateSliderInput(session, inputId = "avgEffectSize", value = 0.41)
           },
           "p2" = {
             updateSliderInput(session, inputId = "alpha", value = 0.05)
             updateSliderInput(session, inputId = "power", value = 0.80)
             updateSliderInput(session, inputId = "percTrue", value = 0.1)
             updateSliderInput(session, inputId = "indResources", value = 5000)
             updateSliderInput(session, inputId = "repsPerPositiveResults", value = 2)
             updateSliderInput(session, inputId = "powerOfReps", value = 0.95)
             updateSliderInput(session, inputId = "avgEffectSize", value = 0.41)
           },
           "p3" = {				
             updateSliderInput(session, inputId = "alpha", value = 0.05)
             updateSliderInput(session, inputId = "power", value = 0.95)
             updateSliderInput(session, inputId = "percTrue", value = 0.1)
             updateSliderInput(session, inputId = "indResources", value = 5000)
             updateSliderInput(session, inputId = "repsPerPositiveResults", value = 2)
             updateSliderInput(session, inputId = "powerOfReps", value = 0.95)
             updateSliderInput(session, inputId = "avgEffectSize", value = 0.41)
           }					
    )
    presetselection$sel <- input$preset
  })
  
})