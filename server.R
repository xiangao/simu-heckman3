library(shiny)
library(ggplot2)
library(reshape)
library(plyr)

results <- read.csv('results2.csv')


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
output$plot1 <-  renderPlot({
#    nobs = input$n
    cr1w=input$cr1w
    cr1u=input$cr1u
    sel.cri <- input$sel.cri
    match <- results[((abs(cr1w-results$cr1w) < 1e-5) & (abs(cr1u - results$cr1u) < 1e-5) & (abs(sel.cri-results$sel.cri) < 1e-5)   &  !is.na(results$lm)),c('nobs','nsim','lm','heck.noinst','heck.inst')]
    match.melt <- melt(match, id.vars=c('nobs','nsim'))
    names(match.melt) <- c('nobs','nsim','type','bias')
    ave.bias <- ddply(match.melt, c('type'),
                       function(x) data.frame(
                         mean.bias=mean(x$bias)
                         ))
     ## mse <- ddply(match.melt, c('type'),
     ##             function(x) data.frame(
     ##                 mse =mean((x$bias)^2, na.rm=TRUE),
     ##                 nobs=nobs
     ##                 ))
    match.melt$bias2 <- match.melt$bias^2
    match.melt$mse <- ave(match.melt$bias2, match.melt[,c('type','nobs')], FUN=mean)

    p <- ggplot(data=match.melt, aes(x=nobs, y=mse,  colour=type))   + labs(title="MSE comparison") +  geom_line()

    print(p)
#    plot(lm ~ heckit, data=match)
})

})
