library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

ui <- fluidPage(
  withMathJax(),
   titlePanel("Malthus' Bevölkerungsdynamiken als einfaches Differenzialgleichungsmodell"),
   
   sidebarLayout(
     sidebarPanel = sidebarPanel(
       helpText("This is help text at the top."),
       sliderInput("lohn_growth", 
                   label='Parameter \\( \\alpha \\)',
                   min = 0, max = 1, step=0.05, value = 0.1),
       helpText("Tendenz zum Bevölkerungswachstum."),
       sliderInput("interaktionsterm_1", 
                   label='Parameter \\( \\beta \\)',
                 min = 0, max = 1, step=0.1, value = 0.3),
       helpText("Interaktionsterm 1"),
       sliderInput("interaktionsterm_2", 
                   label='Parameter \\( \\gamma \\)',
                   min = 0, max = 1, step=0.1, value = 0.1),
       helpText("Interaktionsterm 2"),
       sliderInput("todesterm", 
                   label='Parameter \\( \\delta \\)',
                   min = 0.05, max = 1, step=0.05, value = 0.1),
       uiOutput('delta')
       ),
     mainPanel = mainPanel(
       plotOutput("dynamics_plot"),
       uiOutput('f1'),
       uiOutput('f2'),
       p("wobei N für die Bevölkerung und W für die Löhne steht.")
       )
     )
   )

server <- function(input, output) {

  output$f1 <- renderUI({
    withMathJax(
      helpText('$$\\frac{\\partial N}{\\partial t} = \\gamma WN - \\delta N$$'))
  })
  output$f2 <- renderUI({
    withMathJax(
      helpText('$$\\frac{\\partial W}{\\partial t} = (1+\\alpha) W + \\beta WN - \\delta W$$'))
  })
  
  
   output$dynamics_plot <- renderPlot({
     # Fixe Werte
     N = 1000
     bevoelkerung_init = 200  
     loehne_init = 50 

     # Parameter
     lohn_growth <- input$lohn_growth
     interaktionsterm_1 <- -input$interaktionsterm_1/1000
     interaktionsterm_2 <- input$interaktionsterm_2/2000
     todesterm <- -input$todesterm/10
     
     # Dynamik
     bevoelkerung_list = c(bevoelkerung_init, rep(NA, N))
     loehne_list = c(loehne_init, rep(NA, N))
     
     for (i in 2:N){
       bevoelkerung_list[i] <- bevoelkerung_list[i-1] + 
         interaktionsterm_2*loehne_list[i-1]*bevoelkerung_list[i-1] + 
         bevoelkerung_list[i-1]*todesterm
       loehne_list[i] <- (1+lohn_growth)*loehne_list[i-1] + 
         interaktionsterm_1*loehne_list[i-1]*bevoelkerung_list[i] + 
         loehne_list[i-1]*todesterm
     }
     
     pp_data <- tibble::tibble(
       Bevölkerung=bevoelkerung_list,
       Löhne=loehne_list,
       Zeit=1:(N+1)
     ) %>%
       pivot_longer(cols = -Zeit, names_to = "Art", 
                    values_to = "Wert")
      # Visualisierung
     ggplot(pp_data, aes(x=Zeit, y=Wert, color=Art)) +
       geom_line(key_glyph = draw_key_rect) + 
       scale_color_viridis_d(begin = 0, end = 0.7) + 
       ggtitle("Lohn- und Bevölkerungsdynamiken") + theme_bw()  +
       theme(legend.text = element_text(size=14), 
             axis.text = element_text(size=12), 
             axis.title = element_text(size=13), 
             legend.position = "bottom", 
             legend.title = element_blank(), 
             plot.title = element_text(size=16), 
             axis.line = element_line(), 
             panel.border = element_blank())
   })
}

shinyApp(ui = ui, server = server)
