library("shiny")
library("plotly")
library("ggplot2")
# graphics.off()
# par("mar")
# par(mar=c(1,1,1,1))
# packageVersion('plotly')
# Constants

ls <- 299792458 
t  <- seq(1,10,0.1)
y  <- t*t
p  <- xy.coords(t,y)

#-------------------- UI------------------------

ui <- fluidPage(
  tags$head(tags$link(rel="myicon", href="https://github.com/Bhanuchander210/Bhanuchander210.github.io/blob/master/images/b.jpg")),
  titlePanel("Patch Antenna Design"),
    fluidRow(
      column(3,
      h3("Resonant Frequeuncy"),
      numericInput("freq", label="Enter the Frequency in GHz", value="",min=1.000,max=100.000, step = 0.001)),
      column(3,
      h3("Dielectric Constant"),
      numericInput("diel", label="Enter Relative Permittivity", value="",min=1.000,max=100.000, step = 0.001)
    ),
    column(3,
           h3("Height of patch"),
           numericInput("hi", label="Enter Height in mm", value="",min=0.000,max=1000.000, step = 0.001)
    )
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Dimensions",
               verbatimTextOutput("abswidth"),
               verbatimTextOutput("abslength"),
               verbatimTextOutput("feedl"),
               verbatimTextOutput("feedw"),
               verbatimTextOutput("glength"),
               verbatimTextOutput("gwidth"),
               h5(tags$b("Note :")),
               code("If you find any negative dimenstion means you need to adjust the inputs .
                    Beacause here rendered diemensions are derived from antenna design formulae only for the condition W/H > 1.")
               ),
      tabPanel("Parameters",    verbatimTextOutput("efflength"),
               verbatimTextOutput("effdiel"),
               verbatimTextOutput("elength")),
      tabPanel("Patch 2D",
               h5(tags$b("Note :")),
               code(" This plot shows the relative view of the patch dimensions.
                      1 Unit is equals to the length of patch."),
               verbatimTextOutput("unitview"),
               plotOutput("patchshape")),
      
      tabPanel("Patch 3D",
        
               h5(tags$b("Note :")),
               code("Here Shown 3D Structure for your understanding of the patch structure has to conducting planes one side
                    must be trimmed to the calculated size now then another side is the ground plane. Here Inset Feeder line 
                    not shown with this figure. Still processing to provide better view... Stay tuned will update soon.. :)"),
               h5(tags$b("Cavity :")),
               br(),
               br(),
               plotlyOutput("threedplot")
               )
    )
    ),
  absolutePanel(
    a(img(src='square_icon.png', align = "right-top"),href="https://bhanuchander210.github.io")
    , id = "input_date_control", class = "panel panel-default", fixed = TRUE
    , draggable = FALSE, top = 'auto', left = 'auto', right = 0, bottom = 0
    , width = '5%', height = 'auto'
  ) 
  

  )
  

# -----------------server------------
server <- function(input, output, session) {
  
  wavelength <- reactive({
    
    f <- input$freq * 10^9
    
    (ls/f)
    
  })
  
  elength <- reactive({
    
    wl <- wavelength()
    
    fl <- getfeedl()
    
    (360*fl)/wl
    
  })


  patchwidth <- reactive({
    
    f <- input$freq * 10^9
    
    d <- input$diel 
    
    (ls/(2*f))*(sqrt(2/(d+1)))
    
  })
  
  effd <- reactive({
    
    f  <- input$freq * 10^9
    
    d  <- input$diel 
    
    pw <- patchwidth()
    
    h  <- input$hi * 10^-3
    
    ((d+1)/2)*((d-1)/2)*((1+((12*h)/pw))^(-0.5))
    
  })
  
  effl <- reactive({
    
    f  <- input$freq * 10^9
    
    d  <- effd() 
    
    (ls/(2*f))*(sqrt(1/d))
    
  })
  
  dell <- reactive({
    
    d <- effd() 
    
    h <- input$hi * 10^-3
    
    w <- patchwidth()
    
    r <- w/h
    
    (0.412*h)*((d+0.3)/(d-0.264))*((r+0.258)/(r+0.8))
    
  })
  
  getfeedl <- reactive({
    
    f <- input$freq * 10^9
    
    d <- effd()
    
    l <- effl()
    
    (ls/(4*f))*(sqrt(1/(d)))
    
  })
  
  getfeedw <- reactive({
    
    w <- patchwidth()
    
    (2*w)/5
    
  })
  
  
  patchlength <- reactive({
    
    dl <- dell()
    el <- effl()
    el-(2*dl)
    
  })
  
  glength <- reactive({
    
    h <- input$hi * 10^-3
    
    l <- patchlength()
    
    (l+(6*h))
    
  })
  
  gwidth <- reactive({
    
    h <- input$hi  * 10^-3
    
    w <- patchwidth()
    
    (w+(6*h))
    
  })
  
  patchs <- reactive({
    
    fact<-1/patchlength()
    
    w <- (patchwidth()*fact)
    
    # plot(c(-2,2), c(-2, 2), type= "n", xlab = "width (u)", ylab = "length (u)")
    # 
    # rect((-w/2),0,(w/2),1, density = 10, border = "red")
    # 
    # rect((-getfeedw()*fact/2), 0, (getfeedw()*fact/2), -(getfeedl()*fact), density=10, border = 'red')

      d=data.frame(x1=c((-w/2),(-getfeedw()*fact/2)), y1=c(0,0),x2=c((w/2),(getfeedw()*fact/2)), y2=c(1,-getfeedl()*fact),t=c('patch','feeder'), r=c(10,20))
      ggplot() +
      scale_x_continuous(name="x") +
      scale_y_continuous(name="y") +
      geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.05)
    #geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4)
    #opts(title="geom_rect", plot.title=theme_text(size=40, vjust=1.5))
    
  })
  
  output$abswidth <- renderText({  paste ("Patch Width                   : ",patchwidth()," m")})
  output$abslength <- renderText({ paste ("Patch Length                  : ",patchlength()," m")})
  output$feedl <- renderText({     paste ("Feeder Length                 : ",getfeedl()," m")})
  output$feedw <- renderText({     paste ("Feeder Width                  : ",getfeedw()," m")})
  output$efflength <- renderText({ paste ("Effective Length              : ",effl()," m")})
  output$effdiel <- renderText({   paste ("Effective Dielectric Constant : ",effd())})
  output$elength <- renderText({   paste ("Electrical Length             : ",elength())})
  output$glength <- renderText({   paste ("Ground Plane Length should be > ",glength())})
  output$gwidth <- renderText({    paste ("Ground Plane Width should be  > ",gwidth())})
  output$threedplot <- renderPlotly({p})
  output$twodplot <- renderPlotly({twod})
  output$plotsine <- renderPlot(plot(p,type='1', xlab="time", ylab="Sine wave"))
  output$unitview <- renderText({    paste ("1 Unit = ",patchlength()," m")})
  output$patchshape <- renderPlot({patchs()})
  
  
#---------------------Twod-------------------------------------------
  # twod <- plot_ly(economics, x = ~date, y = ~uempmed, name = "unemployment")
  # twod <- layout(twod, title = 'Highlighting with Rectangles',
  #             shapes = list(
  #               list(type = "rect",
  #                    fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
  #                    x0 = "1980-01-01", x1 = "1985-01-01", xref = "x",
  #                    y0 = 4, y1 = 12.5, yref = "y"),
  #               list(type = "rect",
  #                    fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
  #                    x0 = "2000-01-01", x1 = "2005-01-01", xref = "x",
  #                    y0 = 4, y1 = 12.5, yref = "y")))
  
#--------------------- Three d Plot------------------------------------- 
  
  z <- c(
    c(0,0,0,0),
    c(0,0,0,0),
    c(0,0,0,0),
    c(0,0,0,0)
  )
  
  y <- c(
    c(0,0),
    c(0,0)
  )
  
  dim(y) <- c(2,2)
  
  dim(z) <- c(4,4)
  
  z2     <- z + 1
  
  patchheight <- reactive({ input$hi})
  
  p <- plot_ly(showscale = TRUE) %>%
       add_surface(z = ~z) %>%
       add_surface(z = ~z2) %>%
       add_surface(z= ~y)
  
}

#-------------------Run-----------------
shinyApp(ui = ui, server = server)