library(munsell)
library(colorspace)
library(scales)
library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

# 
age_calc2 = function(dob, dot){
  
  dob_day = as.numeric(strftime(dob, "%d"))
  dob_month = as.numeric(strftime(dob, "%m"))
  dob_year = as.numeric(strftime(dob, "%Y"))
  
  dot_day = as.numeric(strftime(dot, "%d"))
  dot_month = as.numeric(strftime(dot, "%m"))
  dot_year = as.numeric(strftime(dot, "%Y"))
  
  years_diff = dot_year - dob_year
  months_diff = dot_month - dob_month
  days_diff = dot_day - dob_day
  
  total_months_diff = (years_diff*12) + (months_diff) + (days_diff/31)
  
  return(total_months_diff)
}

shinyApp(
  
  ui <- fluidPage(#theme = "flatly.css",
    
    # Instructions page ----
    navbarPage("📈 Mimo Norms",
               
               
               # Extra... nav bar ----
               tabPanel("Norms",
                        
                        sidebarPanel(
                          h4("🔧 Graph tools"),
                          selectInput(inputId = "collection3",
                                      label = "Collection",
                                      choices = c("PLEASE CHOOSE...",
                                                  "Eng-NA", "Eng-UK", "KIDEVAL",
                                                  "Spanish", "French", "German", "Japanese", "EastAsian",
                                                  "Clinical-MOR", "Biling"),
                                      selected = NULL
                          ),
                          
                          selectInput(inputId = "variable3",
                                      label = "Variable",
                                      choices =  c(
                                        "MLU in morphemes" = "mlu_m",
                                        "MLU in words" = "mlu_w",
                                        "HDD" = "hdd",
                                        "TTR" = "ttr"),
                                      selected = "mlu_m"
                          ),
                          
                          numericInput(inputId = "num_utts3",
                                       label = "Min. utterances",
                                       value = 100
                          ),
                          
                          numericInput(inputId = "bin_width3",
                                       label = "Bin width",
                                       value = 10
                          ),
                          
                          sliderInput(inputId = "shading3",
                                      label = "Shading",
                                      min = 0,
                                      max = 0.3,
                                      value = 0.1
                          ),
                          
                          sliderInput(inputId = "trim_data3",
                                      label = "Trim data?",
                                      min = 0,
                                      max = 100,
                                      value = c(1,100)
                          ),
                          
                          hr(),
                          
                          h4("👶🏽 Show speaker"),
                          textInput(inputId = "child_name",
                                    label = "Name of Child",
                                    placeholder = "Child Name"),
                          # Copy the line below to make a date selector
                          textInput(inputId = "value",
                                    label = "Value for MLU/HDD etc",
                                    placeholder = "Value"),
                          dateInput("dob3", label = h5("Date of Birth")),
                          dateInput("dot3", label = h5("Date of Test")),
                          htmlOutput("age")
                        ), # sidebarpanel
                        
                        mainPanel(
                          h5("Table will take a few seconds to appear/refresh..."),
                          plotOutput("DIY_plot",
                                     dblclick = "plot_dblclick3",
                                     brush = brushOpts(
                                       id = "plot_brush3",
                                       resetOnNew = TRUE
                                     ))
                        )
                        
               ), # end of navbar menu
               
               tabPanel("🔎 App", value = "app-link"),
               tabPanel("📈 Docs", value = "docs-link"),
               
               # Make that tab open your URL instead of switching panels
               tags$head(
                 tags$style(HTML("a[data-value='app-link'] { cursor: pointer; }")),
                 tags$script(HTML("
                     $(document).on('click', 'a[data-value=\"app-link\"]', function(e) {
                     e.preventDefault();
                     window.open('http://mimolanguageanalysis.uk', '_blank');
                     });"
                 ))
               ),
               
               tags$head(
                 tags$style(HTML("a[data-value='docs-link'] { cursor: pointer; }")),
                 tags$script(HTML("
                     $(document).on('click', 'a[data-value=\"docs-link\"]', function(e) {
                     e.preventDefault();
                     window.open('http://docs.mimolanguageanalysis.uk', '_blank');
                     });"
                 ))
               )
               
    ) # end of nav bar page
  ),
  
  
  # server statement----
  server <- function(input, output, session){
    
    
    # df_childes_DIY ----
    
    df_childes_DIY <- reactive({
      
      if(input$collection3 == "PLEASE CHOOSE...")return(NULL)
      
      if(input$collection3 == "KIDEVAL"){
        
        kideval_corpora_id <- c(65, #Bates
                                60, #Bernstein
                                71, #Bliss
                                76, #Bloom70
                                41, #Bloom73
                                73, #Braunwald
                                36, #Brown
                                29, #Clark
                                39, #Demetras1
                                46, #Demetras2
                                50, #Feldman
                                43, #Gathercole
                                64, #Gleason
                                57, #Hall
                                31, #Higginson
                                48, #HSLLD
                                54, #MacWhinney
                                47, #McCune
                                30, #NewEngland
                                63, #Post
                                49, #Providence
                                55, #Sachs
                                61, #Snow
                                67, #Supes
                                66, #Tardif
                                62, #Valian
                                52, #VanHouten
                                32, #VanKleeck
                                56, #Warren
                                69) #Weist
        
        
        data_url <- "https://raw.githubusercontent.com/nickriches/CHILDES_speaker_statistics/refs/heads/main/speaker_statistics.csv"
        data_path <- "speaker_statistics.csv"
        download.file(data_url, data_path)
        df <- read.csv(data_path)
        
        
        df <- df[which(df$corpus_id %in% kideval_corpora_id), ]
        
      }
      
      if(input$collection3 != "KIDEVAL"){
        
        data_url <- "https://raw.githubusercontent.com/nickriches/CHILDES_speaker_statistics/refs/heads/main/speaker_statistics.csv"
        data_path <- "speaker_statistics.csv"
        download.file(data_url, data_path)
        df <- read.csv(data_path)
        
        df %>% filter(collection_name == input$collection3) -> df
        
      }
      
      df <- as.data.frame(df)
      
      df %>% filter(num_utterances >= input$num_utts3) -> df
      
      df %>% arrange(target_child_age) %>% filter(is.na(target_child_age) == FALSE) -> df
      
      age_range <- max(df$target_child_age) - min(df$target_child_age)
      
      upper_age_bound <- min(df$target_child_age) + age_range*(input$trim_data3[2]/100)
      lower_age_bound <- min(df$target_child_age) + age_range*(input$trim_data3[1]/100)
      
      df %>%
        filter(target_child_age >= lower_age_bound) %>%
        filter(target_child_age <= upper_age_bound) ->
        df
      
      
      if(input$variable3 == "mlu_m"){
        df$dv <- df$mlu_m
      }
      
      if(input$variable3 == "mlu_w"){
        df$dv <- df$mlu_w
      }
      
      if(input$variable3 == "hdd"){
        df$hdd <- df$hdd*42 # to obtain ACTUAL HDD
        df$dv <- df$hdd
      }
      
      if(input$variable3 == "ttr"){
        df$ttr <- df$num_types/df$num_tokens
        df$dv <- df$ttr
      }
      
      df %>% filter(dv != 0) -> df
      
      df %>% filter(is.na(dv) == FALSE) -> df
      df$mean <- rollapply(df$dv, mean, width = input$bin_width3, partial = TRUE)
      df$sd <- rollapply(df$dv, sd, width = input$bin_width3, partial = TRUE)
      df$plus_one <- df$mean + df$sd
      df$plus_one_point_five <- df$mean + 1.5*df$sd
      df$minus_one <- df$mean - df$sd
      df$minus_one_point_five <- df$mean - 1.5*df$sd
      
      return(df) 
      
    })
    
    
    # xmax_hdd (obtain highest value on x axis)----
    xmax_DIY <- reactive({
      return(max(df_childes_DIY()$target_child_age, na.rm = TRUE))
    })
    
    
    # Speaker_age----
    speaker_age <- reactive({
      age <- age_calc2(input$dob3, input$dot3)
      return(age)
    })
    
    
    # ranges and observeEvent for interactive plots ----
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    
    observeEvent(input$plot_dblclick3, {
      brush <- input$plot_brush3
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
    
    # DIY_plot -----
    output$DIY_plot <- renderPlot({
      
      req(df_childes_DIY())
      
      m2ym <- function(age_m){
        year <- floor(age_m/12)
        month <- floor(age_m - (year*12))
        return(paste0(year, ";", month))
      }
      
      breakpoints <- function(min,max){
        seq <- seq(min, max, 1)
        seq <- unique(floor(seq/3))
        seq <- seq*3
        return(seq)
      }
      
      g <- ggplot()
      
      g <- g + theme_bw()
      
      g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      
      g <- g + geom_point(data = df_childes_DIY(), alpha = input$shading3, aes(x = target_child_age, y = dv, size = num_utterances))
      
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = mean), linetype = "solid", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = plus_one), linetype = "dashed", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = plus_one_point_five), linetype = "dotted", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = minus_one), linetype = "dashed", lwd = 1, se = FALSE, method = "loess")
      g <- g + geom_smooth(data = df_childes_DIY(), aes(x = target_child_age, y = minus_one_point_five), linetype = "dotted", lwd = 1, se = FALSE, method = "loess")
      
      mean_model <- loess(mean ~ target_child_age, data = df_childes_DIY())
      plus_one_model <- loess(plus_one ~ target_child_age, data = df_childes_DIY())
      minus_one_model <- loess(minus_one ~ target_child_age, data = df_childes_DIY())
      
      g <- g + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
      
      g <- g + scale_x_continuous(breaks = breakpoints(0, xmax_DIY()),         # use these breaks...
                                  labels = m2ym(breakpoints(0, xmax_DIY()))    # ...with these labels
      ) 
      
      dv_name <- case_when(input$variable3 == "mlu_m" ~ "MLU in morphemes",
                           input$variable3 == "mlu_w" ~ "MLU in words",
                           input$variable3 == "ttr" ~ "Type Token Ratio",
                           input$variable3 == "hdd" ~ "HDD")
      
      g <- g + labs(title = paste(dv_name, "for CHILDES collection", input$collection3),
                    x = "Age (Months;Years)", y = dv_name)
      
      # Activate this routine if speaker has been selected, and mlum has been chosen as input$variable
      
      
      g_subtitle <- "Blue lines show mean, 1 st.dev, and 1.5 st.dev"
      
      if(input$child_name != "" &
         input$value != "" &
         is.na(predict(mean_model, speaker_age())) == FALSE)
        
      {
        
        mean_for_speaker_age <- predict(mean_model, speaker_age())
        sd_for_speaker_age <- predict(plus_one_model, speaker_age()) - mean_for_speaker_age
        z_score <- round((as.numeric(input$value) - mean_for_speaker_age)/sd_for_speaker_age, 2)
        perc <- round(pnorm(z_score)*100,0)
        g <- g + geom_point(data = df_childes_DIY(), aes(x = speaker_age(), y = as.numeric(input$value), pch = 9, size = 500, colour = "red")) + scale_shape_identity()
        zp_label <- paste0("z = ", as.character(z_score), ", perc =", perc)
        g_subtitle <- paste0(g_subtitle, "\nMarker shows participant ", input$child_name, ",", zp_label)
      }
      
      g <- g + labs(subtitle = g_subtitle)
      
      g
      
    }) # end of output$all plot <- renderPlot...
    
    output$age = renderUI({
      
      m2ym <- function(age_m){
        year <- floor(age_m/12)
        month <- floor(age_m - (year*12))
        return(paste0(year, ";", month))
      }
      
      age <- paste("Age = ", as.character(m2ym(speaker_age())))
      
    })
    
  }
  
)

shinyApp(ui = ui, server = server)



