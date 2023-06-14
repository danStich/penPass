# Define server logic required to run penPass and display result ----
server <- function(input, output) {

  # Library loads ----
  library(dia)
  library(foreach)
  library(DT)
  library(dplyr)
  library(ggplot2)  
  
  # Data read and default parameter load ----
  # . Default fish passage ----
  passage <- read.csv("data/passage.csv")
  r_passage <- shiny::reactiveValues(data = passage)
  
  output$passage <- DT::renderDT({
      DT::datatable(r_passage$data, editable = TRUE,
                    options = list(pageLength = 15, lengthChange = FALSE,
                                   bInfo = FALSE, searching = FALSE,
                                   ordering = FALSE, paging = FALSE))
  })
  
  shiny::observeEvent(input$passage_cell_edit, {
      #get values
      info = input$passage_cell_edit
      i = as.numeric(info$row)
      j = as.numeric(info$col)
      k = as.numeric(info$value)

      #write values to reactive
      r_passage$data[i, j] <- k
  })  
  
  # . Default life-history parameters ----
  lifehistory <- read.csv("data/lifehistory.csv")
  r_lifehistory <- shiny::reactiveValues(data = lifehistory)
  
  output$lifehistory <- DT::renderDT({
      DT::datatable(r_lifehistory$data, editable = TRUE,
                    options = list(pageLength = 15, lengthChange = FALSE,
                                   bInfo = FALSE, searching = FALSE,
                                   ordering = FALSE, paging = FALSE))
  })
  
  shiny::observeEvent(input$lifehistory_cell_edit, {
      #get values
      info = input$lifehistory_cell_edit
      i = as.numeric(info$row)
      j = as.numeric(info$col)
      k = as.numeric(info$value)

      #write values to reactive
      r_lifehistory$data[i, j] <- k
  })
  
  # penPass model run ----

  model_run <- shiny::reactive({
      shiny::req(input$go)   
    
      out_list <- vector(mode = "list", length = input$n_runs)
      outer_outlist <- vector(mode = "list", length = length(unique(input$year)))
    
      for(t in 1:length(unique(input$year))){
        
      year = unique(input$year)[t]
      # year = sample(input$year[1:length(input$year)], 1)

      for(i in 1:input$n_runs){
        
      out_list[[i]] <- 
      data.frame( 
      year = year,  
      x = 
      penPass::run_one_year(
      year = year,
      downstream = list(
        seeboomook = r_passage$data[1, 2],
        ripogenus = r_passage$data[2, 2],
        north_twin = r_passage$data[3, 2],
        quakish = r_passage$data[4, 2],
        dolby = r_passage$data[5, 2],
        east_millinocket = r_passage$data[6, 2],
        medway = r_passage$data[7, 2],
        matagamon = r_passage$data[8, 2],
        guilford = r_passage$data[9, 2],
        moosehead = r_passage$data[10, 2],
        browns_mills = r_passage$data[11, 2],
        howland = r_passage$data[12, 2],
        mattaseunk = r_passage$data[13, 2],
        west_enfield = r_passage$data[14, 2],
        milford = r_passage$data[15, 2],
        great_works = r_passage$data[16, 2],
        gilman_falls = r_passage$data[17, 2],
        stillwater = r_passage$data[18, 2],
        orono = r_passage$data[19, 2],
        veazie = r_passage$data[20, 2],
        bangor_waterworks = r_passage$data[21, 2]
      ),
      km_surv = r_lifehistory$data[1, 2],
      prod = c(
        WPN_prod = r_lifehistory$data[2, 2], 
        EPN_prod = r_lifehistory$data[3, 2], 
        Matt_prod = r_lifehistory$data[4, 2], 
        PISC_prod = r_lifehistory$data[5, 2],
        PN_prod = r_lifehistory$data[6, 2]),
      sat = c(
        WPN_sat = r_lifehistory$data[7, 2], 
        EPN_sat = r_lifehistory$data[8, 2], 
        Matt_sat = r_lifehistory$data[9, 2], 
        PISC_sat = r_lifehistory$data[10, 2],
        PN_sat = r_lifehistory$data[11, 2]),
      p_stillwater = r_lifehistory$data[12, 2]))
      }
      
      outer_outlist[[t]] <- do.call(rbind, out_list)
      
      }
  
      do.call(rbind, outer_outlist)
  
      })
  
  output$model_result <- renderDT(server = FALSE,
    model_run() |>
       data.frame() |>
      group_by(year) |>
      summarize(
      Median = round(median(x), 0),
      Lower = round(quantile(x, .05), 0),
      Upper = round(quantile(x, 0.95), 0)),
    rownames = FALSE,
    colnames = c("Year", "Median", "Lower 90%", "Upper 90%"),
    extensions = 'Buttons',
    options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), 
                   exportOptions = list(modifier = list(page = "all")))
  )
  

  output$graph <- shiny::renderPlot({

    mydata <- model_run()
    
    plotter <- mydata |> 
      data.frame() |>
      group_by(year) |>
      summarize(Median = round(median(x), 0),
                Lower = round(quantile(x, .05), 0),
                Upper = round(quantile(x, 0.95), 0)
                )

    names(plotter)[1] <- "Year"
    plotter$Year <- as.numeric(plotter$Year)

    # Make plot
    p <- ggplot2::ggplot(plotter,
                         ggplot2::aes(x =  Year, y = Median)) +
         ggplot2::geom_point(pch = 15, size = 2) +
         ggplot2::geom_segment(aes(xend = Year, y = Lower, yend = Upper)) +
         ggplot2::theme_classic() +
         scale_y_continuous(breaks = seq(0, 1e6, 2e5), 
                            labels = seq(0, 1000, 200),
                            limits = c(0, 1e6)
                            ) +
         ylab("Number of smolts (thousands)") +
         scale_x_continuous(breaks = seq(1970, 2022, 10),
                            labels = seq(1970, 2022, 10),
                            limits = c(1970, 2022)
                            ) +
         theme(
           axis.title.x = element_text(vjust = -1, size = 12),
           axis.title.y = element_text(vjust = 3, size = 12),
           axis.text = element_text(size = 11)
         )

    print(p)
      
  })
  
}







