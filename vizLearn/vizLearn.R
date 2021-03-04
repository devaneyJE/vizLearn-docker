library(tidyverse)
library(readxl)
library(haven)
library(datasets)
library(shinythemes)
library(reactable)

options(shiny.usecairo = F)

server <- function(input, output) {
    
    #--------------------------------------------------------data
    
    #-----------------------read/view
    
    #selecting file
    output$file.type <- renderUI({
      req(input$data.source == "Import Dataset")
      selectInput("file.type", "File Type",
                  choices = c("csv", "tsv","Excel(xls/xlsx)", "SAS", "SPSS(sav)", "Stata(dta)"))
    })
    
    output$file.select <- renderUI({
      req(input$file.type)
      fileInput("table.data", "",
            multiple = FALSE
      )
    })
    
    #reactive dataframe (manipulation, analysis/plot prep)
    initial.data <- reactive({
      req(input$data.source != "Choose Source")
      if(input$data.source == "mtcars"){
        data("mtcars")
        mtcars
      }
      else if(input$data.source == "iris"){
        data("iris")
        iris
      }
      else if(input$data.source == "attitude"){
        data("attitude")
        attitude
      }
      else if(input$data.source == "airquality"){
        data("airquality")
        airquality
      }
      else if(input$data.source == "freeny"){
        data("freeny")
        freeny
      }
      else if(input$data.source == "USArrests"){
        data("USArrests")
        USArrests
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "csv"){
        read.csv(input$table.data$datapath,
                 header = input$header,
                 stringsAsFactors = F)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "tsv"){
        read_tsv(input$table.data$datapath,
                 header = input$header)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "Excel(xls/xlsx)"){
        as.data.frame(read_excel(input$table.data$datapath,
                  col_names = input$header))
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "SAS"){
        read_sas(input$table.data$datapath)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "SPSS(sav)"){
        read_sav(input$table.data$datapath)
      }
      else if(input$data.source == "Import Dataset" && input$file.type == "Stata(dta)"){
        read_dta(input$table.data$datapath)
      }
    })
    
    #----------------------manipulation
    
    output$tr.fun_select <- renderUI({
      req(input$data.source != "Choose Source")
        checkboxInput("tr.fun",
                    "Filter by Value"
        )
    })

    #filter
    output$tr.filter <- renderUI({
        req(input$tr.fun == T)
        fluidRow(
            column(width = 4,
                selectInput("filter.var", "Variable",
                            choices = c(names(initial.data()))
                )
            ),
            column(width = 4,
                selectInput("filter.operator", "Operator",
                            choices = c("==", ">", ">=", "<", "<=") 
                )
            ),
            column(width = 4,
                numericInput("filter.value", "Value",
                            value = 0)
                )
        )
    })

    filter_reac <- reactive({
      data.filter <- initial.data()
        if(is.null(input$tr.fun)){
          data.filter
        }
        else if(input$tr.fun != "Filter Rows by Value"){
          data.filter
        }
        else if(input$filter.operator == "=="){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))==input$filter.value)
        }
        else if(input$filter.operator == ">"){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))>input$filter.value)
        }
        else if(input$filter.operator == ">="){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))>=input$filter.value)
        }
        else if(input$filter.operator == "<"){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))<input$filter.value)
        }
        else if(input$filter.operator == "<="){
          data.filter <- data.filter %>% filter((!!as.name(input$filter.var))<=input$filter.value)
        }
        data.filter
    })

    #modified data object
    plot.data <- reactive({
      filter_reac()
      })
    
    #data characteristics output
    output$str.info <- renderPrint(
      str(plot.data())
    )
    
    #table view of imported data
    output$data.view <- renderTable({ 
      if(input$disp == "head") {
        return(head(plot.data()))
      }
      
      else {
        return(plot.data())
      }
    })
    
    output$reac_table <- renderReactable({
      if(input$disp == "head") {
        reactable(head(plot.data()), filterable = T, pagination = F, showSortable = T, showSortIcon = T, striped = T, highlight = T)
      }
      
      else {
        reactable(plot.data(),  filterable = T, pagination = F, showSortable = T, showSortIcon = T, striped = T, highlight = T)
      }
    })
    
    #----------------------------------------------------plotting  
    #var select
    output$single_var_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxInput("single.var.check",
                      "Single Variable",
                      value = F)
    })
    
    output$y_select <- renderUI({
        req(input$data.source != "Choose Source")
        if(input$single.var.check == F){
            selectInput("yvar",
                        label = "Y Variable",
                        choices = c(names(plot.data()))
            )
        }
        else{
            selectInput("single.var",
                        label = "Variable",
                        choices = c(names(plot.data()))
            )
        }
    })
    
    output$x_select <- renderUI({
        req(input$single.var.check == F)
        selectInput("xvar",
                    label = "X Variable",
                    choices = c(names(plot.data()))
        )
    })
    
    #reactive x and y vars
    y_var <- reactive({
        if(input$single.var.check == F){
            input$yvar
        }
        else{
            input$single.var
        }
    })
    
    x_var <- reactive({
        input$xvar
    })
    
    #geom select
    geom_list <- reactive({
      if(input$single.var.check == T){
        c("Choose Geometry", "Density", "Dotplot", "Histogram", "Bar", "Boxplot")
      }else{
        c("Choose Geometry", "Point", "Rug", "Smooth", "Violin", "Area", "Line", "Step")
      }
    })
    
    output$geom_select <- renderUI({
        req(input$data.source != "Choose Source")
        selectInput("geom",
                    label = "Geometry Type",
                    choices = geom_list(),
                    multiple = F
        )
    })
    
    #theme
    output$theme_select <- renderUI({
        req(input$data.source != "Choose Source")
        selectInput("theme",
                    label = "Theme",
                    choices = c("default", "theme_bw", "theme_classic", "theme_grey", "theme_minimal", "theme_light", "theme_dark")
        )
    })
    
    theme_reac <- reactive({
      if(input$theme == "default"){
        NULL
      }
      else if(input$theme == "theme_bw"){
        theme_bw()
      }
      else if(input$theme == "theme_classic"){
        theme_classic()
      }
      else if(input$theme == "theme_grey"){
        theme_grey()
      }
      else if(input$theme == "theme_minimal"){
        theme_minimal()
      }
      else if(input$theme == "theme_light"){
        theme_light()
      }
      else if(input$theme == "theme_dark"){
        theme_dark()
      }
    })
    
    #faceting
    output$facet_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxInput("facet.check",
                      "Use Faceting",
                      value = F
        )
    })
    
    output$facet_select <- renderUI({
        req(input$facet.check == T)
        selectInput("facet",
                    label = "Faceting Type",
                    choices = c("None", "Wrap", "Columns", "Rows", "Grid")
        )
    })
    
    output$facet_var1 <- renderUI({
        req(input$facet.check == T)
        req(input$facet == "Rows" || input$facet == "Grid")
        selectInput("facet.var1",
                    label = "Row Facet Variable",
                    choices = c("None", names(plot.data()))
        )
    })
    
    output$facet_var2 <- renderUI({
        req(input$facet.check == T)
        req(input$facet == "Columns" || input$facet == "Wrap" || input$facet == "Grid")
        selectInput("facet.var2",
                    label =  
                        if(input$facet == "Columns" || input$facet == "Grid"){
                            "Column Facet Variable"
                        }
                        else if(input$facet == "Wrap"){
                            "Wrapping Facet Variable"
                        }
                    ,
                    choices = c("None", names(plot.data()))
        )
    })
    
    #reactive faceting
    facet_reac <- reactive({
        if(input$facet.check == F){
          NULL
        }
        else if(input$facet == "None"){
          NULL
        }
        else if(input$facet == "Wrap"){
            req(input$facet.var2 != "None")
            facet_wrap(as.formula(paste("~", input$facet.var2)))
        }
        else if(input$facet == "Columns"){
            req(input$facet.var2 != "None")
            facet_grid(as.formula(paste("~", input$facet.var2)))
        }
        else if(input$facet == "Rows"){
            req(input$facet.var1 != "None")
            facet_grid(as.formula(paste(input$facet.var1, "~", ".")))
        }
        else if(input$facet == "Grid"){
            req(input$facet.var1 != "None" && input$facet.var2 != "None")
            facet_grid(as.formula(paste(input$facet.var1, "~", input$facet.var2)))
        }
    })
    
    facet_string <- reactive({
        if(input$facet.check == F){
          NULL
        }
        else if(input$facet == "None"){
          NULL
        }
        else if(input$facet == "Wrap"){
            req(input$facet.var2 != "None")
            paste("facet_wrap(~", input$facet.var2, ")", sep = "")
        }
        else if(input$facet == "Columns"){
            req(input$facet.var2 != "None")
            paste("facet_grid(~", input$facet.var2, ")", sep = "")
        }
        else if(input$facet == "Rows"){
            req(input$facet.var1 != "None")
            paste("facet_grid(~", input$facet.var1, ")", sep = "")
        }
        else if(input$facet == "Grid"){
            req(input$facet.var1 != "None" && input$facet.var2 != "None")
            paste("facet_grid(", input$facet.var1, "~", input$facet.var2, ")", sep = "")
        }
    })
    
    #--------------------------labels
    #title
    output$lab_check <- renderUI({
        req(input$data.source != "Choose Source")
        checkboxGroupInput("lab.check", "",
                      choices = c("Add Title", "Axis Labels"),
                      inline = T
        )
    })
    
    output$plot_title <- renderUI({
        req(input$lab.check == "Add Title")
        textInput("title",
            "Plot Title",
            "Title"
        )
    })
    
    #xlab
    output$x_label <- renderUI({
      req(input$lab.check == "Axis Labels")
        if(input$single.var.check == F){
          textInput("x_lab",
                    "X-axis Label",
                    value = input$xvar
          )
        }
        else if(input$single.var.check == T){
            textInput("var_lab",
                      "Variable Label",
                      value = input$single.var
            )
        }
    })
    
    #ylab
    output$y_label <- renderUI({
      req(input$lab.check == "Axis Labels")
        if(input$single.var.check == F){
            textInput("y_lab",
                      "Y-axis Label",
                      value = input$yvar
            )
        }
    })
    
    #reactive labels
    labs_reac <- reactive({
      if(input$single.var.check == T){
        lab_fun <- function(title = NULL, y = NULL){
          labs(title = title, y = y)
        }
        lab_args <- list(title = input$title, y = input$var_lab)
      }
      else if(input$single.var.check == F){
        lab_fun <- function(title = NULL, y = NULL, x = NULL){
          labs(title = title, y = y, x = x)
        }
        lab_args <- list(input$title, input$y_lab, input$x_lab)
      }
      do.call(lab_fun, lab_args)
    })
    
    #-----------------------aes and geom parameters
    aes_choice <- reactive({
      ac <- c("Colors", "Alpha")
      if(input$geom != "Rug" && input$geom != "Line" && input$geom != "Step"){
        ac <- c(ac, "Fill")
      }else{ac}
      if(input$geom != "Dotplot"){
        ac <- c(ac, "Size")
      }else{ac}
      if(input$geom != "Dotplot" && input$geom != "Point"){
        ac <- c(ac, "Linetype")
      }else{ac}
      if(input$geom == "Bar" || input$geom == "Point"){
        ac <- c(ac, "Shape")
      }else{ac}
    })
    
    #color
    output$aes_check <- renderUI({
        req(input$data.source != "Choose Source")
        radioButtons("aes.check", "",
                      choices = aes_choice(),
                      inline = T
        )
    })
    
    #color
    output$color_select <- renderUI({
        req(input$aes.check == "Colors")
        fluidRow(    
            column(width = 6,
                selectInput("color",
                            "Select Color",
                            choices = c("None","Black", "Orange", "Sky Blue", "Green", "Yellow", "Blue", "Red", "Pink"))
            )
        )
    })
    
    color_group_reac <- reactive({
        if(is.null(input$aes.check)){
           NULL
        }
        else if(input$aes.check != "Colors"){
            NULL
        }
        else if(input$color.group == "None"){
            NULL
        }
        else{
            input$color.group
        }
    })
    
    color_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Colors"){
        NULL
      }
      else if(input$color == "None"){
        NULL
      }
      else if(input$color == "Black"){
        "#000000"
      }
      else if(input$color == "Orange"){
        "orange"
      }
      else if(input$color == "Sky Blue"){
        "#56B4E9"
      }
      else if(input$color == "Green"){
        "green"
      }
      else if(input$color == "Yellow"){
        "yellow"
      }
      else if(input$color == "Blue"){
        "blue"
      }
      else if(input$color == "Red"){
        "red"
      }
      else if(input$color == "Pink"){
        "#CC79A7"
      }
    })
    
    #fill
    output$fill_select <- renderUI({
        req(input$aes.check == "Fill" && input$geom != "Rug", input$geom != "Line", input$geom != "Step")
        selectInput("fill.select",
                    "Select Fill Color",
                    choices = c("None", "Black", "Red", "Blue", "Green", "Pink", "Orange", "Yellow")
        )
    })
    
    fill_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Fill"){
        NULL
      }
      else if(input$fill.select == "None"){
        NULL
      }
      else if(input$fill.select == "Black"){
        "black"
      }
      else if(input$fill.select == "Blue"){
        "blue"
      }
      else if(input$fill.select == "Green"){
        "green"
      }
      else if(input$fill.select == "Pink"){
        "pink"
      }
      else if(input$fill.select == "Orange"){
        "orange"
      }
      else if(input$fill.select == "Yellow"){
        "yellow"
      }
      else if(input$fill.select == "Red"){
        "red"
      }
    })
    
    #alpha
    output$alpha_adjust <- renderUI({
      req(input$aes.check == "Alpha")
          sliderInput("alpha.adj",
                      "Adjust Alpha",
                      min = 0.01, max = 1,
                      value = 0.7,
                      round = -2)  
    })
    
    alpha_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Alpha"){
        NULL
      }
      else if(input$aes.check == "Alpha"){
        input$alpha.adj
      }
    })
    
    #size
    output$size_adjust <- renderUI({
      req(input$aes.check == "Size")
      sliderInput("size.adj",
        "Adjust Size",
        min = 0.1, max = 20,
        value = 1,
        step = 0.5
      ) 
    })
    
    size_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Size"){
        NULL
      }
      else if(input$aes.check == "Size"){
        input$size.adj
      }
    })
    
    #linetype
    output$linetype_select <- renderUI({
        req(input$aes.check == "Linetype")
        selectInput("linetype",
                    "Select Linetype",
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))
    })
    
    linetype_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      }
      else if(input$aes.check != "Linetype"){
        NULL
      } 
      else if(input$linetype == "solid"){
        NULL
      }
      else if(input$linetype == "dashed"){
        "dashed"
      }
      else if(input$linetype == "dotted"){
        "dotted"
      }
      else if(input$linetype == "dotdash"){
        "dotdash"
      }
      else if(input$linetype == "longdash"){
        "longdash"
      }
      else if(input$linetype == "twodash"){
        "twodash"
      }
    })
    
    #shape
    output$shape_select <- renderUI({
        req(input$aes.check == "Shape")
        selectInput("shape.select",
                    "Select Shape",
                    choices = c("default", "circle", "square", "diamond", "triangle", "plus", "cross", "asterisk"))
    })
    
    shape_reac <- reactive({
      if(is.null(input$aes.check)){
        NULL
      } 
      else if(input$aes.check != "Shape"){
        NULL
      } 
      else if(input$shape.select == "default"){
        NULL
      }
      else if(input$shape.select == "circle"){
        1
      }
      else if(input$shape.select == "square"){
        15
      }
      else if(input$shape.select == "diamond"){
        18
      }
      else if(input$shape.select == "triangle"){
        17
      }
      else if(input$shape.select == "plus"){
        3
      }
      else if(input$shape.select == "cross"){
        4
      }
      else if(input$shape.select == "asterisk"){
        8
      }
    })
    
    #position adjustment
    position_string <- reactive({
      if(input$geom == "Bar"){
        c("None", "Dodge", "Fill", "Stack")
      }
      else if(input$geom == "Point"){
        c("None", "Jitter")
      }
    }) 
    
    output$position_adj <- renderUI({
      req(input$aes.check == "Position")
      selectInput("pos_adj",
                  label = "Position Adjustment",
                  choices = position_string()
      )
    })
    
    position_reac <- reactive({
      if(is.null(input$pos_adj)){
        NULL
      }
      else if(input$pos_adj == "None"){
        NULL
      }
      else if(input$pos_adj == "Jitter"){
        "jitter"
      }
      else if(input$pos_adj == "Dodge"){
        "dodge"
      }
      else if(input$pos_adj == "Fill"){
        "fill"
      }
      else if(input$pos_adj == "Stack"){
        "stack"
      }
    })
    
    #-----------------------plot code division
    p_aes <- reactive({
        if(input$single.var.check == T){
            p_aes_fun <- function(y){
                aes_string(y)
            }
            p_aes_args <- list(y_var())
        }
        else{
            p_aes_fun <- function(x, y){
                aes_string(x, y)
            }
            p_aes_args <- list(x_var(), y_var())
        }
        do.call(p_aes_fun, p_aes_args)
    })
    
    #geom args
    geom_args <- reactive({
      if(!is.null(color_reac())){
        al <- c(color = color_reac())
      }else{al <- NULL}
      if(!is.null(alpha_reac())){
        al <- c(al, alpha = alpha_reac())
      }else{al <- al}
      if(!is.null(fill_reac())){
        al <- c(al, fill = fill_reac())
      }else{al <- al}
      if(!is.null(size_reac())){
        al <- c(al, size = size_reac())
      }else{al <- al}
      if(!is.null(linetype_reac())){
        al <- c(al, linetype = linetype_reac())
      }else{al <- al}
      if(!is.null(position_reac())){
        al <- c(al, position = position_reac())
      }else{al <- al}
      if(!is.null(shape_reac())){
        al <- c(al, shape = shape_reac())
      }else{al <- al}
      as.list(al)
    })
    
    geom_fun_str <- reactive({
      as.list(c(color = color_reac(), alpha = alpha_reac(), fill = fill_reac(), size = size_reac(), linetype = linetype_reac(), position = position_reac(), shape = shape_reac()))
    })
    geom_fun_list <- reactive({
      as.list(c())
    })
    
    geom_reac <- reactive({
      geom_fun <- function(color = NULL, alpha = NULL, fill = NULL, size = NULL, linetype = NULL, position = NULL, shape = NULL){
          if(input$geom == "Choose Geometry"){
            NULL
          }
          else if(input$geom == "Density"){
            do.call(geom_density, geom_fun_str())
          }
          else if(input$geom == "Dotplot"){
            do.call(geom_dotplot, geom_fun_str())
          }
          else if(input$geom == "Histogram"){
            do.call(geom_histogram, geom_fun_str())
          }
          else if(input$geom == "Bar"){
            do.call(geom_bar, geom_fun_str())
          }
          else if(input$geom == "Point"){
            do.call(geom_point, geom_fun_str())
          }
          else if(input$geom == "Rug"){
            do.call(geom_rug, geom_fun_str())
          }
          else if(input$geom == "Smooth"){
            do.call(geom_smooth, geom_fun_str())
          }
          else if(input$geom == "Boxplot"){
            do.call(geom_boxplot, geom_fun_str())
          }
          else if(input$geom == "Violin"){
            do.call(geom_violin, geom_fun_str())
          }
          else if(input$geom == "Area"){
            do.call(geom_area, geom_fun_str())
          }
          else if(input$geom == "Line"){
            do.call(geom_line, geom_fun_str())
          }
          else if(input$geom == "Step"){
            do.call(geom_step, geom_fun_str())
          }
      }
      geom_fun(geom_args())
    })
    
    #plot component layering
    p <- reactive({
      p <- plot.data() %>% 
            ggplot(p_aes())
      if(!is.null(geom_reac())){
        pg <- p + (geom_reac())
      }else{pg <- p}
      if(!is.null(theme_reac())){
        pt <- pg + (theme_reac())
      }else{pt <- pg}
      if(!is.null(facet_reac())){
        pa <- pt + (facet_reac())
      }else{pa <- pt}
      if(!is.null(labs_reac())){
        pl <- pa + (labs_reac())
      }else{pl <- pa}
      pl
    })

    #----------------------plot
    output$plot <- renderPlot({
        p()
    })
    
    #----------------------code
    data_code <- reactive({
      if(input$data.source != "Choose Source" && input$data.source != "Import Dataset"){
        paste(input$data.source, " %>% ", sep = "")
      }
      else{
        paste("<data_object>", " %>% ", sep = "")
      }
    })
    
    ggplot_code_str <- reactive({
      if(input$single.var.check==F){
        paste("ggplot(aes(y=", y_var(), ", x=", x_var(), "))", sep = "")
      }
      else if(input$single.var.check==T){
        paste("ggplot(aes(y=", y_var(), "))", sep = "")
      }else{NULL}
    })
    
    chosen_geom_func <- reactive({
      if(input$geom == "Choose Geometry"){
        NULL
      }
      else if(input$geom == "Density"){
        "geom_density"
      }
      else if(input$geom == "Dotplot"){
        "geom_dotplot"
      }
      else if(input$geom == "Histogram"){
        "geom_histogram"
      }
      else if(input$geom == "Bar"){
        "geom_bar"
      }
      else if(input$geom == "Point"){
        "geom_point"
      }
      else if(input$geom == "Rug"){
        "geom_rug"
      }
      else if(input$geom == "Smooth"){
        "geom_smooth"
      }
      else if(input$geom == "Boxplot"){
        "geom_boxplot"
      }
      else if(input$geom == "Violin"){
        "geom_violin"
      }
      else if(input$geom == "Area"){
        "geom_area"
      }
      else if(input$geom == "Line"){
        "geom_line"
      }
      else if(input$geom == "Step"){
        "geom_step"
      }
    })
    
    present_geom_args <- reactive({
      geom_arg_str <- '('
      if(!is.null(color_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(color_reac())), "^.*?(?=_)"), "=", color_reac(), sep='')
      }
      else if(!is.null(alpha_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(alpha_reac())), "^.*?(?=_)"), "=", alpha_reac(), sep='')
      }
      else if(!is.null(fill_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(fill_reac())), "^.*?(?=_)"), "=", fill_reac(), sep='')
      }
      else if(!is.null(size_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(size_reac())), "^.*?(?=_)"), "=", size_reac(), sep='')
      }
      else if(!is.null(linetype_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(linetype_reac())), "^.*?(?=_)"), "=", linetype_reac(), sep='')
      }
      else if(!is.null(position_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(position_reac())), "^.*?(?=_)"), "=", position_reac(), sep='')
      }
      else if(!is.null(shape_reac())){
        geom_arg_str <- paste(geom_arg_str, stringr::str_extract(deparse(substitute(shape_reac())), "^.*?(?=_)"), "=", shape_reac(), sep='')
      }
      else{geom_arg_str <- NULL}
      geom_arg_str
    })
    
    geom_func_code <- reactive({
      if(is.null(chosen_geom_func())){
        NULL
      }
      else if(!is.null(chosen_geom_func()) && is.null(present_geom_args())){
        paste(" + ", chosen_geom_func(), "()", sep = "")
      }
      else if(!is.null(chosen_geom_func()) && !is.null(present_geom_args())){
        paste(" + ", chosen_geom_func(), present_geom_args(), ")", sep = "")
      }
      else{NULL}
    })
    
    labs_code <- reactive({
      if(input$single.var.check==T){
        if(!is.null(input$title) && is.null(input$var_lab)){
          paste(" + labs(title = ", input$title, ")", sep = "")
        }  
        else if(is.null(input$title) && !is.null(input$var_lab)){
          paste(" + labs(y = ", input$var_lab, ")", sep = "")
        }
        else if(!is.null(input$title) && !is.null(input$var_lab)){
          paste(" + labs(title = ", input$title, ", y = ", input$var_lab, ")", sep = "")
        }else{NULL}
      }
      else if(input$single.var.check==F){
        if(!is.null(input$title) && is.null(input$y_lab)){
          paste(" + labs(title = ", input$title, ")", sep = "")
        }  
        else if(is.null(input$title) && !is.null(input$y_lab)){
          paste(" + labs(y = ", input$y_lab, ", x = ", input$x_lab, ")", sep = "")
        }
        else if(!is.null(input$title) && !is.null(input$y_lab)){
          paste(" + labs(title = ", input$title, ", y = ", input$y_lab, ", x = ", input$x_lab, ")", sep = "")
        }else{NULL}
      }
    })
    
    facet_code <- reactive({
      if(is.null(facet_string())){
        NULL
      }
      else{
        paste(" + ", facet_string(), sep = "")
      }
    })
    
    theme_code <- reactive({
      if(is.null(theme_reac())){
        NULL
      }
      else{
        paste(" + ", input$theme, "()", sep = "")
      }
    })
    
    code_reac <- reactive({
      code_combo <- function(data_code=NULL, ggplot_code_str=NULL, geom_func_code=NULL, labs_code=NULL, facet_code=NULL, theme_code=NULL){
        paste(data_code, ggplot_code_str, geom_func_code, labs_code, facet_code, theme_code, sep = "")
      }
      code_elemets <- list(data_code(), ggplot_code_str(), geom_func_code(), labs_code(), facet_code(), theme_code())
      do.call(code_combo, code_elemets)
    })
    
    output$plot_code <- renderPrint(
      if(input$geom == "Choose Geometry"){
        cat("Select plot geometry type.")
      }
      else if(input$geom != "Choose Geometry"){
        cat(noquote(code_reac()))
      }
    )
}

ui <- navbarPage(title = "vizLearn",
                 
                 theme = shinytheme("sandstone"),
                 
                 #--------------------------------------------Data reading and transformation
                 tabPanel(
                     "Data",
                     sidebarLayout(
                         sidebarPanel(
                           
                             selectInput("data.source", "Data",
                                         choices = c("Choose Source", "Import Dataset", "mtcars", "iris", "attitude", "airquality", "freeny", "USArrests")
                                         ),
                             uiOutput("file.type"),
                             uiOutput("file.select"),
                             
                             checkboxInput("header", "Header", TRUE),
                             
                             tags$hr(),
                             
                             radioButtons("disp", "Display",
                                          choices = c(Head = "head",
                                                      All = "all"),
                                          selected = "head"),
                             
                             verbatimTextOutput("str.info")
                             
                         ), #close sidebarPanel
                         
                         mainPanel(
                           
                             div(style = 'overflow-y: scroll;
                                           height: 542px',
                                reactableOutput("reac_table")
                             ),
                             
                             tags$hr(),
                             uiOutput("tr.fun_select"),
                             tags$hr(),
                             uiOutput("tr.filter"),
                         ) #close mainPanel
                     ) #close sidebarLayout
                 ), #close tabPanel for 'Data'
                 
                 #--------------------------------------------plot creation / feature adjustment  
                 tabPanel(
                     "Plot",
                     sidebarLayout(
                       
                       sidebarPanel(
                          uiOutput("single_var_check"),
                          uiOutput("geom_select"),
                          uiOutput("y_select"),
                          uiOutput("x_select"),
                          uiOutput("position_adj"),
                          uiOutput("theme_select"),
                          uiOutput("facet_check"),
                          uiOutput("facet_select"),
                          uiOutput("facet_var1"),
                          uiOutput("facet_var2")
                      ),#close sidebarPanel
                       
                      mainPanel(
                         plotOutput("plot"),
                         verbatimTextOutput("plot_code"),
                         tabsetPanel(
                           tabPanel("Labels",
                              uiOutput("lab_check"),
                              #title and axis labels
                            fluidRow(
                              column(uiOutput("plot_title"), width = 6)
                            ),
                            fluidRow(
                              column(uiOutput("y_label"), width = 6),
                              column(uiOutput("x_label"), width = 6)
                            ),
                          ),
                           
                           tabPanel("Aesthetic Specifications",
                              uiOutput("aes_check"),
                              tags$hr(),
                              #color
                              uiOutput("color_select"),
                              #alpha
                              uiOutput("alpha_adjust"),
                              uiOutput("alpha_scale"),
                              #size
                              uiOutput("size_adjust"),
                              #fill
                              uiOutput("fill_select"),
                              #linetype
                              uiOutput("linetype_select"),
                              #shape
                              uiOutput("shape_select"),
                           )
                        )#close tabset Panel
                      )#close mainPanel
                    )#close sidebarLayout     
                 )#close tabPanel for 'Plot'
    ) #close navbarPage

shinyApp(ui = ui, server = server)