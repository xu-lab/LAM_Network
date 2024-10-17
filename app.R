#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(igraph)
library(visNetwork) # specifically for filtering nodes and zooming in/out
library(DT) # for putting borders around the figures
# https://stackoverflow.com/questions/75149954/add-black-border-to-sidebarpanel-and-mainpanel-r-shiny
library(shinyGizmo)
library(xfun) # for handling different file types
library(readxl) # for reading Excel files
library(data.table) # for rearranging input data tables
library(ggvenn)
library(tidyverse)
library(shinycssloaders) # for loading images into the app

# Selectize input
# https://github.com/yihanwu/Nutrient_Calculator/blob/master/app.R

ui <- fluidPage(
  headerPanel("LAM Network"),
  tabsetPanel(
    tabPanel("Introduction",
             h4("Gene Network"),
             p("This tab allows the user to view graphs that show interactions betwen transcription factors and target genes.
               One score set can be viewed at a time; the thicknesses of the directed edges depend on how high or low the score is
               for those particular interactions. If the network is very large, it can be subsetted multiple ways:"),
             p("- Choose one or more nodes from a dropdown menu to view only those nodes and their interacting partners."),
             p("- Use a double-slider-bar to set a maximum and minimum for the scores to be displayed on screen."),
             p("- Type a maximum number of edges to display. Edges with higher scores are prioritized."),
             p("If a regulation column is included in the inpout file, the edges will be colored red or green depending on whether
             those interactions are up or down-regulated, respectively. If this information is not provided, the edges will all be
               colored light-blue. A table is also available for the user to view the raw data."),
             #withSpinner(imageOutput("data_table")),
             fluidRow(
               column(5, withSpinner(imageOutput("net_reg"))),
               column(5, withSpinner(imageOutput("net_noreg"))),
               column(2, withSpinner(imageOutput("table")))
             ),
             br(),
             h4("Compare Target Genes"),
             p("This tab allows the user to select two transcription factors from a dataset, and view the target genes they activate.
               A Venn diagram is displayed to give the user an idea of how many target genes are shared among the two selected transcription
               factors. A data table listing the target genes is also viewable beneath the Venn diagram, and the rows are color-coded
               depending on which transcription factor(s) each target gene is activated by."),
             # withSpinner(imageOutput("venn_table")),
             br(),
             h4("Rules for entering new data tables"),
             p("- File must be in one of three formats: Excel, CSV, or TSV"),
             p("- Transcription factors must be in a column named 'TF'"),
             p("- Target genes must be in a column named 'TG'"),
             p("- 'regulation' column must be comprised of 1's for up-regulated and -1's for down-regulated (optional)"),
             p("- Any other columns of type double will be read in as sets of importance scores, selectable in a dropdown menu."),
             br(),
             br()
             ),
    tabPanel("Gene Network",
             sidebarLayout(
               sidebarPanel(
                 # Which file is to be selected?
                 selectInput("file", "Choose file:", choices = list.files("./networks")),
                 
                 # 3. It's unclear how many scores there will be.
                 # Repurposed for choosing visualization
                 # radioButtons("viz", "Choose layout:",
                 #              choiceNames = c("Sphere", "Grid", "Kamada-Kawai"),
                 #              choiceValues = c("layout_on_sphere", "layout_on_grid", "layout_with_kk")),
                 selectInput("viz", "Choose layout:", choices = c("Random" = "layout_randomly",
                                                                  "Grid" = "layout_on_grid",
                                                                  # "Kamada-Kawai" = "layout_with_kk",
                                                                  "Fruchterman-Reingold" = "layout_with_fr")),
                 
                 # Selectize input: https://shiny.posit.co/r/articles/build/selectize/
                 uiOutput("current_genes"), # text box/dropdown menu for genes
                 uiOutput("current_scores"), # dropdown menu for scores
                 
                 # Top x scores?
                 textInput("top_rank", label = "Maximum number of edges to display (edges with highest scores are prioritized):"),
                 textOutput("rank_error_msg"),
                 
                 # This is the only way to divide the sidebar panel
                 # https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar
                 # https://stackoverflow.com/questions/21921665/how-to-make-hr-full-width
                 tags$hr(style="margin: 30px -20px 20px; border: 0; border-top: 3px solid #C0C0C0;"),
                 
                 # Histogram for score distribution?
                 plotOutput("score_dist", height="250px"),
                 
                 # slider bar for scores (minimum and maximum variable)
                 # https://rdrr.io/cran/shiny/man/sliderInput.html
                 sliderInput("range","Set score boundaries:",min=0,max=100,value=c(0,100)),
                 
                 # How to extract information about the selected node?
                 # https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton
                 # https://www.rdocumentation.org/packages/visNetwork/versions/2.1.2/topics/visGetNodes
                 # https://www.rdocumentation.org/packages/visNetwork/versions/0.2.1/topics/visNetworkProxy
                 # https://stackoverflow.com/questions/39905061/get-node-and-edge-data-from-visnetwork-graph
                 # p(strong("Selected node:")),
                 # textOutput("selected_node_data"),
                 # br(),
                 # p(strong("Adjacent nodes:")),
                 # verbatimTextOutput("connected_node_data", placeholder = TRUE),
                 # Make the verbatimTextOutput scrollable: https://forum.posit.co/t/verbatimtextoutput-sizing-and-scrollable/1193
                 
                 # This is the only way to divide the sidebar panel
                 # https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar
                 # https://stackoverflow.com/questions/21921665/how-to-make-hr-full-width
                 tags$hr(style="margin: 30px -20px 20px; border: 0; border-top: 3px solid #C0C0C0;"),
                 
                 p(strong("Data Table")),
                 DTOutput("tbl"),
                 tags$head(tags$style("#connected_node_data{color:black; font-size:12px;  overflow-y:scroll; max-height: 200px; background: ghostwhite;}")),
                 
                 # p(h4("Other possible updates:")),
                 # p("- Identify and shape-code transcription factors (triangles) and target genes (circles)"),
                 # p("- Color-code the genes based on if they come from a human (green) or mouse (red)")
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tags$head(
                   # Note the wrapping of the string in HTML()
                   tags$style(HTML(".well {
                        border: 3px solid #C0C0C0;
                      }")),
                   tags$style(HTML(".col-sm-8 {
                        border: 3px solid #C0C0C0;
                      }")),
                 ),
                 visNetworkOutput("graphPlot", height = "750px", width="100%")
               )
             )
    ),
    tabPanel("Compare Target Genes",
             sidebarLayout(
               sidebarPanel(
                 p("Select two transcription factors to view their target genes. The Venn diagram will tell you which target genes are shared by the selected transcription factors, if any."),
                 # Which file is to be selected?
                 selectInput("file2", "Choose file:", choices = list.files("./networks")),
                 uiOutput("trans_factor1"),
                 uiOutput("trans_factor2")
               ),
               mainPanel(
                 plotOutput("venn"),
                 DTOutput("venn_table")
                 )
             ))
  )
)

# Centrality of nodes?
# https://thatdarndata.com/node-customization-for-stunning-networks/
  
# SERVER START
server <- function(input, output, session) {
  # Compress files: https://imagecompressor.11zon.com/en/compress-png/compress-png-to-200kb#google_vignette
  output$net_reg <- renderImage({
    list(src = "img/Network_with_reg_11zon.png", width = "100%")
  }, deleteFile = F)
  
  output$net_noreg <- renderImage({
    list(src = "img/Network_without_reg_11zon.png", width = "100%")
  }, deleteFile = F)
  
  output$table <- renderImage({
    list(src = "img/data_table.png", width = "100%")
  }, deleteFile = F)
  
  # output$venn_table <- renderImage({
  #   list(src = "img/venn_11zon.png", width = "100%")
  # }, deleteFile = F)
  
  # 1. if someone picks a particular cell, call up that data table
  print("---- START OF SERVER FUNCTION ----")
  current_file = reactive(input$file) # which graph to display...
  print(reactive(input$file))
  #current_weight = "Score" # reactive(input$weights) # weight category to be displayed
  
  # Allow for multiple file types: Excel, CSV, TSV
  initial_data = reactive({
    in_file = paste0("networks/", current_file())
    print("---------------")
    print("---------------")
    print(in_file)
    in_ext = file_ext(in_file)
    print(in_ext)
    if(in_ext == "xlsx"){
      print("Checkpoint 1a: Found an Excel file")
      result = read_excel(in_file, sheet = 1)
    } else if(in_ext == "csv"){
      print("Checkpoint 1b: Found a CSV")
      result = read.csv(in_file)
    } else if (in_ext == "tsv"){
      print("Checkpoint 1c: Found a TSV")
      result = read.table(in_file, header = T, sep = '', stringsAsFactors = F)
    }
    
    # Remove duplicates from data set
    # https://www.statology.org/remove-duplicate-rows-in-r/
    result = result[!duplicated(result), ]
    # print(result)
    return(result)
    }
  )
  
  # Subset the network to a particular gene and its interacting partners
  output$current_genes = renderUI({
    selectInput("current_genes",
                "Select genes to view subnetwork:",
                choices = c(sort(unique(c(initial_data()$TF,
                                          initial_data()$TG)))),
                multiple = TRUE)
  })
  
  selected_genes = reactive({input$current_genes})
  
  # Filter the data if one or more genes is selected
  current_data = reactive({
    print("Genes selected")
    print(length(selected_genes()))
    
    # If no genes are selected, just display all of them
    if(length(selected_genes()) == 0){
      result = initial_data()
    } else {
      # This is the line that will create a subnetwork
      result = subset(initial_data(), TF %in% selected_genes() | TG %in% selected_genes())
    }
    return(result)
  })
  
  # Choose scores based on which columns are of type double
  output$current_scores = renderUI({
    selectInput("current_scores",
                "Select score to display on edges (thicker edges denote higher scores)",
                choices = colnames(current_data()[(sapply(current_data(), typeof) == "double") &
                                               !(colnames(current_data()) %in% c("TF", "TG", "regulation"))]))
  })
  
  selected_score = reactive({input$current_scores})
  
  top_x = reactive({input$top_rank})
  
  # If the user wants to set a limit on the number of edges drawn,
  # another copy of current_data wiill need to be made.
  # Do not change current_data.
  # https://www.geeksforgeeks.org/duplicate-a-data-frame-using-r/
  
  # We're not going to have action buttons. It turns out you CAN'T use an observeEvent
  # to modify a reactive value defined inside the server.
  
  # Histogram for score?
  output$score_dist = renderPlot({
    #print("Selected score")
    #print(typeof( unlist(current_data()[selected_score()]) ))
    
    # Need this failsafe to ensure no errors are displayed
    # Rotating axis labels (las = 1)
    # https://stackoverflow.com/questions/1828742/rotating-axis-labels-in-r
    if(!is.null(selected_score())){
      print("Checkpoint 2: Drawing a histogram")
      hist( unlist(current_data()[selected_score()]),
            main=paste("Distribution of", selected_score()),
            xlab="Score",
            ylab=NULL,
            col="magenta",
            freq=TRUE,
            las = 1 # flips the y-axis labels horizontally
      )
    } else {
      print("Checkpoint 3: Not drawing a histogram")
    }
  })
  
  # Slider bar for the selected score
  # https://rstudio.github.io/shiny/reference/updateSliderInput.html
  # Listen for two inputs at a time
  # https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  
  # If the user changes either the current score of the current genes,
  # The sldier bar should adjust to the the edge score boundaries of the new (sub)network
  observeEvent(list(input$current_scores, input$current_genes), {
    print("Checkpoint 4: User attempted to change score")
    max_score = ceiling( max(current_data()[selected_score()]) )
    
    # If the max score is a very small number, only round to the nearest hundredth
    if(abs(max_score) <= 3){
      max_score = round( max(current_data()[selected_score()]), digits = 2 )
    }
    
    min_score = floor( min(current_data()[selected_score()]) )
    step_size = (max_score - min_score) / 50
    
    updateSliderInput(session, "range", value = c(min_score, max_score),
                      min = min_score, max = max_score, step = step_size)
  })

  
  # Start of renderVisNetwork (FIRST TAB)
  output$graphPlot <- renderVisNetwork({
    print("---------------")
    print("Checkpoint 5: Entered renderVisNetwork")
    print(current_file())
    print("Selected score")
    print(selected_score())
    # print("Current file")
    # print(paste(current_file(), ".csv", sep=""))
    NE = current_data() # copy
    #print("What's this do?")
    #print(input$node_names)
    
    # If TF and TG aren't already the front two columns, they need to be!
    # https://stackoverflow.com/questions/22286419/move-a-column-to-first-position-in-a-data-frame#:~:text=A%20native%20R%20approach
    setcolorder(NE, neworder = c("TF", "TG"))
    
    # 7. It's unclear whether I will be adding a weight filter.
    # around here we have to remove the edges with weights out of bounds
    # NE = NE[(NE[current_weight] >= input$range),]
    
    print("Checkpoint 6: Cleared updateSelectizeInput")
    
    
    #print(!(input$geneName == "All genes" | input$geneName == ""))
    print("Checkpoint 7: Cleared observeEvent")
    
    # How to reorder the data frame in descending order of a column?
    # https://www.datacamp.com/doc/r/sorting
    
    # 2. Ensure that "TF" and "TG" are included among the columns
    # also remove the edges with the same node on both ends (LOOPS)
    NE = NE[(NE["TF"] != NE["TG"]),]
    #print("NE")
    #print(NE)
    
    # around here we have to remove the edges with weights out of bounds
    if(!is.null(selected_score())){
      NE = NE[(NE[selected_score()] >= input$range[1] & NE[selected_score()] <= input$range[2]),]
      print("Edges filtered successfully!")
      # colnum = which(colnames(NE) == selected_score())
      # # need dplyr/tidyverse for this next step
      # NE = NE %>%
      #   arrange(desc(NE[colnum]))
      # print(NE)
    } else {
      print("No edge filtering was done.")
    }
    
    # If the user decides to enter a maximum number of edges to display
    #print("NE is now")
    #print(NE)
    #print("top_x")
    #print(top_x())
    num_edges = dim(NE)[1]
    #print(num_edges)
    
    # Which column number is the selected score in?
    colnum = which(colnames(NE) == selected_score())
    
    topX = 0
    
    # They need a valid entry
    if(num_edges == 0) {
      # There are no edges on screen!
      output$rank_error_msg = renderText({
        "There are no edges on screen. Cannot filter further."
      })
    } else if(is.na(as.integer(top_x()))) {
      # The user entered a non-number!
      # Just output all the edges.
      output$rank_error_msg = renderText({
        paste0("Network has ", num_edges, " edges.")
      })
      topX = num_edges
    } else if(as.integer(top_x()) > num_edges){
      # The user entered a number that is too large
      output$rank_error_msg = renderText({
        paste0("Number should be less than or equal to ", num_edges, ".")
      })
      topX = 0 # Show no edges, or all the edges?
    } else if(as.integer(top_x()) < 1){
      # The user entered a number that is too small
      output$rank_error_msg = renderText({
        "Cannot filter to less than 1 edge."
      })
      topX = 0
    } else {
      output$rank_error_msg = renderText({
        paste0("Showing ", top_x(), " of ", num_edges, " edges.")
      })
      topX = as.integer(top_x())
    }
    
    NE = NE %>%
      arrange(desc(NE[colnum])) %>%
      head(topX)
    
    print("Selected gene")
    print(selected_genes())
    
    # Which set of scores do we want to display?
    NE$weight = round(NE[[selected_score()]])
    
    #print("Weights")
    #print(NE$weight)
    print("Checkpoint 8: About to create graph")
    #print("NE")
    #print(NE)
    
    # Create the graph
    g = graph_from_data_frame(NE)
    v = V(g) # gets the individual nodes
    #print("Graph object G")
    #print(g)
    
    # Group these into transcription factors and target genes (shape-code)
    TF = unique(NE$TF)
    TG = unique(NE$TG)
    TG_exc = setdiff(TG, TF) # which genes are in TG but not TF?
    #print("What's goin on here?")
    #print(TF) # 44
    #print(TG_exc) # 458
    shape.vec = c(rep("triangle", length(TF)), rep("circle", length(TG_exc)))
    
    
    # Adding weights?
    # https://stackoverflow.com/questions/51856706/weighted-graph-from-a-data-frame
    
    nodes<-data.frame(id=v$name, label=v$name)
    nodes$group = ifelse(nodes$id %in% TF, "transcription\nfactor", ifelse(nodes$id %in% TG,"target gene","N/A"))
    print("Checkpoint 9: Created nodes")
    #print(nodes)
    
    print("Checkpoint 10: Nodes grouped")
    #print(g)
    
    # https://stackoverflow.com/questions/4904972/convert-igraph-object-to-a-data-frame-in-r
    # https://www.rdocumentation.org/packages/igraph/versions/1.3.5/topics/as_data_frame
    # Apparently you can't coerce igraph to data frame anymore?!?
    # Unless: https://igraph-help.nongnu.narkive.com/aTDM5b2i/igraph-converting-graph-objects-to-data-frame
    edges=igraph::as_data_frame(g, what=c("edges"))
    
    #edges = NE # the outputs are the same
    
    print("Checkpoint 11: Created edges")
    #print(edges)
    # print(current_weight())
    #print("Edge weights")
    #print(edges$weight)
    
    # 5. For the edges to vary in thickness, we need this line
    edges$value=edges$weight
    
    # Color the edges based on regulation value
    # https://sparkbyexamples.com/r-programming/r-create-a-new-column-based-on-condition/
    if("regulation" %in% names(current_data())){
      # The column in edges should be titled "color":
      # https://stackoverflow.com/questions/78023798/color-edges-according-to-edge-attribute-in-visnetwork
      print("We found a regulation column!")
      edges <- edges %>% mutate(color = ifelse(regulation == 1, "chartreuse", ifelse(regulation == -1, "red", "lightblue")))
    }
    else {
      print("We did not find a regulation column.")
    }
    #print("Edge values")
    #print(edges$value)
    print("Checkpoint 12: Starting visNetwork...")
    
    # How many neighbors? Get the degree of every node
    # https://github.com/datastorm-open/visNetwork/issues/168
    degree_value = degree(g)
    print("Degree")
    nodes$value = degree_value[match(nodes$id, names(degree_value))]
    print("Checkpoint 13: Edges now...")
    #print(edges)
    print("cHECKPOINT 14: Nodes now...")
    #print(nodes)
    
    visNetwork(nodes=nodes, edges=edges, height="650px",width="650px") %>%
      visNodes(
        shape = shape.vec, # 4. If the genes aren't put into groups, there's no need for varying shapes.
        color = list(
          border = "black",
          background = "orange"
        ),
        font = list(size = 30)
      ) %>%
      visEdges(
        color = list(color = "lightblue", highlight="purple", hover="purple", inherit="from"),
        arrows = "to", # which direction should the arrows point?
        arrowStrikethrough = FALSE # if TRUE, some arrowheads are placed before the ends of their edges
      ) %>%
      # Which layout to use?
      # https://stackoverflow.com/questions/46483969/visnetwork-with-r-how-to-prevent-nodes-from-overlapping-with-edges
      # https://igraph.org/r/doc/layout_.html
      # Good layouts: layout_on_sphere, layout_with_kk (Kamada-Kawai algorithm), layout_on_grid
      # layout_with_graphopt scales well to large graphs
      visIgraphLayout(layout = input$viz, randomSeed = 555) %>%
      visGroups(groupname="transcription\nfactor",shape="triangle") %>%
      visGroups(groupname="target gene",shape="dot") %>%
      visOptions(selectedBy = "group",
                 nodesIdSelection=list(enabled=T,
                                       useLabels=T,
                                       main="Select gene"), # this will allow for a dropdown menu for nodes
                 highlightNearest=list(enabled=T,
                                       hover=T,
                                       labelOnly=F,
                                       hideColor="lightblue",
                                       degree=1)) %>%
      visLegend(width = 0.1, zoom=FALSE) %>%
      # ARE YOU SERIOUS?!? It was one word (click) all along?!?!?
      # https://stackoverflow.com/questions/39916465/click-events-for-visnetwork-with-shiny
      visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}")
      
    # print("Checkpoint 12: Cleared visNetwork")
    # visEvents allows for an event to happen if a node is clicked.
    # https://www.rdocumentation.org/packages/visNetwork/versions/2.1.2/topics/visEvents
    # In this case, we want to display node data.
    
    # visOptions allows you to pick the node itself!
    # can set sizes of hexagons and triangles manually, but can't get labels inside of them
    # dot is a type of circle that can ensure consistent size
    # circle would vary in size based on how long the text inside is
    # Legend: https://www.rdocumentation.org/packages/visNetwork/versions/2.1.2/topics/visLegend
    
    # De-highlight secondary edges? Still stuck on that...
    # https://stackoverflow.com/questions/50532361/visnetwork-highlightnearest-show-only-connected-edges-to-a-selected-node
    
    # Get selected and connected nodes?
    # visGetSelectedNodes and visGetConnectedNodes
    # Viz Network Proxy?
    # https://www.rdocumentation.org/packages/visNetwork/versions/2.1.2
    # shiny::runApp(system.file("shiny", package = "visNetwork"))
  }
  )
  
  # Using a VisNetworkProxy to return selected nodes from a network
  # https://datastorm-open.github.io/visNetwork/shiny.html
  # https://rdrr.io/cran/visNetwork/man/visGetSelectedNodes.html
  # shiny::runApp(system.file("shiny", package = "visNetwork"))
  # https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton
  # https://stackoverflow.com/questions/39916465/click-events-for-visnetwork-with-shiny
  # https://github.com/datastorm-open/visNetwork/issues/188
  
  # Output a data table of selected node data:
  # https://stackoverflow.com/questions/52480210/update-a-datatable-after-clicking-on-a-network-node-in-a-shiny-app
  output$tbl <- DT::renderDataTable({
    print("---------------")
    print("Checkpoint 15: Creating data table")
    DT::datatable(data = current_data()[c("TF", "TG", selected_score())],
                  options = list(scrollX = TRUE)
    )
  })
  
  # Code for Venn diagram
  current_file2 = reactive(input$file2) # which graph to display...
  print("Checkpoint 16")
  print(reactive(input$file2))
  
  initial_data2 = reactive({
    in_file2 = paste0("networks/", current_file2())
    in_ext2 = file_ext(in_file2)
    print("Checkpoint 17: Reading in data on second tab")
    print(in_ext2)
    if(in_ext2 == "xlsx"){
      result = read_excel(paste0("networks/", current_file2()), sheet = 1)
    } else if(in_ext2 == "csv"){
      result = read.csv(paste0("networks/", current_file2()), nrows = 20000)
    } else if (in_ext2 == "tsv"){
      result = read.table(in_file2, header = T, sep = '', stringsAsFactors = F, nrows = 20000)
    }
    return(result)
  }
  )
  
  output$trans_factor1 = renderUI({
    print("Checkpoint 18")
    #print(unique(initial_data2()$TF))
    selectInput("trans_factor1",
                "Select 1st transcription factor:",
                choices = c(sort(unique(c(initial_data2()$TF)))))
  })
  tf1 = reactive({input$trans_factor1})
  
  output$trans_factor2 = renderUI({
    print("Checkpoint 19")
    selectInput("trans_factor2",
                "Select 2nd transcription factor:",
                choices = c(sort(unique(initial_data2()$TF))))
  })
  tf2 = reactive({input$trans_factor2})
  
  # How do we change the labels based on the transcription factors chosen?
  # https://www.datanovia.com/en/blog/beautiful-ggplot-venn-diagram-with-r/
  output$venn = renderPlot({
    print("Checkpoint 20")
    #print("tf1")
    #print(tf1())
    TF_1 = unique(initial_data2()[initial_data2()$TF == tf1(),]$TG)
    #print(TF_1)
    
    #print("tf2")
    #print(tf2())
    TF_2 = unique(initial_data2()[initial_data2()$TF == tf2(),]$TG)
    #print(TF_2)
    
    out_venn = list("Transcription Factor 1" = TF_1,
                    "Transcription Factor 2" = TF_2)
    names(out_venn) = c(tf1(), tf2())
    
    print(names(out_venn))
    ggvenn(out_venn, show_elements = FALSE, text_size = 8)
  })
  
  output$venn_table = renderDT({
    # curr_data = initial_data2()
    print("Checkpoint 21")
    TF_1 = unique(initial_data2()[initial_data2()$TF == tf1(),]$TG)
    #print(tf1())
    #print(TF_1)
    
    TF_2 = unique(initial_data2()[initial_data2()$TF == tf2(),]$TG)
    #print(tf2())
    #print(TF_2)
    
    cat_vals = sort(unique(c(TF_1, TF_2)))
    #print(cat_vals)
    
    result = data.frame(target_gene = cat_vals,
                        in_cat1 = cat_vals %in% TF_1,
                        in_cat2 = cat_vals %in% TF_2)
    result$in_both = result$in_cat1 == T & result$in_cat2 == T
    result$found_in = apply(result, 1, function(x){
      ifelse(x[4] == T, "Both",
      ifelse(x[2] == T & x[3] == F, tf1(),
      ifelse(x[2] == F & x[3] == T, tf2(), "None")))
    })
    result_dt = datatable(result[c('target_gene', 'found_in')]) |>
      formatStyle(c('target_gene','found_in'),
                  valueColumns = 2,
                  backgroundColor = styleEqual(c(tf1(), tf2(), 'Both'),
                  values = c('skyblue', 'yellow', 'chartreuse')))
    return(result_dt)
  })
  
  print("Checkpoint 22: So... this happened.")
}

if (interactive()) {
  shinyApp(ui, server)
}
