#import library


#library(editData)
#library(DataEditR)
#library(tidyverse)
#library(tidyquant)
#library(data.tree)
library(igraph)
library(networkD3)
#library(DiagrammeR)
library(plyr)
library(data.table)
library(shiny)
library(shiny.fluent)
library(shinyalert)
library(shinyjs)

values <- reactiveValues()
values$edgeList <- NULL


##### UI -----
ui <- shiny.fluent::fluentPage( 

  shiny::tags$head(
    shiny::tags$link(href = "custom.css", rel = "stylesheet", type = "text/css")
  ),
  shiny::uiOutput("select_network"),
  shiny::br(),
  shiny::div(
  shiny.fluent::Checkbox.shinyInput("edit_check", value = FALSE, label="Check to Edit Node", boxSide="end")),
  shiny::br(),
  #shiny::br(),
  shiny::uiOutput("edit_node"),
  #shiny.fluent::Slider.shinyInput("charge", value = -800, min= -1200, max=200),
  networkD3::forceNetworkOutput(outputId = "force_network", width = "100%", height = "900px")
  
  

)


#### Server ----
server <- function(input, output, session) {
  
  global <- reactiveValues(response = FALSE)
  session_store <- reactiveValues()
  
  
  files_list <- shiny::reactive({
    files <- list.files("all_rds_files/", pattern = ".rds")
    files <- unlist(strsplit(files,".rds"))
    files <- lapply(files, function(x) {list(key=x, text=x)})
  })
  
  output$select_network <- shiny::renderUI({
    shiny.fluent::ComboBox.shinyInput("show_network",
                                      label="Select Network to Show",
                                      value = list(key="create_new", text="create_new"),
                                      options=files_list())
  })
  
  shiny::observeEvent(input$show_network,{
    file <- input$show_network$text
    file_path <- paste0("all_rds_files/", file, ".rds")
    dt_shiny <- readRDS(file_path)
    
    dt_shiny <- data.table::as.data.table(dt_shiny)
    #dt_shiny <- rbindlist(list(dt_shiny, data.table(from="Lab_Members", to="New_member", name="New_member")))
    
    
    edgeList <- dt_shiny[,1:2]
    colnames(edgeList) <- c('SourceName', 'TargetName')
    
    #from_choice <- unique(edgeList[["SourceName"]])
    
    values$edgeList <- edgeList
    
    
    
  })
  

  
  
  add_choice <- shiny::reactive({
    from_choice <- unique(values$edgeList[["SourceName"]])
    to_choice <- unique(values$edgeList[["TargetName"]])
    add_choice <- union(from_choice, to_choice)
    add_choice <- lapply(add_choice, function(x) {list(key=x, text=x)})
    add_choice
    
    
  })
  
 # file_name_choice <- reactive({
 #   
 # })
  

  
  output$edit_node <- shiny::renderUI({
    
    if (input$edit_check) {
    shiny::div(
    
    #shiny::h4("Add a New Node"),
    shiny.fluent::Text(variant="large" ,"Add a New Node", block=TRUE),
    
    shiny.fluent::Stack(
      tokens=list(childrenGap=10),
      horizontal=TRUE,
      
      shiny.fluent::ComboBox.shinyInput("add_from",
                                        
                                        options=add_choice(),
                                        label="From",
                                        allowFreeform=TRUE),
      shiny.fluent::ComboBox.shinyInput("add_to",
                                        options=add_choice(),
                                        label="To",
                                        allowFreeform=TRUE)
      
    ),
    shiny.fluent::ActionButton.shinyInput("add_node", text="Add this node to network",
                                          iconProps = list("iconName" = "AddLink")),
    shiny::br(),
    shiny.fluent::Text(variant="large" ,"Delete a Node", block=TRUE),
    
    shiny.fluent::Stack(
      tokens=list(childrenGap=10),
      horizontal=TRUE,
      shiny.fluent::ComboBox.shinyInput("delete_from",
                                        
                                        options=add_choice(),
                                        label="From",
                                        allowFreeform=TRUE),
      shiny.fluent::ComboBox.shinyInput("delete_to",
                                        options=add_choice(),
                                        label="To",
                                        allowFreeform=TRUE)
      
    ),
    shiny.fluent::ActionButton.shinyInput("delete_node", text="Remove this node from network",

                                          iconProps = list("iconName"="RemoveLink")),
    shiny::tags$br(),
    # shiny.fluent::ComboBox.shinyInput("file_name",
    #                                   
    #                                   options=add_choice(),
    #                                   label="From",
    #                                   allowFreeform=TRUE),
    shiny.fluent::ComboBox.shinyInput("save_rds",
                                      label="Overwrite current file or save as new file",
                                      value = list(key="dtNetwork", text="dtNetwork"),
                                      options=files_list(),
                                      allowFreeform=TRUE),
    # 
    shiny.fluent::ActionButton.shinyInput("write_rds", text="Save all the changes to RDS file",
                                          iconProps=list("iconName"="Database")),
    
    shiny::br(),
    shiny::downloadButton("save_html", "Download Displayed Network",
                          style="color: #323130;"),
    shiny::tags$br(),
    shiny::tags$br()
    
      # shiny.fluent::ActionButton.shinyInput("save_html", text="Download Displayed Network",
      #                                       iconProps=list("iconName"="Download")),
   
    
    

    
    
    
    )}
    
    
  })
  
  #### observeEvent for add_node ----
  shiny::observeEvent(input$add_node,{
    #print(values$edgeList)
    from <- input$add_from$text
    to <- input$add_to$text
    values$edgeList <- data.table::rbindlist(list(values$edgeList,
                                                  data.table(SourceName=from, TargetName=to)))
    # print(values$edgeList)
    
  })
  #### delete node
  shiny::observeEvent(input$delete_node,{
    #print(values$edgeList)
    from <- input$delete_from$text
    to <- input$delete_to$text
    values$edgeList <- values$edgeList[!(SourceName==from & TargetName==to)]
    values$edgeList <- values$edgeList[!(SourceName==to & TargetName==from)]
    # print(values$edgeList)
    
  })
  
  observeEvent(input$id, {
    if (input$edit_check) {
      shinyalert::shinyalert(html = TRUE, showCancelButton = TRUE,text = tagList(
        textInput("node_click", "Edit Node", input$id)
      ),
      callbackR = function(x) {
        global$response <- x
      }
      )}
    print("line 209")
    print(global$response)
  })
  
  shiny::observeEvent(global$response,{
    if(global$response){
      print(input$node_click)
      values$edgeList[SourceName==input$id, SourceName:=input$node_click] 
      values$edgeList[TargetName==input$id, TargetName:=input$node_click]
      global$response <- FALSE
     
     
      # shiny.fluent::updateComboBox.shinyInput(session = shiny::getDefaultReactiveDomain(),
      #                                         inputId = "show_network",
      #                                         value=list(key=input$show_network,text=input$show_network))
    }
  })
  
  
  shiny::observeEvent(input$write_rds,{
    file_name <- paste0(input$save_rds$text, ".rds")
    path <- paste0("all_rds_files/", file_name)
    file <- values$edgeList
    saveRDS(file, path )
    files <- list.files("all_rds_files/", pattern = ".rds")
    files <- unlist(strsplit(files,".rds"))
    files <- lapply(files, function(x) {list(key=x, text=x)})

    shiny.fluent::updateComboBox.shinyInput(session = session,
                                      inputId = "show_network",
                                      
                                      #value = list(key=input$show_network$text, text=input$show_network$text),
                                      options=files)
    shiny.fluent::updateComboBox.shinyInput(session=session,
                                      inputId = "save_rds",
                                      options=files,
                                      allowFreeform=TRUE)



    
    
  })
  
  
  
  #### render network ----
  output$force_network <- networkD3::renderForceNetwork({
    shiny::req(input$show_network)
    
    global$response
    edgeList <- values$edgeList
    
    # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
    gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))
    
    # Create a node list object (actually a data frame object) that will contain information about nodes
    nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), # because networkD3 library requires IDs to start at 0
                           nName = igraph::V(gD)$name)
    
    # Map node names from the edge list to node IDs
    getNodeID <- function(x){
      which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
    }
    # And add them to the edge list
    edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName"), 
                            function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                    TargetID = getNodeID(x$TargetName)))
    
    # Define Node Groups (colors)
    nodeList$Group <- nodeList$nName
    colorGroups <- edgeList$TargetName[which(edgeList$SourceName == edgeList[1,1])]
    for (colorGroup in colorGroups) {
      groupTargets <- edgeList$TargetID[which(edgeList$SourceName == colorGroup)]
      nodeList$Group[which(nodeList$ID %in% groupTargets)] <- colorGroup
    }
    # methodTargets <- edgeList$TargetID[which(edgeList$SourceName == 'Methodologies')]
    # nodeList$Group[which(nodeList$ID %in% methodTargets)] <- 'Methodologies'
    # # memberNames <- c('Dan', 'Kevin', 'Sabbir', 'Susan', 'Vaishnavi', 'Yousuf')
    # # nodeList$Group[which(nodeList$nName %in% memberNames)] <- 'Members'
    # methodTargets <- edgeList$TargetID[which(edgeList$SourceName == 'Resources')]
    # nodeList$Group[which(nodeList$ID %in% methodTargets)] <- 'Resources'
    # methodTargets <- edgeList$TargetID[which(edgeList$SourceName == 'Lab_Members')]
    # nodeList$Group[which(nodeList$ID %in% methodTargets)] <- 'Lab_Members'
    
    ############################################################################################
    # Calculate some node properties and node similarities that will be used to illustrate 
    # different plotting abilities and add them to the edge and node lists
    
    # Calculate degree for all nodes
    nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))
    
    # Calculate betweenness for all nodes
    betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
    betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
    nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    rm(betAll, betAll.norm)
    
    #Calculate Dice similarities between all pairs of nodes
    dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
    
    F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
    edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "SourceID", "TargetID"), 
                            function(x) data.frame(F1(x)))
    
    rm(dsAll, F1, getNodeID, gD)
    
    ############################################################################################
    # We will also create a set of colors for each edge, based on their dice similarity values
    # We'll interpolate edge colors based on the using the "colorRampPalette" function, that 
    # returns a function corresponding to a collor palete of "bias" number of elements (in our case, that
    # will be a total number of edges, i.e., number of rows in the edgeList data frame)
    F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")
    colCodes <- F2(length(unique(edgeList$diceSim)))
    edges_col <- sapply(edgeList$diceSim, function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])
    
    rm(colCodes, F2)
    ############################################################################################
    # Let's create a network
    
    D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                             Nodes = nodeList, # data frame that contains info about nodes
                                             Source = "SourceID", # ID of source node 
                                             Target = "TargetID", # ID of target node
                                             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                                             # Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                             NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                             Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                             Group = "Group",  # value from the node list (data frame) that contains value we want to use for node color
                                             # height = 500, # Size of the plot (vertical)
                                             # width = 1000,  # Size of the plot (horizontal)
                                             # fontSize = 20, # Font size
                                             # linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                             # linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                             fontSize = 16,
                                             charge = -800,
                                             opacity = 1, # opacity
                                             zoom = TRUE, # ability to zoom when click on the node
                                             opacityNoHover = 1, # opacity of labels when static
                                             linkColour = edges_col, # edge colors
                                             clickAction="Shiny.onInputChange('id', d.name);"
                                             
    )
    
    # Plot network
    #D3_network_LM
    session_store$network <- D3_network_LM
    session_store$network
    
  })
  
  
  output$save_html <- shiny::downloadHandler(
    filename = function() {
      paste0(input$show_network$text, ".html")
    },
    content = function(file){
      htmlwidgets::saveWidget(session_store$network, file,selfcontained = TRUE)
    }
  )
  
  # shiny::observeEvent(input$save_html, {
  #   shiny::downloadHandler(
  #     filename = function() {
  #       paste0(input$show_network$text, ".html")
  #     },
  #     content = function(file){
  #       htmlwidgets::saveWidget(session_store$network, file,selfcontained = TRUE)
  #     }
  #   )
  #   
  # })
  
  

  
  
}

shinyApp(ui, server)

