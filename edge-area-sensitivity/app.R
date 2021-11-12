rm(list=ls())
library(dplyr)
library(visNetwork)
library(maptools)
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)

load("./data/matrix_info.rda")
load("./data/bird_info.rda")
load("./data/graph_attr.rda")
load("./data/metadata.rda")
load("./data/edge_lookups.rda")

bird_lookup <- as.data.frame(bird_lookup)
#bird_lookup <- bird_lookup[-which(bird_lookup$V6%in%c("Cuculidae", "Picidae")),]
bird_lookup$V6[bird_lookup$V6=="Tinamidae"] <- "Paridae"
bird_lookup$V6[bird_lookup$original_common=="western flycatcher"] <- "Tyrannidae"

dat <- input.data
dat$taxa[dat$taxa=="community"] <- "Community"
dat$latitude <- as.numeric(dat$latitude)
dat$longitude <- as.numeric(dat$longitude)
dat$forest_class[dat$forest_class %in% c("", " ")] <- "Unclassified"
dat$firstyear <- as.numeric(as.character(dat$firstyear))
dat$firstyear[93] <- 1996
dat[is.na(dat)] <- ""
dat$firstyear <- as.numeric(dat$firstyear)

colnames(matrix_hits) <- c("Developed", "Agriculture", "Industry", "Other")
palette <- c("#0776C5", "#65B4E2", "#FFBA49", "#06A9B2", "#E75371", "#F1E0D9")

ui <- fluidPage(
    theme=shinytheme("flatly"),
    titlePanel("A systematic conceptual model of mechanisms underlying edge and area sensitivity in forest songbirds"),
    h4("Eliza M. Grames, Danielle Schwartz, and Chris S. Elphick"),
    em("If you have questions or would like a copy of the manuscript associated with this site, 
      please contact eliza.grames@uconn.edu."),br(),br(),
    
    # Application title
    
    
    
    div( id ="Sidebar",
         
         title="Conceptual model",
         
         div("Conceptual models are necessary to synthesize what is known about a topic and predict future states of systems, 
       but the process of developing conceptual models has high potential to be biased. 
       We present a novel, systematic approach to conceptual model development through qualitative synthesis and 
       graphical analysis of hypotheses, using edge and area sensitivity of forest songbirds as a case study. 
       We conducted a systematic review to identify population and community studies on breeding forest songbirds 
       that exhibit edge or area sensitivity, extracted hypothesized causal pathways, assembled all causal 
       pathways into a single network model linking hypotheses across studies, and explored trends in research 
       on edge and area sensitivity with respect to space and time. This approach can be used to synthesize 
       scientific thinking across a field of research,
           guide future research to fill in knowledge gaps, and help researchers systematically build conceptual models."), 
         br(), strong("How to use this site: "),
         div("To change which studies are displayed in the map, use the slider and checkboxes to subset the database. 
       If you receive an error upon changing a setting, it means that no studies in the database match all the 
           criteria you have selected. To rearrange the network, click on a node (the circles) and drag to reposition. You can also zoom in and out and move the entire
           network by dragging. To change what studies are included in the network, use the sliders and checkboxes to subset the database; note 
           that this will reset the layout.")),
    
    
    tabsetPanel(type="tabs",
                tabPanel("Map", sidebarLayout(sidebarPanel(
                    sliderInput(inputId = "firstyearmap", "First year of study", min = 1970, max = 2020, value = 1970, sep=""),
                    actionButton("selectalltaxamap", "Select/deselect all taxa"),
                    checkboxGroupInput("taxamap", label = "Taxa", choices=append("Community", sort( unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")]))), 
                                       inline=T, selected="Community"),
                    actionButton("selectallforestmap", "Select/deselect all forest types"),
                    checkboxGroupInput("forestmap", label = "Forest classification", unique(dat$forest_class), 
                                       selected = "Unclassified", inline=T),
                    checkboxGroupInput("matrixmap", label = "Surrounding matrix", colnames(matrix_hits), 
                                       selected=colnames(matrix_hits), inline=T)
                ), 
                mainPanel(leafletOutput(outputId = "mymap", height = "800px")))),
                
                # tabPanel("Metadata", 
                #          sidebarPanel(
                #            sliderInput("firstyearhist", "First year of study", min = 1970, max = 2020, value = 1970, sep=""),
                #            checkboxGroupInput("taxahist", label = "Taxa", append("Community", sort(unique(bird_lookup$V6))), inline=T, selected = append("Community", unique(bird_lookup$V6))),
                #            checkboxGroupInput("foresthist", label = "Forest classification", unique(dat$forest_class), inline=T, selected = unique(dat$forest_class)),
                #            checkboxGroupInput("matrixhist", label = "Surrounding matrix", colnames(matrix_hits), inline=T, selected=colnames(matrix_hits))
                #          ), 
                #          mainPanel(plotOutput("yearhist"))),
                tabPanel("Network", sidebarLayout(
                    sidebarPanel(
                        sliderInput("firstyearnet", "First year of study", min = 1970, max = 2020, value = 1970, sep=""),
                        actionButton("selectalltaxanet", "Select/deselect all taxa"),
                        checkboxGroupInput("taxanet", label = "Taxa", append("Community", sort( unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")]))), 
                                           inline=T, selected = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")]))),
                        actionButton("selectallforestnet", "Select/deselect all forest types"),
                        checkboxGroupInput("forestnet", label = "Forest classification", unique(dat$forest_class), inline=T, selected = unique(dat$forest_class)),
                        checkboxGroupInput("matrixnet", label = "Surrounding matrix", 
                                           choices=colnames(matrix_hits), inline=T, selected=colnames(matrix_hits))
                    ),
                    mainPanel(visNetworkOutput("network", height = "800px"))))))

server <- function(input, output, session) {
    
    
    output$yearhist <- renderPlot({
        
        user_clicked <- (
            (rowSums(bird_dat2[, bird_lookup$V6 %in% input$taxahist]) > 0  | dat$taxa %in% input$taxahist) &
                rowSums(matrix_hits[, colnames(matrix_hits) %in% input$matrixhist]) > 0 &
                (dat$forest_class %in% input$foresthist) &
                as.numeric(as.character(dat$firstyear)) >= input$firstyearhist
        )
        par(las=1)
        hist(
            as.numeric(as.character(dat$firstyear[user_clicked>0])),
            20,
            border = F,
            col = paste(palette[4], "bb", sep=""),
            xlim = c(1965, 2020),
            xlab = "First year of data collection",
            main = ""
        )
    }
    )
    
    observe({
      if (input$selectalltaxamap > 0) {
        if (input$selectalltaxamap %% 2 == 0){
          updateCheckboxGroupInput(session=session, 
                                   inputId="taxamap", inline=T,
                                   choices = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")])),
                                   selected = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")])))
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   inputId="taxamap", inline=T,
                                   choices = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")])),
                                   selected = c())
          
        }}
    })
    
    
    observe({
      if (input$selectallforestmap > 0) {
        if (input$selectallforestmap %% 2 == 0){
          updateCheckboxGroupInput(session=session, 
                                   inputId="forestmap", inline=T,
                                   choices = unique(dat$forest_class),
                                   selected = unique(dat$forest_class))
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   inputId="forestmap", inline=T,
                                   choices = unique(dat$forest_class),
                                   selected = c())
          
        }}
    })
    
    
    observe({
      if (input$selectallforestnet > 0) {
        if (input$selectallforestnet %% 2 == 0){
          updateCheckboxGroupInput(session=session, 
                                   inputId="forestnet", inline=T,
                                   choices = unique(dat$forest_class),
                                   selected = unique(dat$forest_class))
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   inputId="forestnet", inline=T,
                                   choices = unique(dat$forest_class),
                                   selected = c())
          
        }}
    })
    
    observe({
      if (input$selectalltaxanet > 0) {
        if (input$selectalltaxanet %% 2 == 0){
          updateCheckboxGroupInput(session=session, 
                                   inputId="taxanet", inline=T,
                                   choices = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")])),
                                   selected = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")])))
          
        } else {
          updateCheckboxGroupInput(session=session, 
                                   inputId="taxanet", inline=T,
                                   choices = append("Community", unique(bird_lookup$V6[!bird_lookup$V6%in%c("Cuculidae", "Sturnidae", "Picidae")])),
                                   selected = c())
          
        }}
    })
    
    
    
    
    
    
    #create the map
    output$mymap <- renderLeaflet({
        user_clicked <- (
            (rowSums(bird_dat2[, bird_lookup$V6 %in% input$taxamap, drop=F]) > 0  | dat$taxa %in% input$taxamap) &
                (rowSums(matrix_hits[, colnames(matrix_hits) %in% input$matrixmap, drop=F]) > 0) &
                (dat$forest_class %in% input$forestmap) &
                (as.numeric(as.character(dat$firstyear)) > input$firstyearmap)
        )
        
        leaflet(dat[user_clicked,]) %>%
            addTiles(  urlTemplate = "//server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}") %>%
            addCircles(
                data = dat[user_clicked,],
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 1,
                radius = 50000, 
                popup = ~ as.character(label),
                #   label = ~ as.character(label),
                color = "#06A9B2",
                fillOpacity = 0.5
            )
    })
    
   #input <- list(taxanet=c("Community", "Parulidae"), matrixnet=c("Developed", "Agriculture", "Industry", "Other"), forestnet=unique(dat$forest_class), firstyearnet=1970)
    
    output$network <- renderVisNetwork({
        user_clicked <- (
           (rowSums(bird_dat2[, bird_lookup$V6 %in% input$taxanet, drop=F]) > 0  | dat$taxa %in% input$taxanet) &
              (rowSums(matrix_hits[, colnames(matrix_hits) %in% input$matrixnet, drop=F]) > 0) &
              (dat$forest_class %in% input$forestnet) &
              (as.numeric(as.character(dat$firstyear)) >= input$firstyearnet)
        )
        selected <- rowSums(selection_mat[,which(user_clicked)])
        
        edges <- edgedf[selected>0,]
        
        
        #edges$from <- factor(append(edges$from, nodes$id))[1:length(edges$from)]
        #edges$to <- factor(append(edges$to, nodes$id))[1:length(edges$to)]
        edges$degree <- selected[selected>0]
        edges$value <- edges$degree
        
        nodes <- nodedf[(nodedf$id %in% edges$from | nodedf$id %in% edges$to),]
        nodes$label <- nodes$id
        
        tmpgraph <- igraph::strength(igraph::graph.edgelist(cbind(edges$from, edges$to)))
        nodes$value <- tmpgraph[match(nodes$label, names(tmpgraph))]
        nodes$font.size <- ceiling(nodes$value)
        
        nodes$font.size <- ceiling(log(as.numeric(scale(nodes$value, center=F)*20)))*7
        nodes$shape <- rep("dot", nrow(nodes))
        
        
        
        visNetwork(nodes, edges, width = "100%") %>%
          visIgraphLayout() %>%
          visNodes(
            scaling = list(
              min = min(nodes$value)+15,
              max = max(nodes$value)+50, 
              label=list(enabled="true")
            ),
            shadow = list(enabled = F)
          ) %>%
          visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 1)),    
                   scaling = list(
                     min = min(edges$value)+1,
                     max = max(edges$value)+1, 
                     label=list(enabled="true")
                   ),
                   shadow = FALSE,
                   color = list(color = "#06A9B2", highlight = "#06A9B2")
          ) %>%
          visOptions(
            highlightNearest = list(enabled = T, degree = 1, hover = T)
          ) %>%
          visGroups(groupname = "1", color = list(background = palette[2],
                                                  border = palette[1],
                                                  highlight = list(background=palette[2], border=palette[1]),
                                                  hover=list(background=palette[2], border=palette[1]))) %>%
          visLayout(randomSeed = 11)
        
    }
    
    )
    
}


shinyApp(ui = ui, server = server)
