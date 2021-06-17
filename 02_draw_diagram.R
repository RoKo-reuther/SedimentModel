
# create empty data frames
nodes <- data.frame(id=c(), label=c(), group=c())
links <- data.frame(from=c(), to=c())

# fill data frames
# go through occuring_reactions
for (reaction in occuring_reactions){
# create and attach "reaction node"
  new_node <- data.frame(id=c(reaction$abbreviation), label=c(reaction$name), group=c("reaction"))
  nodes <- rbind(nodes, new_node)
  
# create and attach nodes for species and links between species- and reaction-nodes
  for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
    # new "species-node", if it does not exist yet
    if (any(nodes==species)==FALSE){
      new_node <- data.frame(id=c(species), label=c(species), group=c("species"))
      nodes <- rbind(nodes, new_node)
    }
    # new link(s)
    # check if it is a reversible reaction
    if (reaction$reversibel==TRUE){
      # create links in both directions
      new_link <- data.frame(from=c(species, reaction$abbreviation), to=c(reaction$abbreviation, species))
      links <- rbind(links, new_link)
    }
    else {
      # check if current species is educt
      if (exists(species, reaction$involved_species$educts)){
        # create new link: species -> reaction
        new_link <- data.frame(from=c(species), to=c(reaction$abbreviation))
        links <- rbind(links, new_link)
      }
      else{
        # create link: reaction -> species
        new_link <- data.frame(from=c(reaction$abbreviation), to=c(species))
        links <- rbind(links, new_link)
      }
    }
  }
}

###########################################################################################################

library("visNetwork")

# create visNetwork object
model_diagram <- visNetwork(nodes, links, width = 1920, height = 1080)

# set diagram layout (igraph layout)
model_diagram <- visIgraphLayout(model_diagram, layout = "layout_on_grid") # try: _with_kk; _with graphopt; with_fr etc.

# global node customisation
model_diagram <- visNodes(model_diagram,
                          shape = "box",
                          borderWidth = 3)

# node customisation per group
# reactions
model_diagram <- visGroups(model_diagram, groupname = "reaction",       
                           color = list(background = "lightblue", border="royalblue"),
                           shapeProperties = list(borderDashes = c(10,5)))
# species
model_diagram <- visGroups(model_diagram, groupname = "species",       
                           color = list(background = "white", border="royalblue"),
                           shapeProperties = list(borderDashes = FALSE))

# global links customisation
model_diagram <- visEdges(model_diagram,
                          arrows = "to",
                          smooth = TRUE) #list(enabled = TRUE, type = "diagonalCross"))

# clear workspace: remove auxiliary variables
rm(reaction, species, new_node, new_link, nodes, links)
