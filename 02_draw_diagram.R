
# two data-frames ("nodes" and "links") are created, based on the "occuring_reactions"-list
# these data-frames are needed to draw a diagram using the visNetwork-package

# create empty data frames
nodes <- data.frame(id=c(), label=c(), title=c(), group=c())
links <- data.frame(from=c(), to=c())

# fill data frames
# go through occuring_reactions
for (reaction in occuring_reactions){
# create and attach "reaction node"
  new_node <- data.frame(id=c(reaction$abbreviation), label=c(""), title=c(reaction$name), group=c("reaction"))
  nodes <- rbind(nodes, new_node)
  
# create and attach nodes for species and links between species- and reaction-nodes
  for (species in c(reaction$involved_species$educts, reaction$involved_species$products)){
    # new "species-node", if it does not exist yet
    if (any(nodes==species)==FALSE){
      new_node <- data.frame(id=c(species), label=c(species), title=c(species), group=c("species"))
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
model_diagram <- visNetwork(nodes, links)

# set some general options
model_diagram <- visOptions(model_diagram,
                           width = 1920, height = 1080,
                           highlightNearest = list(enabled = TRUE, degree = list(from = 2, to = 2), algorithm = "hierarchical"),
                           selectedBy = list(variable = "title", highlight = TRUE)
                           )

# set diagram layout (igraph layout)
# there are many options, e.g.: layout_nicely; _with_dh; with_gem; _as_tree (if you have a "non circled" setup); _with_lgl; merge_coords; normalize
model_diagram <- visIgraphLayout(model_diagram, layout = "layout_with_dh")

# node customisation per group
# reactions
model_diagram <- visGroups(model_diagram, groupname = "reaction",
                           shape = "dot",
                           size = 5,
                           color = list(background = "lightblue", border="lightblue", highlight = list(background = "orange", border = "orange")),
                           shapeProperties = list(borderDashes = c(10,5)),
                           hidden=FALSE)
# species
model_diagram <- visGroups(model_diagram, groupname = "species",
                           shape = "box",
                           color = list(background = "white", border="royalblue", highlight = list(background = "orange", border = "darkred")),
                           shapeProperties = list(borderDashes = FALSE))

# global links customisation
model_diagram <- visEdges(model_diagram,
                          arrows = "to",
                          color = list(color = "royalblue", highlight = "darkred"),
                          smooth = TRUE)

# clear workspace: remove auxiliary variables
rm(reaction, species, new_node, new_link, nodes, links)
