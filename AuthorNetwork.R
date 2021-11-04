# Librarires:
library(tidyverse)
library(igraph)
library(tidygraph)


ALEA <- read.csv("ALEA.csv", sep = ";")

#  Get distinct Author1 names
Author1 <- ALEA %>%
  distinct(Author1) %>%
  rename(label = Author1)
# Get distinct Author2 names
Author2 <- ALEA %>%
  distinct(Author2) %>%
  rename(label = Author2)
# Join the two data to create node
# Add unique ID for each country
nodes <- full_join(Author1, Author2, by = "label")
# Gets nodes for edges:
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
head(nodes, 3)

# Rename the Frequency column to weight
ALEA <- ALEA %>%
  rename(weight = Frequency)
# (a) Join nodes id for Author1 column
edges <- ALEA %>% 
  left_join(nodes, by = c("Author1" = "label")) %>% 
  rename(from = id)
# (b) Join nodes id for Author2 column
edges <- edges %>% 
  left_join(nodes, by = c("Author2" = "label")) %>% 
  rename(to = id)
# (c) Select/keep only the columns from and to
edges <- select(edges, from, to, weight)
head(edges, 3)

# Create an igraph network object:
net.igraph <- graph_from_data_frame(
  d = edges, vertices = nodes, 
  directed = FALSE
)

# Create a network graph with igraph:
set.seed(123)
plot(net.igraph, edge.arrow.size = 0.2,
     layout = layout_with_graphopt)

# Create a network object using tidygraph:
net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = FALSE
)

# Visualize network using ggraph:
library(ggraph)
ggraph(net.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "ALEA") +
  theme_graph()


BJPS <- read.csv("BJPS.csv", sep = ";")

#  Get distinct Author1 names
Author1 <- BJPS %>%
  distinct(Author1) %>%
  rename(label = Author1)
# Get distinct Author2 names
Author2 <- BJPS %>%
  distinct(Author2) %>%
  rename(label = Author2)
# Join the two data to create node
# Add unique ID for each country
nodes <- full_join(Author1, Author2, by = "label") 
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
head(nodes, 3)

# Rename the Frequency column to weight
BJPS <- BJPS %>%
  rename(weight = Frequency)
# (a) Join nodes id for Author1 column
edges <- BJPS %>% 
  left_join(nodes, by = c("Author1" = "label")) %>% 
  rename(from = id)
# (b) Join nodes id for Author2 column
edges <- edges %>% 
  left_join(nodes, by = c("Author2" = "label")) %>% 
  rename(to = id)
# (c) Select/keep only the columns from and to
edges <- select(edges, from, to, weight)
head(edges, 3)

# Create an igraph network object:
net.igraph <- graph_from_data_frame(
  d = edges, vertices = nodes, 
  directed = FALSE
)

# Create a network graph with igraph:
set.seed(123)
plot(net.igraph, edge.arrow.size = 0.2,
     layout = layout_with_graphopt)

# Create a network object using tidygraph:
net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = FALSE
)

# Visualize network using ggraph:
library(ggraph)
ggraph(net.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "BJPS") +
  theme_graph()


ChJS <- read.csv("ChJS.csv", sep = ";")

#  Get distinct Author1 names
Author1 <- ChJS %>%
  distinct(Author1) %>%
  rename(label = Author1)
# Get distinct Author2 names
Author2 <- ChJS %>%
  distinct(Author2) %>%
  rename(label = Author2)
# Join the two data to create node
# Add unique ID for each country
nodes <- full_join(Author1, Author2, by = "label") 
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
head(nodes, 3)

# Rename the Frequency column to weight
ChJS <- ChJS %>%
  rename(weight = Frequency)
# (a) Join nodes id for Author1 column
edges <- ChJS %>% 
  left_join(nodes, by = c("Author1" = "label")) %>% 
  rename(from = id)
# (b) Join nodes id for Author2 column
edges <- edges %>% 
  left_join(nodes, by = c("Author2" = "label")) %>% 
  rename(to = id)
# (c) Select/keep only the columns from and to
edges <- select(edges, from, to, weight)
head(edges, 3)

# Create an igraph network object:
net.igraph <- graph_from_data_frame(
  d = edges, vertices = nodes, 
  directed = FALSE
)

# Create a network graph with igraph:
set.seed(123)
plot(net.igraph, edge.arrow.size = 0.2,
     layout = layout_with_graphopt)

# Create a network object using tidygraph:
net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = FALSE
)

# Visualize network using ggraph:
library(ggraph)
ggraph(net.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "ChJS") +
  theme_graph()


RCE <- read.csv("RCE.csv", sep = ";")

#  Get distinct Author1 names
Author1 <- RCE %>%
  distinct(Author1) %>%
  rename(label = Author1)
# Get distinct Author2 names
Author2 <- RCE %>%
  distinct(Author2) %>%
  rename(label = Author2)
# Join the two data to create node
# Add unique ID for each country
nodes <- full_join(Author1, Author2, by = "label") 
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
head(nodes, 3)

# Rename the Frequency column to weight
RCE <- RCE %>%
  rename(weight = Frequency)
# (a) Join nodes id for Author1 column
edges <- RCE %>% 
  left_join(nodes, by = c("Author1" = "label")) %>% 
  rename(from = id)
# (b) Join nodes id for Author2 column
edges <- edges %>% 
  left_join(nodes, by = c("Author2" = "label")) %>% 
  rename(to = id)
# (c) Select/keep only the columns from and to
edges <- select(edges, from, to, weight)
head(edges, 3)

# Create an igraph network object:
net.igraph <- graph_from_data_frame(
  d = edges, vertices = nodes, 
  directed = FALSE
)

# Create a network graph with igraph:
set.seed(123)
plot(net.igraph, edge.arrow.size = 0.2,
     layout = layout_with_graphopt)

# Create a network object using tidygraph:
net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = FALSE
)

# Visualize network using ggraph:
library(ggraph)
ggraph(net.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "RCE") +
  theme_graph()


RIO <- read.csv("RIO.csv", sep = ";")

#  Get distinct Author1 names
Author1 <- RIO %>%
  distinct(Author1) %>%
  rename(label = Author1)
# Get distinct Author2 names
Author2 <- RIO %>%
  distinct(Author2) %>%
  rename(label = Author2)
# Join the two data to create node
# Add unique ID for each country
nodes <- full_join(Author1, Author2, by = "label") 
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
head(nodes, 3)

# Rename the Frequency column to weight
RIO <- RIO %>%
  rename(weight = Frequency)
# (a) Join nodes id for Author1 column
edges <- RIO %>% 
  left_join(nodes, by = c("Author1" = "label")) %>% 
  rename(from = id)
# (b) Join nodes id for Author2 column
edges <- edges %>% 
  left_join(nodes, by = c("Author2" = "label")) %>% 
  rename(to = id)
# (c) Select/keep only the columns from and to
edges <- select(edges, from, to, weight)
head(edges, 3)

# Create an igraph network object:
net.igraph <- graph_from_data_frame(
  d = edges, vertices = nodes, 
  directed = FALSE
)

# Create a network graph with igraph:
set.seed(123)
plot(net.igraph, edge.arrow.size = 0.2,
     layout = layout_with_graphopt)

# Create a network object using tidygraph:
net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = FALSE
)

# Visualize network using ggraph:
library(ggraph)
ggraph(net.tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "RIO") +
  theme_graph()
