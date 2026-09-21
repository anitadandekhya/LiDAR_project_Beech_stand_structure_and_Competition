#install the packages
install.packages("Rfast", type = "binary")
devtools::install_github("juliarieder/TreeCompR", dependencies = TRUE)

#load the packages
library(devtools)
library(tidyverse)
library(TreeCompR)
library(rgl)

# Competition Indices with TreeCompR
# define the path
hood_dir <- "S:/EAGLE_Academic/LiDAR/Site_9/neighbourhood"
tree_dir  <- "S:/EAGLE_Academic/LiDAR/Site_9/trees"
output_dir <- "S:/EAGLE_Academic/LiDAR_finalproject/Results"

# List .las files in the neighborhood and tree folders
hood_files <- list.files(hood_dir,
                         full.names = TRUE, 
                         pattern = "\\.las$")
tree_files <- list.files(tree_dir,
                         full.names = TRUE, 
                         pattern = "\\.las$")


# Create TreeIDs by stripping the file extension from the tree files
TreeIDs <- gsub("\\.las", "", list.files(tree_dir,
                                         full.names = FALSE, 
                                         pattern = "\\.las$"))

# Create the lookup table to match target tree with corresponding neighborhood
lookup_table <- tibble(
  tree_name = TreeIDs,
  forest_source = hood_files,
  tree_source = tree_files)

# define additional parameter settings for compete_pc within the lookup table
lookup_cone50_cyl5 <- lookup_table %>% 
  mutate(comp_method = "both", 
         center_position = "crown_pos", 
         cyl_r = 5, h_cone = 0.5, 
         z_min = 100, h_xy = 0.3, 
         print_progress = "some")

# use pmap to loop over the table
results <- lookup_cone50_cyl5 %>% 
  pmap(compete_pc)
names(results)

# bind the results of all trees into a single dataset
results_table <- bind_rows(results)

results_df <- results_table %>%
  select(target, height_target, CI_cone, CI_cyl ) %>%
  rename(Tree = target,
         Tree_height = height_target,
         Cone_CI = CI_cone,
         Cylinder_CI = CI_cyl)

results_df <- results_df %>%
  mutate(Tree_number = as.numeric(
    gsub(".*Tree([0-9]+).*", "\\1", Tree)),
    Tree_label = paste("Tree", Tree_number) ) %>%
  arrange(Tree_number)

results_df
results_df <- read.csv("S:/EAGLE_Academic/LiDAR/TreeLS_Results/TreeCompR_Competition.csv")
#save the results
write.csv(
  results_df,
  file.path(
    output_dir,
    "TreeCompR_Competition.csv"
  ),
  row.names = FALSE
)

# Cone and Competition Indices of all target trees
competition_long <- results_df %>%
  select(Tree_label,
         Tree_number,
         Cone_CI,
         Cylinder_CI) %>%
  pivot_longer(cols = c(Cone_CI, Cylinder_CI),
               names_to = "Method",
               values_to = "Competition_Index" ) %>%
  mutate(Method = recode(
      Method,
      Cone_CI = "Cone-based CI",
      Cylinder_CI = "Cylinder-based CI"))

# Keep the original tree numbering, including gaps
competition_long$Tree_label <- factor(
  competition_long$Tree_label,
  levels = results_df$Tree_label)

# Plot
p1 <- ggplot(competition_long,aes(x = Tree_label,
                                  y = Competition_Index,
                                  fill = Method)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  labs(
    title = "Cone and cylinder-based competition indices",
    subtitle = "TreeCompR competition derived from point clouds",
    x = "Target tree",
    y = "Competition index (number of competing voxels)",
    fill = "Competition method") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold",size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    #egend.position = "top", 
    legend.title = element_blank(),
    legend.position = c(0.98, 0.98), 
    legend.justification = c(1, 1), 
    legend.direction = "vertical",
    panel.border = element_rect( colour = "black", linewidth = 1.2, fill = NA ))

#save the plot
ggsave(filename = file.path(output_dir,"Competition_Index_Cone_Cylinderfinal.png"),
       plot = p1,
       width = 12,
       height = 7,
       dpi = 300)


# calculate correlation between cone and cylinder competition Indices
correlation <- cor(results_df$Cone_CI,
                   results_df$Cylinder_CI,
                   method = "pearson",
                   use = "complete.obs")

# Linear regression model
model <- lm(Cylinder_CI ~ Cone_CI, data = results_df)

# R-squared
r_squared <- summary(model)$r.squared
r_squared
# create text for the graph
correlation_text <- paste0("Pearson r = ", 
                            round(correlation, 3),
                            "\nR² = ", round(r_squared,3))
# Scatter plot with regression line 
 p3 <- ggplot(results_df, aes(x = Cone_CI,
                                y = Cylinder_CI)) + 
   geom_point(size = 4) + 
   geom_text(aes(label = Tree_number), nudge_y = 3000, size = 4) +
   geom_smooth(method = "lm", se = TRUE) + 
   annotate("text", 
             x = Inf, 
             y = -Inf, 
             label = correlation_text, 
             hjust = 1.1, 
             vjust = -0.5, 
             size = 5) + 
   labs(title = "Cone-based vs Cylinder-based Competition", 
         subtitle = "TreeCompR competition indices", 
         x = "Cone-based competition index", 
         y = "Cylinder-based competition index") + 
   theme_minimal(base_size = 14 ) + 
   theme(plot.title = element_text( face = "bold", size = 17, hjust = 0.5), 
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.text.x = element_text(colour = "black", size = 10, hjust = 1),
          axis.text.y = element_text(colour="black", size = 10),
          panel.border = element_rect(colour = "black", linewidth = 1.2, fill = NA))
 

 # save the graph 
 ggsave(filename = file.path(output_dir, 
                               "Cone_vs_Cylinder_Competition_R2.png"), 
         plot = p3, 
         width = 9, 
         height = 7, 
         dpi = 300)

# summary statistics
summary_competition <- results_df %>%
  summarise(n_trees = n(),
  Mean_height = mean(Tree_height, na.rm = TRUE),
  SD_height = sd(Tree_height, na.rm = TRUE),
  Mean_Cone = mean(Cone_CI, na.rm = TRUE),
  SD_Cone = sd(Cone_CI, na.rm = TRUE),
  Min_Cone = min(Cone_CI, na.rm = TRUE),
  Max_Cone = max(Cone_CI, na.rm = TRUE),
  Mean_Cylinder = mean(Cylinder_CI, na.rm = TRUE),
  SD_Cylinder = sd(Cylinder_CI, na.rm = TRUE),
  Min_Cylinder = min(
  Cylinder_CI, na.rm = TRUE),
  Max_Cylinder = max(Cylinder_CI, na.rm = TRUE))

summary_competition

# save csv
write.csv(summary_competition, 
          file.path(output_dir,"Competition_Summary_Statistics.csv"),
          row.names = FALSE)

# create TreeCompR 3D competition visualization for tree 8
tree_id <- 8

# target tree 8 and its neighbourhood files path 
tree_file <- file.path(tree_dir, 
                       paste0("9_Tree", tree_id, "_cut.las"))


hood_file <- file.path(hood_dir,
                       paste0("9_Tree",tree_id,"_Hood.las"))

# read target tree 8 and its neighbourhood
target_tree <- readLAS(tree_file, select = "xyz")

neighbourhood <- readLAS(hood_file, select = "xyz")

# convert to dataframes
# Target tree
target_xyz <- as.data.frame(target_tree@data[, c("X", "Y", "Z")])

# Neighbourhood
hood_xyz <- as.data.frame(neighbourhood@data[, c("X", "Y", "Z")])

# Tree height
tree_height <- max(target_xyz$Z, na.rm = TRUE)
tree_min_z <- min(target_xyz$Z, na.rm = TRUE)

# get actual TreeCompR crown position
tree_pc <- read_pc(pc_source = tree_file)
tree_position <- tree_pos(tree_pc)

# Actual crown position calculated by TreeCompR
crown_x <- unname(tree_position$crown_pos["x"])
crown_y <- unname(tree_position$crown_pos["y"])
crown_z <- unname(tree_position$crown_pos["z"])

# TreeCompR Parameters
h_cone <- 0.5
cyl_r <- 5

# calculate cone opening height at 50% of tree height
cone_opening_z <- tree_min_z + h_cone * (tree_height - tree_min_z)

# identify points inside the 5m cylinder
hood_xyz <- hood_xyz %>% 
  mutate(distance_xy = sqrt( (X - crown_x)^2 + 
                                (Y - crown_y)^2)) 
competing_xyz <- hood_xyz %>% 
  filter(distance_xy <= cyl_r)

# get Competition indices results for tree 8
tree_result <- results_df %>% 
  filter(Tree_number == tree_id)

# extract competition values
cone_value <- tree_result$Cone_CI[1]
cylinder_value <- tree_result$Cylinder_CI[1]
tree_height_result <- tree_result$Tree_height[1]

# prepare number of points for visualization
set.seed(123)

n_target <- min(100000, nrow(target_xyz))
n_hood <- min(200000, nrow(hood_xyz))

# Random sampling
target_plot <- target_xyz[
  sample(seq_len(nrow(target_xyz)),n_target),]

hood_plot <- hood_xyz[
  sample(seq_len(nrow(hood_xyz)),n_hood),]

# open 3D window
open3d() 
bg3d(color = "white")

# plot the neighbourhood
points3d(hood_plot$X, 
          hood_plot$Y, 
          hood_plot$Z, 
          size = 1, 
          col = "grey75")

# plot the target tree
points3d(target_plot$X, 
          target_plot$Y, 
          target_plot$Z, 
          size = 3, 
          col = "red")

# plot competing points
points3d(competing_xyz$X, 
          competing_xyz$Y, 
          competing_xyz$Z, 
          size = 2, 
          col = "blue")

# show TreeCompR crown position
points3d(crown_x, 
          crown_y, 
          crown_z, 
          size = 10, 
          col = "black")

# draw 5m radius cylinder
theta <- seq(0, 2 * pi, length.out = 100)
z_bottom <- tree_min_z
z_top <- tree_height
x_cyl <- crown_x + cyl_r * cos(theta)
y_cyl <- crown_y + cyl_r * sin(theta)

#vertical cylinderlines
#for (i in seq_along(theta)) { 
  #segments3d(x = c(x_cyl[i], x_cyl[i]), 
             # y = c(y_cyl[i], y_cyl[i]), 
              #z = c(z_bottom, z_top), 
              #col = "blue" ) }

# cylinder bottom
lines3d(x_cyl, 
         y_cyl, 
         rep( z_bottom, length(theta)), 
         col = "blue" )

# cylinder top
lines3d(x_cyl, y_cyl, rep(z_top, length(theta)), col = "blue" )

# draw cone opening at 50%
theta_cone <- seq(0, 2 * pi, length.out = 80)
cone_z <- seq(cone_opening_z, tree_height, length.out = 30)

#draw cone rings
for (i in seq_along(cone_z)) { 
  # Relative vertical position 
  relative_height <- (cone_z[i] - cone_opening_z) / (tree_height - cone_opening_z)
  # Radius increases toward the tree top 
  radius_i <- cyl_r * relative_height
  x_ring <- crown_x + radius_i * cos(theta_cone)
  y_ring <- crown_y + radius_i * sin(theta_cone)
  lines3d( x_ring, 
           y_ring, 
           rep( cone_z[i], length(theta_cone) ), 
           col = "green" ) }

# draw cone side lines
x_cone_top <- crown_x + cyl_r * cos(theta_cone) 
y_cone_top <- crown_y + cyl_r * sin(theta_cone) 
for (i in seq_along(theta_cone)) { 
  segments3d(x = c(crown_x, x_cone_top[i]), 
              y = c(crown_y, y_cone_top[i]), 
              z = c(cone_opening_z, tree_height), 
              col = "green")}

# title
title3d(main = paste0( "Tree", tree_id, "- TreeCompR competition"), 
         xlab = "X", 
         ylab = "Y", 
         zlab = "Height (m)")

#add legend
legend3d("topright", 
          legend = c("Neighbourhood", 
                      "Target tree", 
                      "Competing points", 
                      "Crown position", 
                      "Cylinder r = 5 m", 
                      "Cone h_cone = 0.5"), 
          col = c("grey75", "red", "blue", "black", "blue", "green"),
          pch = c(16, 16, 16, 16, NA, NA), 
          lwd = c(NA, NA, NA, NA, 2, 2), 
          cex = 0.6)


# Set final camera view
rgl::view3d(theta = 180, phi = 90, zoom = 0.75)

# Save 3D visualization
rgl::rgl.snapshot(
  filename = file.path(
    output_dir,
    paste0("TreeCompR_3D_Tree", tree_id, ".png")
  ),
  fmt = "png"
)

