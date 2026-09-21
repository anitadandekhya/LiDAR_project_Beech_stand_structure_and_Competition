# load the required packages
library(tidyverse)
library(lidR)
library(TreeLS)
library(patchwork)

# define folders
hood_dir <- "S:/EAGLE_Academic/LiDAR/Site_9/neighbourhood"
cut_dir  <- "S:/EAGLE_Academic/LiDAR/Site_9/trees"
output_dir <- "S:/EAGLE_Academic/LiDAR_finalproject/Results"

# create the output folder
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# find all plots las files
hood_files <- list.files(hood_dir,pattern = "\\.las$",
                      recursive = TRUE,
                      full.names = TRUE)

#check the data on point cloud
hood1 <- readLAS("S:/EAGLE_Academic/LiDAR/Site_9/trees/9_Tree1_cut.las")
hood11@header
hood1@data

# create empty list for all plots
all_inventory <- list()

# process each neighbourhood
for (i in seq_along(hood_files)) {
  file <- hood_files[i]
 
  #read the point clouds
  tls <- readTLS(file)
 
  # thin point clouds
  thin <- tlsSample(tls, smp.voxelize(0.01))
 
  # detect/map trees
  map <- treeMap( thin, map.hough(min_density = 0.1), 0)
 
  # extract points belonging to detected trees
  tls_t <- treePoints(tls, map, trp.crop())
 
  # detect stems
  tls_stem <- stemPoints(tls_t, stm.hough())
 
  # calculate tree inventory
  inv <- tlsInventory(
    tls_stem,
    d_method = shapeFit(shape = "circle",algorithm = "ransac"))
  
  # calculate dbh
  inv$dbh <- inv$Radius * 2
  
  # add neighborhood name
  inv$Plot <- tools::file_path_sans_ext(basename(file))

 # store inventory
  all_inventory[[i]] <- inv

  # save individual inventory
  write.csv(inv,
    file.path(output_dir,
              paste0(tools::file_path_sans_ext(basename(file)),
                     "_inventory.csv")),
    row.names = FALSE)
  
  # remove large objects before next plot
  rm(tls, thin, map, tls_t, tls_stem, inv)
  gc()
}

# combine all neighborhood inventory
inventory_all <- bind_rows(
  all_inventory) %>%
  mutate(
    Plot_ID = Plot,
    Plot_name = as.numeric(sub("9_Tree([0-9]+)_Hood", "\\1", Plot)),
    Plot = paste("Plot", Plot_name),
    dbh_cm = dbh * 100)
  

# Save combined inventory
write.csv(
  inventory_all,
  file.path(
    output_dir,
    "all_neighbourhood_inventory.csv"
  ),
  row.names = FALSE
)


# look at the result
head(inventory_all)

# calculate stem density of each neighbourhood
plot_area_m2 <- 30 * 30
plot_area_ha <- plot_area_m2 / 10000
stem_density <- inventory_all %>%
  group_by(Plot_name) %>%
  summarise(
    Number_of_stems = n(),
    Stem_density_ha = Number_of_stems / plot_area_ha)

print(stem_density)

# compare number of stems in each plot
numberofstems_plot <- stem_density %>% 
  ggplot(aes(x = reorder(factor(Plot_name), -Number_of_stems),
              y = Number_of_stems)) + 
  geom_col(width = 0.65, fill = "#F4A261") + 
  labs(title = "Number of stems in each plot", 
        subtitle = "Beech stand structure derived from mobile laser scanning",
        x = "Plot", 
        y = "Number of stems") + 
  theme_minimal() + 
  theme( 
    plot.title = element_text( size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text (size =12, hjust = 0.5),
    axis.text.x = element_text( colour = "black", size = 10, hjust = 1 ),
    axis.text.y = element_text( colour="black", size = 10 ),
    panel.border = element_rect( colour = "black", linewidth = 1.2, fill = NA))
#save the plot
ggsave(
  filename = file.path(
    output_dir,
    "Number_stems_per_plot.png"),
  plot = numberofstems_plot,
  width = 10,
  height = 6,
  dpi = 300)

# compare stems per hectare in each plot
stem_density_plot <- stem_density %>% 
  ggplot(aes(x = reorder(factor(Plot_name), -Stem_density_ha),
              y = Stem_density_ha)) + 
  geom_col(width = 0.65, fill = "#4C7A34") + 
  labs(title = "Stem density per plot", 
        subtitle = "Beech stand structure derived from mobile laser scanning",
        x = "Plot", 
        y = "Stem density (stems/ha)") + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text (size =12, hjust = 0.5),
    axis.text.x = element_text(colour = "black", size = 10, hjust = 1 ),
    axis.text.y = element_text(colour="black", size = 10 ),
    panel.border = element_rect(colour = "black", linewidth = 1.2, fill = N))
  
# Save the plot 
ggsave(
  filename = file.path(
    output_dir,
    "Stem_density_per_plot.png"),
  plot = stem_density_plot,
  width = 10,
  height = 6,
  dpi = 300
)

######### dbh distribution######################
inventory_all<- read.csv("S:/EAGLE_Academic/LiDAR/TreeLS_Results/all_neighbourhood_inventory.csv")
# create boxplot und individual trees dbh
dbh_boxplot <- ggplot( inventory_all, aes( x = factor(Plot_name), y = dbh_cm ) ) +
  geom_boxplot( width = 0.55, fill = "#F4A261", colour = "black", linewidth = 0.4, outlier.shape = NA ) +
  geom_jitter( width = 0.10, height = 0, size = 0.8, colour = "black" ) +
  labs(title = "DBH distribution across plots", 
       subtitle = "Beech stand structure derived from mobile laser scanning", 
       x = "Plot", 
       y = "DBH (cm)") +
  scale_y_continuous( expand = expansion( mult = c(0.02, 0.05) ) ) +
  theme_classic() +
  theme(
    plot.title = element_text( size = 18, face = "bold", hjust = 0.5 ),
    plot.subtitle = element_text( size = 12, hjust = 0.5 ),
    axis.title.x = element_text( size = 12, face = "bold", colour = "black" ),
    axis.title.y = element_text( size = 12, face = "bold", colour = "black" ),
    axis.text.x = element_text( size = 10, colour = "black", angle = 45, hjust = 1 ),
    axis.text.y = element_text( size = 10, colour = "black" ),
    axis.line = element_line( colour = "black", linewidth = 0.7 ),
    panel.border = element_rect( colour = "black", linewidth = 1.2, fill = NA),
    plot.margin = margin( 10, 10, 10, 10 ),)

# save the plot
ggsave(filename = file.path(
  output_dir, "DBH_distribution_boxplot_individual_trees.png" ), 
  plot = dbh_boxplot, 
  width = 12, 
  height = 7, 
  dpi = 300)

#individual plot dbh distribution
# dbh range of first 9 plots
first_9_plots <- sort(unique(inventory_all$Plot))[1:9]

dbh_range_first9_each <- inventory_all %>%
  filter(Plot %in% first_9_plots) %>%
  group_by(Plot) %>%
  summarise(
    Minimum_DBH_cm = min(dbh_cm, na.rm = TRUE),
    Maximum_DBH_cm = max(dbh_cm, na.rm = TRUE),
    Mean_DBH_cm = mean(dbh_cm, na.rm = TRUE),
    Number_of_stems = n(),
    .groups = "drop")

dbh_range_first9_each

# dbh range of remaining 9 plots
remaining_plots <- sort(unique(inventory_all$Plot))[10:18]

dbh_range_remaining <- inventory_all %>%
  filter(Plot %in% remaining_plots) %>%
  group_by(Plot) %>%
  summarise(
    Minimum_DBH_cm = min(dbh_cm, na.rm = TRUE),
    Maximum_DBH_cm = max(dbh_cm, na.rm = TRUE),
    Mean_DBH_cm = mean(dbh_cm, na.rm = TRUE),
    Number_of_stems = n(),
    .groups = "drop")

dbh_range_remaining

# group first nine plots
first_9_plots <- c(
  "Plot 1",
  "Plot 2",
  "Plot 3",
  "Plot 4",
  "Plot 5",
  "Plot 6",
  "Plot 7",
  "Plot 8",
  "Plot 9")

# remaining nine plots
remaining_plots <- c(
  "Plot 10",
  "Plot 11",
  "Plot 13",
  "Plot 14",
  "Plot 17",
  "Plot 18",
  "Plot 19",
  "Plot 20",
  "Plot 22")

# dbh class for first 9 plots
dbh_class1 <- c(
  "0–10",
  "10–20",
  "20–30",
  "30–40",
  "40–50",
  "50–60",
  "60–70",
  "70–80",
  "80–90",
  "90–100",
  "100–110",
  "110–120",
  "120–130")

first9_dbh <- inventory_all %>%
  filter(Plot %in% first_9_plots) %>%
  mutate(DBH_class = cut(dbh_cm,
                         breaks = seq(0, 130, by = 10),
                         labels = dbh_class1,
                         right = FALSE,
                         include.lowest = TRUE))

# dbh class for remaining 9 plots
dbh_class2 <- c(
  "0–10",
  "10–20",
  "20–30",
  "30–40",
  "40–50",
  "50–60",
  "60–70",
  "70–80",
  "80–90",
  "90–100")

remaining_dbh <- inventory_all %>%
  filter(Plot %in% remaining_plots) %>%
  mutate(DBH_class = cut(dbh_cm,
                         breaks = seq(0, 100, by = 10),
                         labels = dbh_class2,
                         right = FALSE,
                         include.lowest = TRUE))

# function to create each individual dbh distribution plot
make_dbh_plot <- function(data,plot_name,dbh_classes) 
  {
  plot_data <- data %>%
    filter(Plot == plot_name) %>%
    mutate(DBH_class = factor(DBH_class,levels = dbh_classes))
  
  ggplot(plot_data,aes(x = DBH_class)) +
    geom_bar(fill = "#4DBBD5",colour = "black",width = 1) +
    scale_x_discrete(limits = dbh_classes,drop = FALSE, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0.5)) +
    labs(
      title = plot_name,
      x = "DBH (cm)",
      y = "Number of trees"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12,face = "bold",hjust = 0.5),
      axis.title.x = element_text(size = 9, colour = "black"),
      axis.text.x = element_text(size = 7, colour = "black", angle = 45, hjust = 1),
      axis.title.y = element_text(size = 9, colour = "black"),
      axis.text.y = element_text(size = 7, colour = "black"),
      axis.line = element_line(colour = "black",linewidth = 0.5),
      #panel.grid.major.y = element_line(colour = "grey60", linewidth = 0.25),
      #panel.grid.major.x = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.border = element_rect( colour = "black", linewidth = 1.2, fill = NA ),
      plot.margin = margin(5,5,5,5))
}

# make first 9 plots
plot1 <- make_dbh_plot(first9_dbh, "Plot 1", dbh_class1)
plot2 <- make_dbh_plot(first9_dbh, "Plot 2", dbh_class1)
plot3 <- make_dbh_plot(first9_dbh, "Plot 3", dbh_class1)
plot4 <- make_dbh_plot(first9_dbh, "Plot 4", dbh_class1)
plot5 <- make_dbh_plot(first9_dbh, "Plot 5", dbh_class1)
plot6 <- make_dbh_plot(first9_dbh, "Plot 6", dbh_class1)
plot7 <- make_dbh_plot(first9_dbh, "Plot 7", dbh_class1)
plot8 <- make_dbh_plot(first9_dbh, "Plot 8", dbh_class1)
plot9 <- make_dbh_plot(first9_dbh, "Plot 9", dbh_class1)

# combine all 9 plots
dbh_first9_figure <- (plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 + plot9) +
  plot_layout(ncol = 3)

dbh_first9_figure

# save first 9 plots
ggsave(
  filename = file.path(
    output_dir, "DBH_distribution_Plots_1_to_9_final.png"),
  plot = dbh_first9_figure,
  width = 12,
  height = 10,
  dpi = 300)

# make the remaining 9 plots
plot10 <- make_dbh_plot(remaining_dbh, "Plot 10", dbh_class2)
plot11 <- make_dbh_plot(remaining_dbh, "Plot 11", dbh_class2)
plot13 <- make_dbh_plot(remaining_dbh, "Plot 13", dbh_class2)
plot14 <- make_dbh_plot(remaining_dbh, "Plot 14", dbh_class2)
plot17 <- make_dbh_plot(remaining_dbh, "Plot 17", dbh_class2)
plot18 <- make_dbh_plot(remaining_dbh, "Plot 18", dbh_class2)
plot19 <- make_dbh_plot(remaining_dbh, "Plot 19", dbh_class2)
plot20 <- make_dbh_plot(remaining_dbh, "Plot 20", dbh_class2)
plot22 <- make_dbh_plot(remaining_dbh, "Plot 22", dbh_class2)

# combine these remaining plots
dbh_remaining9_figure <- (plot10 + plot11 + plot13 + plot14 + plot17 + plot18 + plot19 + plot20 + plot22) +
  plot_layout(ncol = 3)

dbh_remaining9_figure

# save the graph
ggsave(
  filename = file.path(
    output_dir, "DBH_distribution_Plots_10_to_22.png"),
  plot = dbh_remaining9_figure,
  width = 12,
  height = 10,
  dpi = 300
)











