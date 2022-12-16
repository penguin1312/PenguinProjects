#Title: Plotting functions
#Date generated: 16/12/2022
#
#
#
#Function to plot beak length against body mass and sex as a scatter plot, overlaid with regression lines
plot_culmen_figure <- function(penguins_culmen_length){
  penguins_culmen_length %>%
    ggplot(aes(x =body_mass_g, y=culmen_length_mm, colour = sex))+ #assigning colour to sex in aes() applies colour of plot points based on sex 
    geom_point(alpha = 0.2)+ #generates slightly transparent scatter plot to increase visibility of regression lines
    geom_smooth(method = "lm")+ #plots regression lines
    scale_color_manual(labels = c("Female", "Male"), values = c("orange","purple"))+ #colour assigned to each sex
    labs (colour = "Legend", x ="Body mass (g)", y = "Culmen length (mm)", title = "Scatter plot: linear model of culmen length against body mass, \n in different sexes of Adelie penguins", subtitle = "Regression lines for each sex calculated with 95% confidence intervals")+ #specify labels and axis titles 
    theme_bw()+ #black and white theme to increase contrast
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #aligns title and subtitle
}
#
#
#
#Function to save the figure as a vector (svg) with size and scaling defined, does not require a resolution to be specified
  save_culmen_figure <- function(penguins_culmen_length, filename, size, res, scaling){
    size_inches = size/2.54
    svglite(filename, width = size_inches,
            height = size_inches,
            scaling = scaling)
    culmen_length_plot <-plot_culmen_figure(penguins_culmen_length)
    print(culmen_length_plot)
    dev.off()
}
#
#
#