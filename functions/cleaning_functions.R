#Title: Function for cleaning the raw data set
#Date generated: 16/12/2022
#
#
#
#Function to change column names and remove specific columns
clean_data <- function (raw_data){
  raw_data %>%
    clean_names() %>% #Removes capital letters from column names
    remove_empty(c("rows", "cols")) %>% #Removes empty rows and columns
    select(-starts_with("delta"))%>% #Removes columns that start with "delta"
    select(-comments) #Removes columns that start with "comments"
}   
#
#
#
#Function to filter and subset data to only include body mass, beak length and sex for Adelie penguins, removing rows with NA.
filter_data <- function(penguins_clean){
  penguins_clean %>%
    filter(!is.na(body_mass_g),!is.na(culmen_length_mm),!is.na(sex), species == "Adelie Penguin (Pygoscelis adeliae)") %>% #removes rows with NA, only includes rows for which the species is Adelie Penguin.
    select(body_mass_g, culmen_length_mm, sex) #subsets data columns for body mass, beak length and sex
}
#


