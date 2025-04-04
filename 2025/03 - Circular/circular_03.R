# load packages

library(tidyverse)
library(here)
library(circlize)

# data wrangling

# data from https://www.tandfonline.com/doi/full/10.1080/09692290.2024.2365757?scroll=top&needAccess=true#abstract

df <- read_csv(here("data_rikap.csv")) |>
  pivot_longer(everything(),
               names_to = "company",
               values_to = "university") |>
  arrange(company) |>
  na.omit() |>
  group_by(company, university) |>
  count()

df <- df |>
  mutate(university = str_replace(university, "ASIT Japan", "AIST")) |> 
  mutate(university = str_replace(university, "CMU", "Carnegie Mellon University")) |>
  mutate(university = str_replace(university, "Mila", "MILA")) |>
  mutate(university = str_replace(university, "University of Münster", "University of Munster" )) |>
  mutate(country = case_when(
    university == "Caltech" ~ "USA",
    university == "Carnegie Mellon University" ~ "USA",
    university == "Heidelberg University" ~ "Germany",
    university == "Imperial College" ~ "UK",
    university == "Ohio State University" ~ "USA",
    university == "Rutgers University" ~ "USA",
    university == "University College London" ~ "UK",                    
    university == "University of California" ~ "USA",                    
    university == "University of Edinburgh" ~ "UK",                     
    university == "University of Southern California" ~ "USA",
    university == "University of Texas" ~ "USA",                          
    university == "University of Washington" ~ "USA",                    
    university == "University of Wisconsin-Madison" ~ "USA",              
    university == "Georgia Tech" ~ "USA",                                 
    university == "Harvard" ~ "USA",                                      
    university == "ICREA" ~ "Spain",                                       
    university == "INRIA" ~ "France",                                        
    university == "Johns Hopkins University" ~ "USA",                     
    university == "McGill University" ~ "Canada",                            
    university == "New York University" ~ "USA",                          
    university == "Sorbonne Universite" ~ "France",                        
    university == "Stanford University" ~ "USA",                          
    university == "Tel Aviv University" ~ "Israel",                          
    university == "Texas A&M University" ~ "USA",                         
    university == "Universite Le Mans" ~ "France",                          
    university == "University of Michigan" ~ "USA",                       
    university == "AIST" ~ "Japan",                                   
    university == "Australian National University" ~ "Australia",               
    university == "Bar Ilan University" ~ "Israel",                          
    university == "Brown University" ~ "USA",
    university == "Columbia University" ~ "USA",                           
    university == "Cornell University" ~ "USA",                         
    university == "ETH Zurich" ~ "Switzerland",                                    
    university == "Hebrew University" ~ "Israel",                            
    university == "INSEE" ~ "France",                                      
    university == "MIT" ~ "USA",                                          
    university == "Mines ParisTech" ~ "France",                              
    university == "Princeton University" ~ "USA",                          
    university == "TTS Research" ~ "USA",                                 
    university == "Technion-Israel Institute of Technology" ~ "Israel",       
    university == "University of Alberta" ~ "Canada",                        
    university == "University of Colorado" ~ "USA",                       
    university == "University of Minnesota" ~ "USA",                      
    university == "University of Oxford" ~ "UK",                         
    university == "University of South California" ~ "USA",               
    university == "University of Warsaw" ~ "Poland",                          
    university == "Aalto University" ~ "Finland",                             
    university == "Alan Turing Institute" ~ "UK",                        
    university == "Cambridge University" ~ "UK",                         
    university == "China Sun Yat-Sen University" ~ "China",                  
    university == "Chinese Academy of Sciences" ~ "China",                  
    university == "Harbin Institute of Technology" ~ "China",                
    university == "Hefei University of Technology Beijing" ~ "China",       
    university == "Hong Kong Polytechnic University" ~ "China",              
    university == "Indian Institute of Science" ~ "India",                  
    university == "MILA" ~ "Canada",                                          
    university == "Polytechnique Montreal" ~ "Canada",                       
    university == "Shanghai Jiao Tong Unviersity" ~ "China",                
    university == "South China University of Technology" ~ "China",         
    university == "Tsinghua University" ~ "China",                           
    university == "Universite de Montreal" ~ "Canada",                       
    university == "University of Illinois" ~ "USA",                        
    university == "University of Maryland" ~ "USA",                       
    university == "University of Massachusetts" ~ "USA",                   
    university == "University of Munster" ~ "Germany",                  
    university == "University of Science and Technology of China" ~ "China", 
    university == "University of Trento" ~ "Italy",                         
    university == "Weizmann Institute" ~ "Israel"                           
  )) 

df_chord <- df |>
  group_by(company, country) |>
  summarise(n = sum(n)) |>
  ungroup() 

# circular plot 

chosen_order = c(Amazon = "#cdb4db",Facebook = "#ffc8dd",Google = "#ffafcc",Microsoft = "#bde0fe",
                 "Italy","Spain","Japan","UK",
                 "Finland","USA","Australia","Canada",
                 "Switzerland","France","Poland","Israel",
                 "Germany","China","India")

circos.clear()

circos.par("track.height" = 0.1,
           points.overflow.warning = FALSE,
           start.degree = 245,
           gap.degree = c(rep(2,3), 40, rep(2,14), 40),
           circle.margin = 0.5)

png(file = "circualr_03.png", width = 30*600, height = 30*600, res = 300)

par(
  cex = 8, 
  mar = c(0, 0, 5, 0),
  bg = "grey90"
)

chordDiagram(df_chord,
             grid.col = chosen_order,
             annotationTrack = "grid",
             annotationTrackHeight = c(0.1, 0.1))

circos.track(track.index = 1,
             bg.border = NA,
             panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim")
               sector.index = get.cell.meta.data("sector.index")
               
               circos.text(x = mean(xlim), 
                           y = 1.25,
                           labels = sector.index,
                           facing = "clockwise", 
                           niceFacing = TRUE,
                           cex = 0.9,
                           adj = c(0,0.5))
               
               circos.text(x = mean(xlim),
                           y = 0.7,
                           labels = paste0(" ", xlim[2]),
                           facing = "inside",
                           cex = 0.75,
                           adj = c(0.7,1))
             })

dev.off()



