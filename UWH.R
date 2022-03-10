# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyverse)
# pacman: for loading/unloading packages
# party: for decision trees
# rio: for importing data
# tidyverse: for so many reasons
# Clear environment
rm(list = ls()) 

# Import other formats with rio::import() from rio
(df <- import("data/hervoiceuwh.xlsx") %>% as_tibble())

# Sets default value


#recode

df2 <- df %>%
  mutate(b11 = recode(B1.1, "1"="10th std","2"="12th std", "3"="Graduation", "4"="Vocation degree", "5"="Post-graduation",
                      "6"="Above post-graduation","7"="Don't know",
                          .default = "0")) %>%
  mutate(b21 = recode(B2.1, "1"="10th std","2"="12th std", "3"="Graduation", "4"="Vocation degree", "5"="Post-graduation",
                      "6"="Above post-graduation","7"="Don't know",
                      .default = "0")) %>%
  mutate(b12 = recode(B1.2, "3"=0.5,"4"=0.5,"3,4"=1,"4,6"=0.5,"4,5"=0.5,"2,3"=0.5,"1,3"=0.5,
                      .default = 0)) %>% 
  mutate(b22 = recode(B2.2, "3"=0.5,"4"=0.5,"3,4"=1,"4,6"=0.5,"4,5"=0.5,"2,3"=0.5,"1,3"=0.5,"2,4"=0.5,
                      .default = 0)) %>%  
  mutate(b13 = recode(B1.3,"2"=1,
                      .default = 0)) %>% 
  mutate(b23 = recode(B2.3, "2"=1,
                      .default = 0)) %>% 
  mutate(b14 = recode(B1.4, "3"=1,"1"=1,"2"=1,"4"=1,"1,3"=1,"1,4"=1,
                      .default = 0)) %>% 
  mutate(b24 = recode(B2.4, "5"=0,
                      .default =1)) %>% 
  mutate(b15 = recode(B1.5, "1"="Yes, I have started planning and have a clear idea, what I am going to do after 12th class",
                      "2"="Yes, I am thinking about my future and have started planning, but don't have a clear idea",
                      "3"="No, I haven't started thinking about what I'm going to do after school",
                      "4"="4.	Don't want to respond",
                      .default = "NA")) %>% 
  mutate(b25 = recode(B2.5, "1"="Yes, I have started planning and have a clear idea, what I am going to do after 12th class",
                      "2"="Yes, I am thinking about my future and have started planning, but don't have a clear idea",
                      "3"="No, I haven't started thinking about what I'm going to do after school",
                      "4"="4.	Don't want to respond",
                      .default = "NA")) %>% 
  mutate(b16 = recode(B1.6, "1"="Yes","2"="No",
                      .default = "NA")) %>% 
  mutate(b26 = recode(B2.6, "1"="Yes","2"="No",
                      .default = "NA")) %>% 
  mutate(b17_plan = B1.7) %>% 
  mutate(b27_plan = B2.7) %>% 
  mutate(b18_challenges = B1.8) %>% 
  mutate(b28_challenges = B2.8) %>% 
  mutate(b19 = recode(B1.9, "4"=1,
                      .default = 0)) %>% 
  mutate(b29 = recode(B2.9, "4"=1,
                      .default = 0)) %>% 
  mutate(b110 = recode(B1.10, "1"=0.25,"2"=0.25,"3"=0,"4"=0.25,"5"=0.25,
"1.2.3.4"=0.75,"1.5"=0.5,"1.2.4.5"=1,"1.2.4.6"=0.75,
"1.2.5.6"=0.5,"1.2.4.5.6"=1,"1,2,3,4"=0.75,"1,2,4,6"=0.75,
"2,4,6,7"=0.5,"1,2,3,6"=0.5,"1,3,4,6"=0.5,"1246"=0.75,
"1,2,5,6"=0.75,"1,2,4,7"=0.75,"1,2,4,5,6"=1,"1,2,3,4,5,6"=0,
"2,6"=0.25,"1,7"=0.25,"1,2,6"=0.5,"1,2,4"=0.75,"1,2"=0.5,
"1,,5,6,4"=0.75,"2,4,5,6"=0.75,"3,5,6,7"=0.25,
"1,2,3,5"=0.5,"1,3,5,6"=0.5,"1,4,6"=0.5,"2,5,6,7"=0.5,
"2,3,4,6"=0.5,"1,4,5,6"=0.75,"1,2,7"=0.5,
"1,4,6,7"=0.5,"1,3,4,6,7"=0.5,"2,4,6"=0.5,
"1,2,4,6,"=0.75,"1,2,3"=0.5,
                      .default = 0)) %>% 
  mutate(b210 = recode(B2.10, "1"=0.25,"2"=0.25,"3"=0,"4"=0.25,"5"=0.25,
                       "1.2.3.4"=0.75,"1.5"=0.5,"1.2.4.5"=1,"1.2.4.6"=0.75,
                       "1.2.5.6"=0.5,"1.2.4.5.6"=1,"1,2,3,4"=0.75,"1,2,4,6"=0.75,
                       "2,4,6,7"=0.5,"1,2,3,6"=0.5,"1,3,4,6"=0.5,"1246"=0.75,
                       "1,2,5,6"=0.75,"1,2,4,7"=0.75,"1,2,4,5,6"=1,"1,2,3,4,5,6"=0,
                       "2,6"=0.25,"1,7"=0.25,"1,2,6"=0.5,"1,2,4"=0.75,"1,2"=0.5,
                       "1,,5,6,4"=0.75,"2,4,5,6"=0.75,"3,5,6,7"=0.25,
                       "1,2,3,5"=0.5,"1,3,5,6"=0.5,"1,4,6"=0.5,"2,5,6,7"=0.5,
                       "2,3,4,6"=0.5,"1,4,5,6"=0.75,"1,2,7"=0.5,
                       "1,4,6,7"=0.5,"1,3,4,6,7"=0.5,"2,4,6"=0.5,
                       "1,2,4,6,"=0.75,"1,2,3"=0.5,
                       .default = 0)) %>% 
  mutate(b111 = recode(B1.11, "3"=0.5,"4"=0.5,"3,4"=1,"3.4"=1,"3,5"=0.5,
                       "2,4"=0.5,"2,3"=0.5,"1,3"=0.5,
                      .default = 0)) %>% 
  mutate(b211 = recode(B2.11, "3"=0.5,"4"=0.5,"3,4"=1,"3,4,"=1,"3.4"=1,"3,5"=0.5,
                       "2,4"=0.5,"2,3"=0.5,"1,3"=0.5,
                       .default = 0)) %>% 
  mutate(b112 = recode(B1.12, "1"=1,
                      .default = 0)) %>% 
  mutate(b212 = recode(B2.12, "1"=1,
                       .default = 0)) %>% 
  mutate(b113 = recode(B1.13, "1"=1,
                       .default = 0)) %>% 
  mutate(b213 = recode(B2.13, "1"=1,
                       .default = 0)) %>%
  mutate(b114 = recode(B1.14, "3"=1,
                       .default = 0)) %>% 
  mutate(b214 = recode(B2.14, "3"=1,
                       .default = 0)) %>%
  mutate(b115 = recode(B1.15, "2"=1,
                       .default = 0)) %>% 
  mutate(b215 = recode(B2.15, "2"=1,
                       .default = 0)) %>%
  mutate(b116 = recode(B1.16, "1"=1,
                       .default = 0)) %>% 
  mutate(b216 = recode(B2.16, "1"=1,
                       .default = 0)) %>%
  mutate(b117 = recode(B1.17, "5"=1,
                       .default = 0)) %>% 
  mutate(b217 = recode(B2.17, "5"=1,
                       .default = 0)) %>%
  mutate(b118 = recode(B1.18, "1"=0.5,"2"=0.5,"1,2"=1,"1.2"=1,
                      .default = 0)) %>% 
  mutate(b218 = recode(B2.18, "1"=0.5,"2"=0.5,"1,2"=1,"1,,2"=1,"1.2"=1,
                       .default = 0)) %>% 
  mutate(b119 = recode(B1.19, "2"=0.25,"4"=0.25,"5"= 0.25,"3"=0.25, "2,7,8"=0.25,
                         "2,3,4,5"=1,"2,9"=0.25,"2,3,5"=0.75,
                         "3,4"=0.5,"2,4,5"= 0.75,"2,4,5,7"= 0.75,
                         "2,3,4"=0.75,"2,8"=0.25,
                       .default = 0)) %>% 
  mutate(b219 = recode(B2.19, "2.4.7"=0.5,
                       "2,3,4,5,6,7,8"=1,
                       "2,3,4,5,6"=0,
                       "3,4,6"=0.5,
                       "2,7"=0.25,
                       "2,5"=0.5,
                       "4,5"=0.5,
                       "2,4,5"=0.75,
                       "7,8"=0,
                       "2,6"=0.25,
                       "3,4"=0.5,
                       "3,5"=0.5,
                       "2,3,4,6"=0.75,
                       "2,4"=0.5,
                       "2,3,4"=0.75,
                       "2,3,4,5"=1,
                       "2,3"=0.5,
                       "2,4,5,6"=0.75,
                       "2,6,8"=0.25,
                       "2,3,4,5,8"=0,
                       "2,5,8"=0.5,
                       "2,8"=0.25,
                       "2,4,5,8"=0.75,
                       "3,4,5"=0.75,
                       "2,3,5"=0.75,
                       "1,2"=0,
                       "2,4,6"=0.5,
                       "2,3,6"=0.5,
                       "2,3,5,7"=0.75,
                       "1,4"=0.25,
                       "2,3,4,7"=0.75,
                       "1,3"=0.25,
                       "2,3,4,7,8"=0,
                       "2,3,45,5"=1,
                       "7"=0,
                       "2,6,7,9"=0,
                       "2,7,8"=0,
                       .default = 0)) %>% 
  mutate(b120 = recode(B1.20, "4"=1,
                       .default = 0)) %>% 
  mutate(b220 = recode(B2.20, "4"=1,
                       .default = 0)) %>% 
  mutate(b121 = recode(B1.21, "2"=1,
                       .default = 0)) %>%
  mutate(b221 = recode(B2.21, "2"=1,
                       .default = 0)) %>%
  mutate(b122 = recode(B1.22, "2"=1,
                       .default = 0)) %>%
  mutate(b222 = recode(B2.22, "2"=1,
                       .default = 0)) %>%
  mutate(b123 = recode(B1.23, "4"=1,
                       .default = 0)) %>% 
  mutate(b223 = recode(B2.23, "4"=1,
                       .default = 0)) %>%
  mutate(b124 = recode(B1.24, "2"=1,
                       .default = 0)) %>%
  mutate(b224 = recode(B2.24, "2"=1,
                       .default = 0)) %>%
  mutate(b125 = recode(B1.25, "1"=0.5,"3"=0.5,"1,3"=0.5,"1.3"=0.5,
                       .default = 0)) %>% 
  mutate(b225 = recode(B2.25, "1"=0.5,"3"=0.5,"1,3"=0.5,"1.3"=0.5,
                       .default = 0)) %>% 
  mutate(b126 = recode(B1.26, "1"=1,
                       .default = 0)) %>%
  mutate(b226 = recode(B2.26, "1"=1,
                       .default = 0)) %>%
  mutate(b127 = recode(B1.27, "1"=1,
                       .default = 0)) %>%
  mutate(b227 = recode(B2.27, "1"=1,
                       .default = 0)) %>%
  mutate(b128 = recode(B1.28, "1"=1,
                       .default = 0)) %>%
  mutate(b228 = recode(B2.28, "1"=1,
                       .default = 0)) %>%
  mutate(b129 = recode(B1.29, "4"=1,
                       .default = 0)) %>%
  mutate(b229 = recode(B2.29, "4"=1,
                       .default = 0)) %>%
  mutate(b130 = recode(B1.30, "1"=1,
                       .default = 0)) %>%
  mutate(b230 = recode(B2.30, "1"=1,
                       .default = 0)) %>%
  mutate(b131 = recode(B1.31, "2"=1,
                       .default = 0)) %>%
  mutate(b231 = recode(B2.31, "2"=1,
                       .default = 0)) %>%
  mutate(b132 = recode(B1.32, "1"=1,
                       .default = 0)) %>%
  mutate(b232 = recode(B2.32, "1"=1,
                       .default = 0)) %>%
  mutate(b133 = recode(B1.33, "3"=1,
                       .default = 0)) %>%
  mutate(b233 = recode(B2.33, "3"=1,
                       .default = 0)) %>%
  mutate(b134 = recode(B1.34, "1"=0.33,
                       "2"=0.33,
                       "5"=0,
                       "6"=0,
                       "4"=0.33,
                       "2.5"=0.33,
                       "3"=0,
                       "-"=0,
                       "1.2.4"=1,
                       "1,2"=0.66,
                       "1,2,3,4,5"=0,
                       "1,2,4"=1,
                       "3,5"=0,
                       "2,4"=0.66,
                       "1,4"=0.66,
                       "1,2,3,4"=0.66,
                       "1,2,5"=0.66,
                       "7"=0,
                       "3,4"=0.33,
                       "1,3,5"=0.33,
                       "1,3"=0.33,
                       "1,3,6"=0.33,
                       .default = 0)) %>%
  mutate(b234 = recode(B2.34, "6"=0,
                       "-"=0,
                       "4"=0.33,
                       "5"=0,
                       "2"=0.33,
                       "1"=0.33,
                       "1.4"=0.6,
                       "3"=0,
                       "1.2"=0.66,
                       "1.2.5"=0.66,
                       "1.2.4"=1,
                       "1,2"=0.66,
                       "1,2,3,4,5"=0,
                       "1,2,4"=1,
                       "2,4"=0.66,
                       "1,4"=0.66,
                       "1,2,3,4"=0.66,
                       "1,2,5"=0.66,
                       "5,6"=0,
                       "1,5"=0.33,
                       "4,6"=0.33,
                       "3,4"=0.33,
                       "2,5"=0.33,
                       "1,3,4"=0.66,
                       "1,3"=0.33,
                       "1,2,4,"=1,
                       "1,6"=0.33,
                       .default = 0)) %>%
  mutate(b135 = recode(B1.35, "2"=0,
                       "7"=0,
                       "5"=0.33,
                       "3"=0,
                       "6"=0.33,
                       "4"=0.33,
                       "5.6"=0.66,
                       "2,6"=0.33,
                       "5,6"=0.66,
                       "4,6"=0.66,
                       "2,3,5"=0.33,
                       "1"=0,
                       "2,3,6"=0.33,
                       "3,6"=0.33,
                       "2,5,6"=0.66,
                       "2,3,4,5"=0.66,
                       "9"=0,
                       "2,4"=0.33,
                       "4,5"=0.66,
                       .default = 0)) %>%
  mutate(b235 = recode(B2.35, "7"=0,
                       "1"=0,
                       "6"=0.33,
                       "5"=0.33,
                       "3"=0,
                       "2"=0,
                       "4.5.6"=1,
                       "4"=0.33,
                       "3.5"=0.33,
                       "4.6"=0.66,
                       "2,3"=0,
                       "4,6"=0.66,
                       "3,4"=0.33,
                       "5,6"=0.66,
                       "4,5"=0.66,
                       "4,5,6"=1,
                       "2,3,4,5,6"=0,
                       "2,3,4"=0.33,
                       "2,5"=0.33,
                       "2,3,5"=0.33,
                       "2,4,5"=0.66,
                       "2,6"=0.33,
                       "1,2,4"=0.33,
                       "3,4,5,6"=0.66,
                       "3,6"=0.33,
                       "3,5"=0.33,
                       "3,5,6"=0.66,
                       "2,4"=0.33,
                       .default = 0)) %>%
  mutate(b136 = recode(B1.36, "2"=1,
                      .default = 0)) %>% 
  mutate(b236 = recode(B2.36, "2"=1,
                       .default = 0)) %>%
  mutate(b137 = recode(B1.37, "2"=1,
                       .default = 0)) %>% 
  mutate(b237 = recode(B2.37, "2"=1,
                       .default = 0)) %>%
  mutate(b138 = recode(B1.38, "3"=1,
                       .default = 0)) %>% 
  mutate(b238 = recode(B2.38, "3"=1,
                       .default = 0)) %>%
  mutate(b139 = recode(B1.39, "7"=0,
                       "1"=0.25,
                       "1.2"=0.5,
                       "6"=0,
                       "2.3"=0.5,
                       "1.2.3"=0.75,
                       "2"=0.25,
                       "3"=0.25,
                       "5"=1,
                       "2.3.4"=0.75,
                       "1.2.3.7"=0.75,
                       "1.6"=0.25,
                       "2.6"=0.25,
                       "1.2.3.4"=1,
                       "1.4"=0.5,
                       "4"=0.25,
                       "2,3"=0.5,
                       "1,2"=0.5,
                       "1,4"=0.5,
                       "1,2,3"=0.75,
                       "1,2,3,4"=1,
                       "1,2,3,4,5,6"=0,
                       "1,2,3,4,5"=1,
                       "1,5"=1,
                       "1,2,6"=0.5,
                       "1,2,4"=0.75,
                       "2,4"=0.5,
                       "5,6"=0,
                       "2,3,6"=0.5,
                       "DON'T UNDERSTAND"=0,
                       "4,6"=0.25,
                       "3,7"=0.25,
                       "3,4"=0.5,
                       "2,5"=0.5,
                       .default = 0)) %>% 
  mutate(b239 = recode(B2.39, "7"=0,
                       "2"=0.25,
                       "1"=0.25,
                       "5"=1,
                       "1.2.3"=0.75,
                       "3"=0.25,
                       "1.2.3.4"=1,
                       "4"=0.25,
                       "2.3"=0.5,
                       "1.2"=0.5,
                       "1.2.3."=0.75,
                       "1.2.4.7"=0.75,
                       "1.3"=0.5,
                       "2.3.4"=0.75,
                       "1.5"=0.5,
                       "1,2,3,4,5"=1,
                       "1,2,3,4"=1,
                       "1,2"=0.5,
                       "1,2,3"=0.75,
                       "2,3"=0.5,
                       "1,2,4"=0.75,
                       "2,4"=0.5,
                       "1,2,3,4,5,6"=0,
                       "1,2,3,6"=0.75,
                       "5,7"=0,
                       "1,2,3,5"=1,
                       "6"=0,
                       "1,3"=0.5,
                       "1,2,5"=0.5,
                       "1,2,5,6"=0.5,
                       "1,2,32,4"=1,
                       "3,5,6"=0.25,
                       "1,4,5"=0.5,
                       "1,2,4,5"=0.75,
                       "3,5"=0.25,
                       .default = 0)) %>%
  mutate(b140 = recode(B1.40, "3"=1,
                       .default = 0)) %>% 
  mutate(b240 = recode(B2.40, "3"=1,
                       .default = 0)) %>%
  mutate(b141 = recode(B1.41, "5"=0,
                       "2.3"=1,
                       "3.4"=0.5,
                       "2"=0.5,
                       "1"=0,
                       "3"=0.5,
                       "1.2.3"=1,
                       "2,3"=1,
                       "4"=0,
                       "1,2,3"=1,
                       "3,4"=0.5,
                       "1,2"=0.5,
                       "2,5"=0.5,
                       "2,3,4"=1,
                       "2,4"=0.5,
                       "3,5"=0.5,
                       .default = 0)) %>% 
  mutate(b241 = recode(B2.41, "5"=0,
                       "2"=0.5,
                       "3"=0.5,
                       "3.4"=0.5,
                       "4"=0,
                       "2.3"=1,
                       "1.3"=0.5,
                       "1"=0,
                       "1.2.3"=1,
                       "2,3"=1,
                       "1,2"=0.5,
                       "1,3"=0.5,
                       "2,4"=0.5,
                       "3,4"=0.5,
                       "1,2,4"=0.5,
                       "1,2,3"=1,
                       "2,3,4"=1,
                       "3,5"=0.5,
                       "2,5"=0.5,
                       .default = 0)) %>%
  mutate(c11 = recode(C1.1, "1"=1,"2"=0,
                       .default = 0)) %>% 
  mutate(c21 = recode(C2.1, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c12 = recode(C1.2, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c22 = recode(C2.2, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c13 = recode(C1.3, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c23 = recode(C2.3, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c14 = recode(C1.4, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c24 = recode(C2.4, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c15 = recode(C1.5, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c25 = recode(C2.5, "1"=1,"2"=0,
                      .default = 0)) %>% 
  mutate(c16 = recode(C1.6, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c26 = recode(C2.6, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c17 = recode(C1.7, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c27 = recode(C2.7, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c18 = recode(C1.8, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c28 = recode(C2.8, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c19 = recode(C1.9, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c29 = recode(C2.9, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c110 = recode(C1.10, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c210 = recode(C2.10, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c111 = recode(C1.11, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c211 = recode(C2.11, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c112 = recode(C1.12, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c212 = recode(C2.12, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c113 = recode(C1.13, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c213 = recode(C2.13, "1"=0,"2"=1,
                       .default = 0)) %>% 
  mutate(c114 = recode(C1.14, "1"=1,"2"=0,
                       .default = 0)) %>% 
  mutate(c214 = recode(C2.14, "1"=1,"2"=0,
                       .default = 0)) %>% 
  mutate(c115 = recode(C1.15, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c215 = recode(C2.15, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c116 = recode(C1.16, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c216 = recode(C2.16, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c117 = recode(C1.17, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c217 = recode(C2.17, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c118 = recode(C1.18, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c218 = recode(C2.18, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c119 = recode(C1.19, "1"=0,"2"=1,
                      .default = 0)) %>% 
  mutate(c219 = recode(C2.19, "1"=0,"2"=1,
                      .default = 0)) %>%
  mutate(d11 = recode(D1.1, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>% 
  mutate(d21 = recode(D2.1, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>%
  mutate(d12 = recode(D1.2, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>% 
  mutate(d22 = recode(D2.2, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>%
  mutate(d13 = recode(D1.3, 
                      "1"=1,
                      "2"=2,
                      "3"=3,
                      "4"=4,
                      .default = 0)) %>% 
  mutate(d23 = recode(D2.3, 
                      "1"=1,
                      "2"=2,
                      "3"=3,
                      "4"=4,
                      .default = 0)) %>%
  mutate(d14 = recode(D1.4, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>% 
  mutate(d24 = recode(D2.4, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>%
  mutate(d15 = recode(D1.5, 
                      "1"=1,
                      "2"=2,
                      "3"=3,
                      "4"=4,
                      .default = 0)) %>% 
  mutate(d25 = recode(D2.5, 
                      "1"=1,
                      "2"=2,
                      "3"=3,
                      "4"=4,
                      .default = 0)) %>%
  mutate(d16 = recode(D1.6, 
                      "1"=1,
                      "2"=2,
                      "3"=3,
                      "4"=4,
                      .default = 0)) %>% 
  mutate(d26 = recode(D2.6, 
                      "1"=1,
                      "2"=2,
                      "3"=3,
                      "4"=4,
                      .default = 0)) %>%
  mutate(d17 = recode(D1.7, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>% 
  mutate(d27 = recode(D2.7, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>%
  mutate(d18 = recode(D1.8, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>% 
  mutate(d28 = recode(D2.8, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>%
  mutate(d19 = recode(D1.9, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>% 
  mutate(d29 = recode(D2.9, 
                      "1"=4,
                      "2"=3,
                      "3"=2,
                      "4"=1,
                      .default = 0)) %>%
  mutate(d110 = recode(D1.10, 
                       "1"=4,
                       "2"=3,
                       "3"=2,
                       "4"=1,
                      .default = 0)) %>% 
  mutate(d210 = recode(D2.10, 
                       "1"=4,
                       "2"=3,
                       "3"=2,
                       "4"=1,
                      .default = 0))
#export re-coded data frame
#install.packages("writexl")

library("writexl")

write_xlsx(df2,"C:/Users/harik/Documents/Desktop-Samsung/VOICE/UWH/IA/HV_UWH_recoded.xlsx")
#categorization + addition

df3 <- df2 %>% 
  mutate(
    b_ed_1 = rowSums(select(.,b13,b14),na.rm = TRUE),
    b_pln_1 = b16,
    b_crf_1 = rowSums(select(.,b19,b110,b111,b136), na.rm = TRUE),
    b_srh_1 = rowSums(select(.,b112,b119), na.rm = TRUE),
    b_msh_1 = rowSums(select(.,b113,b114,b115,b116,b117,b118), na.rm = TRUE),
    b_mh_1 = rowSums(select(.,b120,b121,b122,b123), na.rm = TRUE),
    b_v_1 = rowSums(select(.,b124,b125,b126,b127,b128), na.rm = TRUE),
    b_lr_1 = rowSums(select(.,b129,b130,b131,b132), na.rm = TRUE),
    b_hr_1 = rowSums(select(.,b133,b134), na.rm = TRUE),
    b_mr_1= rowSums(select(.,b135), na.rm = TRUE),
    b_ng_1 = rowSums(select(.,b137,b140,b141), na.rm = TRUE),
    b_cm_1 = rowSums(select(.,b138,b139), na.rm = TRUE),
    b_ed_2 = rowSums(select(.,b23,b24), na.rm = TRUE),
    b_pln_2 = b26,
    b_crf_2 = rowSums(select(.,b29,b210,b211,b236), na.rm = TRUE),
    b_srh_2 = rowSums(select(.,b212,b219), na.rm = TRUE),
    b_msh_2 = rowSums(select(.,b213,b214,b215,b216,b217,b218), na.rm = TRUE),
    b_mh_2 = rowSums(select(.,b220,b221,b222,b223), na.rm = TRUE),
    b_v_2 = rowSums(select(.,b224,b225,b226,b227,b228), na.rm = TRUE),
    b_lr_2 = rowSums(select(.,b229,b230,b231,b232), na.rm = TRUE),
    b_hr_2 = rowSums(select(.,b233,b234), na.rm = TRUE),
    b_mr_2= rowSums(select(.,b235), na.rm = TRUE),
    b_ng_2 = rowSums(select(.,b237,b240,b241), na.rm = TRUE),
    b_cm_2 = rowSums(select(.,b238,b239), na.rm = TRUE),
    
    c_ed_1 = rowSums(select(.,c11,c12,c13,b12), na.rm = TRUE),
    c_gd_1 = rowSums(select(.,c14,c15,c16), na.rm = TRUE),
    c_mh_1 = rowSums(select(.,c17,c18), na.rm = TRUE),
    c_v_1 = rowSums(select(.,c19,c110,c117,c118,c119), na.rm = TRUE),
    c_cmr_1 = rowSums(select(.,c111,c112,c113), na.rm = TRUE),
    c_pl_1 = rowSums(select(.,c114), na.rm = TRUE),
    c_ms_1 = rowSums(select(.,c115,c116), na.rm = TRUE),
    c_ed_2 = rowSums(select(.,c21,c22,c23,b22), na.rm = TRUE),
    c_gd_2 = rowSums(select(.,c24,c25,c26), na.rm = TRUE),
    c_mh_2 = rowSums(select(.,c27,c28), na.rm = TRUE),
    c_v_2 = rowSums(select(.,c29,c210,c217,c218,c219), na.rm = TRUE),
    c_cmr_2 = rowSums(select(.,c211,c212,c213), na.rm = TRUE),
    c_pl_2 = rowSums(select(.,c214), na.rm = TRUE),
    c_ms_2 = rowSums(select(.,c215,c216), na.rm = TRUE),
    selfesteempre = rowSums(select(.,d11,d12,d13,d14,d15,d16,d17,d18,d19,d110), na.rm = TRUE),
    selfesteempost = rowSums(select(.,d21,d22,d23,d24,d25,d26,d27,d28,d29,d210), na.rm = TRUE),
  ) %>% 
  mutate (
    knowledgepre = (b_ed_1 + b_crf_1 + b_srh_1 + b_msh_1 + b_mh_1 + b_v_1 + b_lr_1 + b_hr_1 + b_mr_1 + b_ng_1 + b_cm_1),
    knowledgepost = (b_ed_2 + b_crf_2 + b_srh_2 + b_msh_2 + b_mh_2 + b_v_2 + b_lr_2 + b_hr_2 + b_mr_2 + b_ng_2 + b_cm_2),
    attpre = (c_ed_1 + c_mh_1 +c_gd_1 + c_v_1 + c_pl_1 + c_cmr_1 + c_ms_1),
    attpost = (c_ed_2 + c_mh_2 + c_gd_2 + c_v_2 + c_pl_2 + c_cmr_2 + c_ms_2),
  ) %>% 
  select(DATE...2,UID...3,A1.1,A1.2,A1.3,A1.4,A1.5,A1.6,A1.7,A1.8,A1.9,A1.10,A1.11,A1.12,
         b11,	b21,	b12,	b22,	b13,	b23,	b14,	b24,	b15,	b25,
         b16,	b26,	b17_plan,	b27_plan,	b18_challenges,	b28_challenges,	b19,	b29,
         b110,	b210,	b111,	b211,	b112,	b212,	b113,	b213,	b114,	b214,	b115,	b215,
         b116,	b216,	b117,	b217,	b118,	b218,	b119,	b219,	b120,	b220,	b121,	b221,	b222,
         b123,	b223,	b124,	b224,	b125,	b225,	b126,	b226,	b127,	b227,	b128,	b228,	b129,
         b229,	b130,	b230,	b131,	b231,	b132,	b232,	b133,	b233,	b134,	b234,	b135,	b235,	
         b136,	b236,	b137,	b237,	b138,	b238,	b139,	b239,	b140,	b240,	b141,	b241,	c11,
         c21,	c12,	c22,	c13,	c23,	c14,	c24,	c15,	c25,	c16,	c26,	c17,	c27,	c18,
         c28,	c19,	c29,	c110,	c210,	c111,	c211,	c112,	c212,	c113,	c213,	c114,	c214,	c115,
         c215,	c116,	c216,	c117,	c217,	c118,	c218,	c119,	c219,	d11,	d21,	d12,	d22,	d13,	d23,	d14,	d24,	d15,
         d25,	d16,	d26,	d17,	d27,	d18,	d28,	d19,	d29,	d110,	d210, 
         b_ed_1, b_pln_1, b_crf_1, b_srh_1, b_msh_1, b_mh_1, b_v_1, b_lr_1, b_hr_1, b_mr_1,b_ng_1, b_cm_1,
         b_ed_2, b_pln_2, b_crf_2, b_srh_2, b_msh_2, b_mh_2, b_v_2, b_lr_2, b_hr_2, b_mr_2,b_ng_2, b_cm_2,
         c_ed_1, c_mh_1, c_gd_1, c_v_1, c_pl_1, c_cmr_1, c_ms_1,
         c_ed_2, c_mh_2, c_gd_2, c_v_2, c_pl_2, c_cmr_2, c_ms_2,
         selfesteempre,selfesteempost,knowledgepre,knowledgepost, attpre, attpost) %>%
  rename(education_b1 = b_ed_1, 
         planning_b1 = b_pln_1, 
         career_fin_b1 = b_crf_1, 
         srh_b1 = b_srh_1, 
         menstrual_health_b1 = b_msh_1, 
         mental_health_b1 = b_mh_1, 
         violence_b1 = b_v_1, 
         laws_rights_b1 = b_lr_1, 
         healthy_relations_b1 = b_hr_1, 
         marriage_b1 = b_mr_1,
         negotiation_b1 = b_ng_1, 
         conflict_b1 = b_cm_1,
         education_b2 = b_ed_2, 
         planning_b2 = b_pln_2, 
         career_fin_b2 = b_crf_2, 
         srh_b2 = b_srh_2, 
         menstrual_health_b2 = b_msh_2, 
         mental_health_b2 = b_mh_2, 
         violence_b2 = b_v_2, 
         laws_rights_b2 = b_lr_2, 
         healthy_relations_b2 = b_hr_2, 
         marriage_b2 = b_mr_2,
         negotiation_b2 = b_ng_2, 
         conflict_b2 = b_cm_2,
         education_c1 = c_ed_1, 
         mental_health_c1 = c_mh_1, 
         gender_disc_c1 = c_gd_1, 
         violence_c1 = c_v_1, 
         planning_c1 = c_pl_1, 
         child_marriage_c1 = c_cmr_1, 
         menstruation_c1 = c_ms_1,
         education_c2 = c_ed_2, 
         mental_health_c2 = c_mh_2, 
         gender_disc_c2 = c_gd_2, 
         violence_c2 = c_v_2, 
         planning_c2 = c_pl_2, 
         child_marriage_c2 = c_cmr_2, 
         menstruation_c2 = c_ms_2
  ) %>% 
  print()
  write_xlsx(df3,"C:/Users/harik/Documents/Desktop-Samsung/VOICE/UWH/IA/HV_UWH_recoded+scored2.xlsx")
  
 