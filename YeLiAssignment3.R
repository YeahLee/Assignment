#' ---
# title: Assignment 3 
# author: "Ye Li"
# date: "Winter 2016"
# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/772cee1c713acd91521f7f0620914bd75f4a5664/Assignments/Econ_294_Assignment_3.pdf
# ---


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 
print("Ye Li")
print(1505112)
print("yli247@ucsc.edu")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
#loaded this way, it's a data frae
class(df.ex)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2 Filter
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2a <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2a))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3 Arrange

df.ex.3a<- df.ex %>%
  dplyr::arrange(year,month) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 4 Select
#df.ex.4a with only columns year through age
df.ex.4a<-select(df.ex, year:age)
print(ncol(df.ex.4a))
  

#df.ex.4b with only columns year, month, and columns that start with i.
df.ex.4b<-select(df.ex,year,month,starts_with("i"))

#For the variable `state` print the distinct set of values in the original df.ex.
distinct(select(df.ex,state))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 5 Mutate

#Function stndz
stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}

#Function nrmlz
nrmlz <-function(x){
  (x - min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm = T))
}

#df.ex.5a with two new columns, 
#rw.stndz with the standardized score of real wages, 
#rw_nrmlz with feature scaled values of real wages.

df.ex.5a<- df.ex %>% 
  mutate(
    rw.stndz= stndz(df.ex$rw),
    rw_nrmlz= nrmlz(df.ex$rw)
  ) 


#`df.ex.5b` with three new columns
#the standard score (`rw.stndz`), feature scaled value (`rw_nrmlz`), and `count` at year, month groupings. 

df.ex.5b <- df.ex %>%
  dplyr::group_by(year, month) %>%
  dplyr::mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count    = n()
  ) 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 6 Summarize

df.ex.6 <- df.ex %>%
  dplyr::group_by(year, month, state) %>%
  dplyr::summarise(
    rw.min = min(rw, na.rm = T),
    rw.1stq = quantile(rw, 0.25, na.rm = T),
    rw.mean = mean(rw, na.rm = T),
    rw.median = median(rw, na.rm = T),
    rw.3rdq = quantile(rw, 0.75, na.rm = T),
    rw.max = max(rw, na.rm = T),
    count = n()
  )

print(nrow(df.ex.6))

#Print the year, month, state of observation that has the highest mean real wage.
print(df.ex.6%>%
        ungroup() %>%
        arrange(desc(rw.mean))%>%
        select(year,month,state,rw.mean) %>%
        head(1))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 7 Challenge

#convert state into a char vector 
df.ex$state<- as.character(df.ex$state)

#df.ex.7a that is sorted with year and month ascending and state sorted in descending alphabetical order
df.ex.7a<-df.ex %>% 
  arrange(year, month, desc(state)) %>%
  select(year,month,state)








