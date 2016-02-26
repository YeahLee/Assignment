#' ---
# title: Assignment 5
# author: "Ye Li"
# date: "Winter 2016"
# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/master/Assignments/Econ_294_Assignment_5.pdf
# ---

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 
print("Ye Li")
print(1505112)
print("yli247@ucsc.edu")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1
library(ggplot2)
library(tidyr)
library(dplyr)
library(foreign)
#---a---#
diamonds <- diamonds %>% 
  mutate(
    volume=x*y*z
  ) %>%
  tbl_df()


a <- ggplot(diamonds,
            aes(x=volume, y=price))

a + geom_point(aes(colour = clarity,size=carat),alpha=0.2) +
  scale_x_log10() +scale_y_log10()


#---b---#


b<-ggplot(diamonds) 

b+geom_histogram(aes(x=carat,
                     y=..density..,
                     fill=clarity
                     ),binwidth = 0.2) +facet_grid(cut ~ .) 


#---c---#
c <- ggplot(diamonds, aes(cut, price))
c + geom_violin()+geom_jitter(alpha=0.02) 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3
#---a---#
org<-read.dta("/Users/liye/Documents/Graduate/UCSC/Academic/Winter/294lab/econ294_2015/data/org_example.dta")


org1 <- org%>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  filter(!is.na(rw)) %>%
  tbl_df()
#generate summary table of real wage
RW<-org1 %>%
  group_by(year,month) %>%
  summarize(
    qtl10=quantile(rw,0.10),
    qtl25=quantile(rw,0.25),
    qtl75=quantile(rw,0.75),
    qtl90=quantile(rw,0.90),
    Median.Rw=median(rw),
    n=n()
  )
#generate date variable
RW <- RW%>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  tbl_df()

#plot
aa<-ggplot(RW, aes(date, Median.Rw))
  
aa+geom_line()+lims(y=c(0,50)) +
  geom_ribbon(aes(ymin=qtl10, 
                  ymax=qtl90), 
              alpha = 0.2)+
  geom_ribbon(aes(ymin=qtl25, 
                  ymax=qtl75), 
              alpha = 0.5)


#---b---#
#generate summary table including group of educ of real wage
RWb<-org1 %>%
  group_by(year,month,educ) %>%
  summarize(
    Median.Rw=median(rw)
  )

#generate date variable
RWb <- RWb%>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  tbl_df()
#plot
bb<-ggplot(RWb, aes(date, Median.Rw,group=educ))
bb+geom_line(aes(colour=educ))
  
