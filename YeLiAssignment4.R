#' ---
# title: Assignment 4 
# author: "Ye Li"
# date: "Winter 2016"
# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/master/Assignments/Econ_294_Assignment_4.pdf
# ---

rm(list=ls())
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 
print("Ye Li")
print(1505112)
print("yli247@ucsc.edu")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1 load `flights.csv`, `planes.csv`, `weather.csv`, and `airports.csv`.
library(foreign)
library(dplyr)
flights<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv", 
               stringsAsFactors = FALSE) %>%
  tbl_df()

planes<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv", 
                   stringsAsFactors = FALSE) %>%
  tbl_df()

weather<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv", 
                   stringsAsFactors = FALSE) %>%
  tbl_df()

airports<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv", 
                   stringsAsFactors = FALSE) %>%
  tbl_df()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2 convert date

flights$DATE<-as.Date(flights$date)
flights<-flights%>%
  select(DATE,everything())

weather$DATE<-as.Date(weather$date)
weather<-weather%>%
  select(DATE,everything())
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3 
#flights.2a-->all flights that went to the city of San Francisco(SFO) or Oakland(OAK)CA
flights.2a<-subset(flights,dest=="SFO"|dest=="OAK")
a3<-"the number of observation is"
nrow(flights.2a)
print(paste(a3,nrow(flights.2a)))

#flights.2b-->all flights delayed by an hour or more
flights.2b<-subset(flights,arr_delay>=60|dep_delay>=60)
nrow(flights.2b)
print(paste(a3,nrow(flights.2b)))

#flights.2c-->all flights in which the arrival delay was more than twice as much as the departure delay 
flights.2c<-subset(flights,arr_delay>=2*dep_delay)
nrow(flights.2c)
print(paste(a3,nrow(flights.2c)))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 4 select

#1st Method
Method1<-select(flights,dep_delay,arr_delay)

#2nd Method
Method2<-select(flights,ends_with("_delay"))

#3rd Method
Method3<-select(flights,dep_delay:arr_delay)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 5 Arrange

#5a--> the top five most (departure) delayed flights
flights.5a<-flights %>% 
  arrange(-dep_delay) %>% 
  select(dep_delay,flight,dest,plane,everything()) 

print(head(flights.5a,5))


#5b--> the top five flights that caught up the most (in absolute time)during the flight
flights.5b<-flights %>%
  mutate(
    CaughtUp=dep_delay-arr_delay
  ) %>%
  arrange(-CaughtUp) %>%
  select(CaughtUp,dep_delay,arr_delay,flight,plane,everything()) 

print(head(flights.5b,5))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 6 Mutate

#speed in mph
flights.6<-flights %>%
  mutate(
    speed=dist/(time/60)
  )



#delta


flights.6<-flights.6 %>%
  mutate(
    delta=dep_delay-arr_delay
  )%>%
  select(delta,dep_delay,arr_delay,speed,everything())%>%
  arrange(-delta,-speed) 
print(head(flights.6,5))

#6a-->top five flights by speed.
flights.6a<-flights.6 %>%
  arrange(-speed)%>%
  select(speed,everything())
print(head(flights.6a,5))
  

#6b-->top five flights that made up the most time in flight (this should match 5b)
flights.6b<-flights.6 %>%
  arrange(-delta)%>%
  select(delta,dep_delay,arr_delay,everything())
print(head(flights.6b,5))





#6c-->top flights that lost the most time in flight.
flights.6c<-flights.6 %>%
  arrange(delta)%>%
  select(delta,dep_delay,arr_delay,everything())
print(head(flights.6c,5))





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 7 group_by &summarize
##flights.7a
#Grouping by carrier.
flights.7a<-flights.6 %>%
  group_by(carrier) %>%               
  summarise( 
    num_cancelled=sum(cancelled),
    total_flights =n(),
    ratio=num_cancelled/total_flights,
    min_delta = min(delta,na.rm = T), 
    q1st_delta = quantile(delta,0.25,na.rm = T),
    mean_delta = mean(delta,na.rm=T),
    q3st_delta = quantile(delta,0.75,na.rm = T),
    q90_delta=quantile(delta,0.90,na.rm=T),
    max_delta = max(delta,na.rm = T)
  )

flights.7a<-flights.7a %>%
  arrange(-ratio) %>%
print(flights.7a)

#Grouping by Day and Hour. 
day_delay <- dplyr::filter( summarize(
  group_by( dplyr::filter(
    flights,
    !is.na(dep_delay) ),
    date ),
  delay = mean(dep_delay), n = n()
),
n > 10 )
a7<-"the code above first filter the NA observations in dep_delay in flights, then group it by date, then making a summary table of the average delay of dep_delay, total number of flights, grouped by date"

print(a7)

#rewrite use %>%
day_delay1<-flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarize(
    delay=mean(dep_delay),
    n=n()
  ) %>%
  dplyr::filter(n>10)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 8

day_delay1<-day_delay %>%
  mutate(
    day_lag=delay-lag(delay)
  ) %>%
  arrange(desc(day_lag)) %>%
  head(5)

day_delay1


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 9
dest_delay<-flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarize(
    delay=mean(arr_delay),
    n=n()
  )
 

  
airports1<- airports %>%
  select(iata, airport, city, state, lat, long) %>%
  rename(dest=iata) %>%
  rename(name=airport)

#df.9a
df.9a<-dest_delay %>%
  left_join(airports1) %>%
  arrange(desc(delay))

print(head(df.9a,5))

#df.9b
df.9b<-dest_delay %>%
  inner_join(airports1)
a9b<-"no, the number of observation when use inner_join does not match with lef_join "
print(a9b)

#df.9c
df.9c<-dest_delay %>%
  right_join(airports1)
nrow(df.9c)
a9c<-"the number of observation now is 3376, and NA appears in the new table.Because right_join merging keeps all varibles in the right table, when they can not match with left one, NA appears"
print(a9c)

#df.9d
df.9d<-dest_delay %>%
  full_join(airports1)
nrow(df.9d)
a9d<-"the number of observation now is 3378, and NA appears in the new table.Because full_join merging keeps all varibles in the both sides tables, when they can not match with each other, NA appears"
print(a9d)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 10

hourly_delay<-flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(DATE,hour) %>%
  summarize(
    delay=mean(dep_delay),
    n=n()
  )

df10<-hourly_delay %>%
  left_join(weather)

condition<- df10 %>%
  filter(!is.na(delay)) %>%
  group_by(conditions)%>%
  summarize(
   delay=weighted.mean(delay,n),
   number=sum(n)
  ) %>%
  arrange(desc(delay))

print(head(condition,5))

a10<-"Freezing Rain associates with the biggest delay"
print(a10)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 11
##a
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df

df2<-df %>%
  gather(subject,value,
        ...=-treatment,
         na.rm=T) %>%
  arrange(subject,treatment)
df2

df3<-df2 %>%
  separate(subject,
           c("name","subject"),
           7
  )
df3

df4<-df3 %>%
  select(subject,treatment,value) %>%
  arrange(subject,treatment)
df4

##b

df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"), value = c(3,4,5,6)
)
df

df1<-df %>%
  mutate(
    name="subject"
  )
df1

df2<-df1 %>%
  unite(
    col=subject,
    ...=name,subject,
    sep=""
  )
df2


df3<-df2 %>%
  spread(
    key=subject,
    value=value
  ) 
df3

##c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"), value = c(3,4,5,6)
)
df

df2<-df %>%
  separate(
    demo,
    c("sex","age","state"),
    "_"
  )
df2

##d
df <- data.frame(
subject = c(1,2,3,4),
sex = c("f","f","m",NA),
age = c(11,55,65,NA),
city = c("DC","NY","WA",NA), value = c(3,4,5,6)
)
df
df1<-df %>%
  unite(
    col = demo, 
    ... = sex, age,city,
    sep = "."
  ) 
df1


df1$demo<- replace(x=df1$demo,df1$demo=="NA.NA.NA",values=NA)
df1
