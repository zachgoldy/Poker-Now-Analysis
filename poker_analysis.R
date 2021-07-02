library(tidyverse)
library(lubridate)
library(wesanderson)
library(DataCombine)
setwd(getwd()) #ADD POKERNOW DIRECTORY HERE
pokernow <- read.csv("poker_now_log_j2MzecjXNYuyl58PLPO5yhjj7.cs") #INSERT POKER NOW CSV HERE 
entries <- pokernow$entry
entry_stack <- c()

# Helper Functions 

#I'm sure this function exists as a built in thing somewhere, but since I couldn't find it I made my own
#Just runs the grepl function on each value in a vector and returns a vector of the corresponding booleans.
grepleachin <- function(x, y){
  return_str <- c()
  for(i in x){
    if(grepl(y , i, fixed = TRUE) == TRUE){
      return_str <- c(return_str, TRUE)
    }
    else{
      return_str <- c(return_str, FALSE)
    }
  }
  return_str
}

#Helper function to account for busts later on. 
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

#Grabbing players' stack sizes at different times 
raw_stacks <- pokernow[grepleachin(entries, "Player stacks"),]

final_df <- data.frame(name=character(), stack = double(), time =double())
for(i in 1:nrow(raw_stacks)){
  raw_row <- raw_stacks[i,]
  raw_entry <- raw_row$entry
  temp1 <- scan(text=raw_entry, what='"', quiet=TRUE)
  temp2 <- str_split(raw_entry, "[()]")
  cash_range <- seq(2, length(temp2[[1]]), by = 2)
  name_range <- seq(4, length(temp1), by = 4)
  
  
  stack <- c()
  
  
  for(i in cash_range){
    stack <- c(stack, as.double(temp2[[1]][[i]]))
  }
  
  name <- c()
  for(i in name_range){
    temp3 <- temp1[i]
    name_val <- strsplit(temp3, "@")[[1]][[1]]
    name <- c(name, name_val)
  }
  time_val <- ymd_hms(raw_row$at,tz=Sys.timezone())
  time <- rep(time_val, times = length(name))
  
  return_df <- data.frame(name = name, stack= stack, time = time)
  
  final_df <- rbind(final_df, return_df)
  
}


#Grabbing a player's stack sizes when they quit the game and adding them into the stack dataframe 
raw_quits <- pokernow[grepleachin(entries, "quits the game"),]
updates_df <- data.frame(name=character(), value = double(), time =double())

for(i in 1:nrow(raw_quits)){
  temp_quit <- raw_quits[i,]$entry
  temp_step <-  scan(text=temp_quit, what='"', quiet=TRUE)
  temp_name <- temp_step[3]
  temp_val <- temp_step[11]
  stack <- as.numeric(substr(temp_val, 1, nchar(temp_val) - 1))
  name <- strsplit(temp_name, "@")[[1]][1]
  time <- ymd_hms(raw_quits$at[i],tz=Sys.timezone())
  
  insert_index <- tail(which(final_df$time > time), n = 1)
  if(length(insert_index) == 0){
    insert_index = 1
  }
  print(i)
  return_df <- data.frame(name = name, value= stack, time = time)
  update_df_add <- data.frame(name = name, value = -stack, time = time)
  updates_df <- rbind(updates_df, update_df_add)
  final_df <- InsertRow(final_df, NewRow = return_df, RowNum = insert_index)
  
}

#Plotting player stack sizes as a function of time  
p <- ggplot(final_df, aes(x=time, y=stack)) +
  geom_line(aes(color = name), size = 1) + 
  xlab("time") + 
  scale_fill_manual(values = wes_palette(21, name = "GrandBudapest1", type = "continuous"), name = "") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

raw_buyins <- pokernow[grepleachin(entries, "The admin approved the player"),]
buyin_df <- data.frame(name=character(), value = double(), time =double())


#Grabbing buyins to calculate player nets at given times
for(i in 1: nrow(raw_buyins)){
  temp_buyin <- raw_buyins$entry[i]
  temp_buyin_step <-scan(text=temp_buyin, what='\"', quiet=TRUE)
  temp_amount <- temp_buyin_step[12]
  value <- as.numeric(substr(temp_amount, 1, nchar(temp_amount) - 1))
  if(value %% 1000 == 0){
    value <- value/100 
  }
  name <- strsplit(temp_buyin_step[6], "@")[[1]][1]
  time <- ymd_hms(raw_buyins$at[i],tz=Sys.timezone())
  return_df <- data.frame(name = name, value= value, time = time)
  buyin_df <- rbind(buyin_df, return_df)
}


#Grabbing stack updates to calculate player nets

raw_updates <- pokernow[grepleachin(entries, "The admin updated"),]

for(i in 1: nrow(raw_updates)){
  temp_update <- raw_updates$entry[i]
  temp_update_step <-scan(text=temp_update, what='\"', quiet=TRUE)
  temp_amount <- temp_update_step[9]
  value1 <- as.numeric(substr(temp_amount, 1, nchar(temp_amount) - 1))
  temp_amount2 <- temp_update_step[11]
  value2 <- as.numeric(substr(temp_amount2, 1, nchar(temp_amount) - 1))
  value <- value2 - value1
  name <- strsplit(temp_update_step[6], "@")[[1]][1]
  time <- ymd_hms(raw_updates$at[i],tz=Sys.timezone())
  return_df <- data.frame(name = name, value= value, time = time)
  updates_df <- rbind(updates_df, return_df)
}

updates_df <- updates_df[updates_df$value != 0,]
#Adding buyins and stack updates as factors in net calculation to create a new 
#Net Column 

net_df <- final_df 
net_df$net <- net_df$stack
for(i in 1:nrow(buyin_df)){
  temp <- buyin_df[i,]
  print(temp)
  net_df$net <- ifelse(net_df$name == temp$name & net_df$time > temp$time, net_df$net - temp$value, net_df$net)
}

for(i in 1:nrow(updates_df)){
  print(i)
  temp <- updates_df[i,]
  print(temp)
  if(!(is.na(temp[[1]]))){
    net_df$net <- ifelse(net_df$name == temp$name & net_df$time > temp$time, net_df$net - temp$value, net_df$net)
  }
}

# Adding hand_no to net_graph 

hand_start_entries <- rev(which(grepl("starting hand", entries)))
#rev reverses the order of hands since the first log entry is the last hand  

hand_df <- data.frame(hand_no = double(), time = double())
for(i in hand_start_entries){
  split_entry <- strsplit(entries[i], "#")
  temp_no <-split_entry[[1]][2]
  second_split <- strsplit(temp_no, "\\(")
  hand_no <- as.numeric(second_split[[1]][1])
  time <- ymd_hms(pokernow$at[i],tz=Sys.timezone())
  new_row <- data.frame(hand_no = hand_no, time = time)
  hand_df <- rbind(hand_df, new_row)
}
last_hand_no <- tail(hand_df, 1)$hand_no + 1 
last_hand_time <- ymd_hms(pokernow$at[1],tz=Sys.timezone())
last_hand <- data.frame(hand_no = last_hand_no, time = last_hand_time)
hand_df <- rbind(hand_df, last_hand)

hand <- c() #creating hand column for net_df
for(i in c(1:nrow(net_df))){
  row <- net_df[i,]
  sub.1 <- hand_df %>% filter(time >= row$time, na.rm=TRUE)
  
  hand_no <- sub.1[1, "hand_no"]
  hand <- c(hand, hand_no)
}
net_df$hand <- hand
#Plotting player nets as a function of time 

n <- ggplot(net_df, aes(x=time, y=net)) +
  geom_line(aes(color = name), size = 1) + 
  xlab("time") + 
  scale_fill_manual(values = wes_palette(21, name = "GrandBudapest1", type = "continuous"), name = "") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

h <- ggplot(net_df, aes(x=hand, y=net)) +
  geom_line(aes(color = name), size = 1) + 
  xlab("hand") + 
  scale_fill_manual(values = wes_palette(21, name = "GrandBudapest1", type = "continuous"), name = "") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 