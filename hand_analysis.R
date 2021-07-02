library(tidyverse)
library(lubridate)
library(wesanderson)
setwd("~/Downloads") #ADD POKERNOW DIRECTORY HERE
pokernow <- read.csv("poker_now_log_3YT_eWKribu5znUsVt7_1VWc3.csv") #INSERT POKER NOW CSV HERE 
entries <- pokernow$entry
entry_stack <- c()

hand_start_entries <- rev(which(grepl("starting hand", entries)))
#rev reverses hand_entries to parse from start to end 
hand_df <- data.frame(hand_no = double(), pot_size = double(), 
                      winner = character(), participants = character())

class(hand_df$participants) <- "list"
for(i in hand_start_entries){
  split_entry <- strsplit(entries[i], "#")
  temp_no <-split_entry[[1]][2]
  second_split <- strsplit(temp_no, "\\(")
  hand_no <- as.numeric(second_split[[1]][1])
  next_index <- hand_start_entries[which(hand_start_entries == i) + 1]
  if(is.na(entries[next_index])){
    next_index <<- 1
  }
  hand_entries <- entries[i:next_index  + 1]
  raw_pot_result  <- hand_entries[grepl("collected", hand_entries)]
  temp_step <-  scan(text=raw_pot_result, what='"', quiet=TRUE)
  winner <- strsplit(temp_step[1], "@")[[1]][1]
  pot_size <- as.numeric(temp_step[3])
  temp_participants <- hand_entries[grepl("calls|raises", hand_entries)]
  temp_participants.2 <- strsplit(temp_participants, "@")
  participants <- c()
  
  if(length(temp_participants.2) != 0){
    for(i in c(1: length(temp_participants.2))){
      temp <- temp_participants.2[[i]][1]
      temp.participant <- gsub("[^[:alnum:][:space:]]","",temp)
      participants <- c(participants, temp.participant)
      
    }
  }
  participants <- toString(unique(participants))
  if(length(temp_participants.2) != 0 ){
  row <- data.frame(hand_no = hand_no, pot_size = pot_size, 
                    winner = winner, participants = participants)
  }
  else{
    row <- data.frame(hand_no = hand_no, pot_size = pot_size, 
                      winner = winner, participants = NA)
  }
  hand_df <- rbind(hand_df, row)
  
  
}



names <- unique(hand_df$winner)
vpip_df <- data.frame(name = character(), vpip = double())
for(i in names){
  name <- gsub("[^[:alnum:][:space:]]","",i)
  hands_played_in <- hand_df[grepl(name, hand_df$participants),]
  
  vpip <- nrow(hands_played_in) / nrow(hand_df) 
  temp_row <- data.frame(name = name, vpip = vpip)
  vpip_df <- rbind (vpip_df, temp_row)
}
