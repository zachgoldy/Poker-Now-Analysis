library(tidyverse)
library(lubridate)
library(wesanderson)
setwd("~/Downloads") #ADD POKERNOW DIRECTORY HERE
pokernow <- read.csv("poker_now_log_kF34l6-SpSG0wNQh1d-XNsp1r.csv") #INSERT POKER NOW CSV HERE 
entries <- pokernow$entry
entry_stack <- c()

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


hand_start_entries <- rev(which(grepl("starting hand", entries)))
#rev reverses hand_entries to parse from start to end 
hand_df <- data.frame(hand_no = double(), pot_size = double(), 
                      winner = character(), winning_hand = character(), 
                      participants = character(), players = character())

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
  
  ####Finding available players during a hand for vpip calculation 
  raw_stacks <- hand_entries[grepleachin(hand_entries, "Player stacks")]
  temp1 <- scan(text=raw_stacks, what='"', quiet=TRUE)
  name_range <- seq(4, length(temp1), by = 4)
  
  players <- c()
  for(i in name_range){
    temp3 <- temp1[i]
    name_val <- strsplit(temp3, "@")[[1]][[1]]
    name_val <- gsub("[^[:alnum:][:space:]]","",name_val)
    players <- c(players, name_val)
  }
  players <- toString(players)
  ########
  
  
  raw_pot_result  <- hand_entries[grepl("collected", hand_entries)]
  temp_step <-  scan(text=raw_pot_result, what='"', quiet=TRUE)
  winner <- strsplit(temp_step[1], "@")[[1]][1]
  winning_hand <- "Hand Not Shown"
  if(length(temp_step) > 6){
    winning_hand <- paste(temp_step[7:length(temp_step)], collapse = ' ')
  }
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
                      winner = winner, winning_hand=winning_hand, 
                      participants = participants, players = players)
  }
  else{
    row <- data.frame(hand_no = hand_no, pot_size = pot_size, 
                      winner = winner, winning_hand = winning_hand, participants = NA, players = players)
  }
  hand_df <- rbind(hand_df, row)
  
  
}



names <- unique(hand_df$winner)
vpip_df <- data.frame(name = character(), vpip = double())
for(i in names){
  name <- gsub("[^[:alnum:][:space:]]","",i)
  hands_played_in <- hand_df[grepl(name, hand_df$players),]
  hands_participated_in <- hand_df[grepl(name, hand_df$participants),]
  
  vpip <- nrow(hands_participated_in) / nrow(hands_played_in) 
  temp_row <- data.frame(name = name, vpip = vpip)
  vpip_df <- rbind (vpip_df, temp_row)
}

biggest_hands <- hand_df[
  with(hand_df, order(-pot_size)),
]

biggest_pots <- "Biggest Hands:\n"
for(i in c(1,2,3)){
biggest_pots <- paste(biggest_pots, "\nA $", format(biggest_hands[i,2], nsmall=2), " pot was taken down by ", 
                      biggest_hands[i,3], "\nWinning Hand: ", biggest_hands[i,4], 
                      "\nParticipants: ", biggest_hands[i, 5], "\n", sep="")

}

biggest_pots <- cat(biggest_pots)