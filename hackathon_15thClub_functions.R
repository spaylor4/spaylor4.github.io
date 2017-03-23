### Helper Functions for 15th Club Hackathon ###


# Get Similar Shots -------------------------------------------------------

similar_shots <- function(shot, comp_data) {
  comp_data <- filter(comp_data, lie_before==shot[["lie_before"]])
  
  #need to get shots of similar distances, but want different intervals for approaches/putts
  #putts +/- 12 inches, inside 30 yards +/- 36 inches, 30-75 yards +/- 90 inches, approach/layups/par3tees +/- 180,
  #drives par4/5s +/- 360 inches
  if(shot[["lie_before"]] == "Tee" & shot[["par"]] %in% c(4,5)) {
    comp_data <- filter(comp_data, left_to_pin_before >= shot[["left_to_pin_before"]] - 360, 
                        left_to_pin_before <= shot[["left_to_pin_before"]] + 360,
                        lie_before == shot[["lie_before"]])
  } else if(shot[["left_to_pin_before"]] > 2700) {
    comp_data <- filter(comp_data, left_to_pin_before >= shot[["left_to_pin_before"]] - 180, 
                        left_to_pin_before <= shot[["left_to_pin_before"]] + 180)
  } else if(shot[["left_to_pin_before"]] > 1080) {
    comp_data <- filter(comp_data, left_to_pin_before >= shot[["left_to_pin_before"]] - 90, 
                        left_to_pin_before <= shot[["left_to_pin_before"]] + 90)
  } else if(shot[["lie_before"]] != "Green") {
    comp_data <- filter(comp_data, left_to_pin_before >= shot[["left_to_pin_before"]] - 36, 
                        left_to_pin_before <= shot[["left_to_pin_before"]] + 36)
  } else {
    comp_data <- filter(comp_data, left_to_pin_before >= shot[["left_to_pin_before"]] - 12, 
                        left_to_pin_before <= shot[["left_to_pin_before"]] + 12)
  }
  
  return(comp_data)
}


# Baseline Averages -------------------------------------------------------

baselines <- function(shot, sim_shots) {
  
  avg_strokes <- mean(sim_shots[["score"]] - sim_shots[["shot_no"]] + 1)
  return(avg_strokes)
  
}

# Calculate Strokes Gained ------------------------------------------------


strokes_gained <- function(player_data) {
  
  #calculate average number of shots needed from similar spot for each shot
  avg_strokes <- c(1:nrow(player_data))  #empty vector to fill
  numb_similar <- c(1:nrow(player_data))
  for(i in 1:nrow(player_data)) {
    shot <- player_data[i,]
    sim_shots <- similar_shots(shot, player_data)
    numb_similar[i] <- nrow(sim_shots)
    avg_strokes[i] <- baselines(shot, sim_shots)
  }
  new_data <- cbind(player_data, avg_strokes, numb_similar)
  
  #get strokes gained by average shots of before position - average shots of after position - 1
  strokes_gained <- c(1:nrow(player_data))
  for(i in 1:nrow(player_data)) {
    shot <- new_data[i,]
    if(shot[["shot_no"]] == shot[["score"]]) {
      avg_of_next <- 0
    } else {
      if (shot[["lie_after"]] %in% c("Water", "Out of Bounds")) {
        next_shot <- filter(new_data, name == shot[["name"]], round_no == shot[["round_no"]], 
                            hole_no == shot[["hole_no"]], shot_no == shot[["shot_no"]] + 2)
      } else {
        next_shot <- filter(new_data, name == shot[["name"]], round_no == shot[["round_no"]], 
                            hole_no == shot[["hole_no"]], shot_no == shot[["shot_no"]] + 1)
      }
      if(nrow(next_shot)==0) {
        next_shot <- filter(new_data, name == shot[["name"]], round_no == shot[["round_no"]], 
                            hole_no == shot[["hole_no"]], shot_no == shot[["shot_no"]] + 2)
      }
      avg_of_next <- next_shot[["avg_strokes"]]
    }
    
    if(is.na(avg_of_next) | is.na(shot[["avg_strokes"]])) {
      strokes_gained[i] <- NA
    } else {
      strokes_gained[i] <- shot[["avg_strokes"]] - avg_of_next - 1
    }
  }
  new_data <- cbind(new_data, strokes_gained)
  return(new_data)
  
}


