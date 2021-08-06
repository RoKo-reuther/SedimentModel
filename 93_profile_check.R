
###########################################################################################################
#                                            MASS BALANCE CHECKS                                          #
###########################################################################################################

draw_depth_profile <- function(selection){
  #*************************************
  # depth vector
  #*************************************
  depth_p <- -1*grid_collection$grid$x.mid
  
  
  #*************************************
  # OM-degradation
  #*************************************
  R1a <- data.frame(class="R1a", value=ss$R1a)
  RNa <- data.frame(class="RNa", value=ss$RNa)
  RMa <- data.frame(class="RMa", value=ss$RMa)
  R2a_Ox <- data.frame(class="R2a_Ox", value=ss$R2a_Ox)
  R2a_P <- data.frame(class="R2a_P", value=ss$R2a_P)
  R3a <- data.frame(class="R3a", value=ss$R3a)
  R4a <- data.frame(class="R4a", value=ss$R4a)
  
  OM <<- rbind(R1a, RNa, RMa, R2a_Ox, R2a_P, R3a, R4a)
  
  
  #*************************************
  # Mn2+
  #*************************************
  RMn_2 <- data.frame(class="RMn_2", value=ss$RMn_2)
  RMa <- data.frame(class="RMa", value=ss$RMa)
  RMb <- data.frame(class="RMb", value=ss$RMb)
  R27 <- data.frame(class="R27", value=ss$R27)
  R28 <- data.frame(class="R28", value=ss$R28)
  R29_Ox <- data.frame(class="R29_Ox", value=ss$R29_Ox)
  R29_P <- data.frame(class="R29_P", value=ss$R29_P)
  R30 <- data.frame(class="R30", value=ss$R30)
  R34 <- data.frame(class="R34", value=ss$R34)
  
  Mn_2 <<- rbind(RMn_2, RMa, RMb, R27, R28, R29_Ox, R29_P, R30, R34)
  
  
  #*************************************
  # MnO2A
  #*************************************
  RMnO2A <- data.frame(class="RMnO2A", value=ss$RMnO2A)
  RMa <- data.frame(class="RMa", value=ss$RMa)
  RMb <- data.frame(class="RMb", value=ss$RMb)
  R28 <- data.frame(class="R28", value=ss$R28)
  R29_Ox <- data.frame(class="R29_Ox", value=ss$R29_Ox)
  R29_P <- data.frame(class="R29_P", value=ss$R29_P)
  R30 <- data.frame(class="R30", value=ss$R30)
  R34 <- data.frame(class="R34", value=ss$R34)
  
  MnO2A <<- rbind(RMnO2A, RMa, RMb, R28, R29_Ox, R29_P, R30, R34)
  
  
  #*************************************
  # draw profiles
  #*************************************
  draw_frame <- cbind(depth_p, selection)
  
  ggplot(data = draw_frame) + 
    geom_path(mapping = aes(x = value, y = depth_p, color = class))
}

draw_depth_profile(Mn_2)
