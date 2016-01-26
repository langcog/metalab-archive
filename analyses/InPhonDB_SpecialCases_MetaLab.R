  else if ((!is.na(participant_design) && participant_design == "within_two") || (!is.na(between_participant) && between_participant == 0)) {
  if (is.na(corr)) {
    corr <- study$corr_imputed
    #corr <- 1
  }
  if (all(!is.na(x_1), !is.na(x_2), !is.na(SD_1), !is.na(SD_2))) {
    pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2)
    d <- (x_1 - x_2) / pooled_SD
    return(d)
  }
  #special cases 
  #in this particular case, there are three groups, two experimental and one control (which is actually two control groups collapsed together). Only one exp group was relevant & entered
  #k=3 (groups in this ANOVA) n_1=n_2=8 n_3=16 x_1=.2 x_2=.21 x_3=-.07 Fvalue=10.34
  else if ((unique_ID=="Kuhl1982") {
    pooled_SD <- sqrt((8 * 0.2^2 + 8 * 0.21^2 + 16 * (-0.07)^2  - ((8 *0.2 + 8 *0.21 + 16 *(-0.07))^2/(8+8+16)))/(3-1)/10.34) 
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  #We estimate the SD, in this case, from an F that compares the 2 vowel orders within each contrast - it is probably pretty bad because the df are incorrect (there are 40 children in total) 
  #k=3 (groups in this ANOVA) n_1=n_2=20
  #For the German contrast, performance was signifi- cantly poorer for those infants tested with /u/ as the back- ground or reference vowel compared to infants tested with /y/ as the background vowel 􏰂F 􏰅1,32􏰀􏰋17.606, p 􏰈0.0001􏰁.
  #x_1=mean(.524,.524) (average for u->y), x_2=mean(0.824, 0.704) (average for y->u)  Fvalue=17.606
  else if ((unique_ID=="Polka1996" & (contrast_sampa=="dy:t-du:t"|contrast_sampa=="du:t-dy:t")] ) {
    pooled_SD <- sqrt((20 * mean(.524,.524)^2 + 20 * mean(0.824, 0.704)^2 - ((20 * mean(.524,.524) + 20 * mean(0.824, 0.704))^2/(20+20)))/(2-1)/17.606) 
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  #For the English contrast, performance was poorer for those infants tested with /,/ as the background vowel compared to infants tested with /􏰆/ as the background vowel 􏰂F􏰅1,32􏰀 􏰋19.941, p 􏰈0.0001􏰁.
  #x_1=mean(0.492,.428) (average for ae->E), x_2=mean(0.692, 0.672) (average for E->ae)  Fvalue=19.941 
  else if ((unique_ID=="Polka1996" & (contrast_sampa=="dy:t-du:t"|contrast_sampa=="du:t-dy:t")] ) {
    pooled_SD <- sqrt((20 * mean(0.492,.428)^2 + 20 * mean(0.692, 0.672)^2 - ((20 * mean(0.492,.428) + 20 * mean(0.692, 0.672))^2/(20+20)))/(2-1)/19.941) 
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  #in this particular case, there are three groups, two experimental and one control. The two exp groups are entered as two subsequent rows, and control group is the same for both
  #k=3 (groups in this ANOVA) n1=n2=n3=20 x_1 = 6.88, x_2=5.83, x_3=-2.8 Fvalue= 4.49
  else if ((unique_ID=="Swoboda1976") {
  pooled_SD <- sqrt((20 * 6.88^2 + 20 * 5.83^2 + 20 * (-2.8)^2  - ((20 * 6.88 + 20 * 5.83 + 20 *(-2.8))^2/(20+20+20)))/(3-1)/4.49) 
  d <- (x_1 - x_2) / pooled_SD  
  return(d)
  }
  #We estimate the SD, in this case, from the t that compares the 2 conditions in exp2, and then we attribute it to both conditions
  #n1=n2=16, x1=64.8, x2=77.5, t=9.4
  else if ((unique_ID=="Grieser1989" & exp_num!="exp1") {
    pooled_SD <- abs(64.8-77.5)/(9.4*sqrt((16+16)/(16*16)))
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  