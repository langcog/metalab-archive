  #special cases 
  #in this particular case, there are three groups, two experimental (vowel discrimination and pitch discrimination) and one control (which is actually two control groups collapsed together). Only one exp group (vowel discr) was relevant & entered
  #note that the 2nd row for this study is covered by a t score and does not need the special case calculation
  else if ((unique_ID=="Kuhl1982") {
    special<-as.numeric(unlist(strsplit(as.character(special_cases_measures),";")))
    pooled_SD<-sqrt((n_1[1]* x_1[1]^2 + n_2[1]* x_2[1]^2 + special[1]* special[2]^2  - ((n_1[1]*x_1[1] + n_2[1]*x_2[1] + special[1]*special[2])^2/(n_1[1]+n_2[1]+special[1])))/(special[4]-1)/special[5])
    return(d)
  }
  #We estimate the SD, in this case, from an F that compares the 2 vowel orders within each contrast - it is probably pretty bad because the df are incorrect (there are 40 children in total) 
  #For the German contrast, performance was signifi- cantly poorer for those infants tested with /u/ as the back- ground or reference vowel compared to infants tested with /y/ as the background vowel 􏰂F 􏰅1,32􏰀􏰋17.606, p 􏰈0.0001􏰁.
  #x_1=mean(.524,.524) (average for u->y), x_2=mean(0.824, 0.704) (average for y->u)  Fvalue=17.606
  else if ((unique_ID=="Polka1996" & (contrast_sampa=="dy:t-du:t"|contrast_sampa=="du:t-dy:t")) {
    special<-as.numeric(unlist(strsplit(as.character(special_cases_measures),";")))
    pooled_SD <- sqrt((special[1] * special[2]^2 + special[1] * special[3]^2 - ((special[1] * special[2] + special[1] * special[3])^2/(special[1]+special[1])))/(special[4]-1)/special[5]) 
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  #For the English contrast, performance was poorer for those infants tested with /,/ as the background vowel compared to infants tested with /􏰆/ as the background vowel 􏰂F􏰅1,32􏰀 􏰋19.941, p 􏰈0.0001􏰁.
  #x_1=mean(0.492,.428) (average for ae->E), x_2=mean(0.692, 0.672) (average for E->ae)  Fvalue=19.941 
  else if ((unique_ID=="Polka1996" & (contrast_sampa=="d{t-dEt"|db$contrast_sampa=="dEt-d{t")] ) {
    special<-as.numeric(unlist(strsplit(as.character(special_cases_measures),";")))
    pooled_SD <- sqrt((special[1] * special[2]^2 + special[1] * special[3]^2 - ((special[1] * special[2] + special[1] * special[3])^2/(special[1]+special[1])))/(special[4]-1)/special[5]) 
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  #in this particular case, there are three groups, two experimental and one control. The two exp groups are entered as two subsequent rows, and control group is the same for both
  else if (unique_ID=="Swoboda1976") {
    special<-as.numeric(unlist(strsplit(as.character(special_cases_measures),";")))
    pooled_SD<-sqrt((n_1[1]* x_1[1]^2 + n_2[1]* x_2[1]^2 + special[1]* special[2]^2  - ((n_1[1]*x_1[1] + n_2[1]*x_2[1] + special[1]*special[2])^2/(n_1[1]+n_2[1]+special[1])))/(special[4]-1)/special[5])
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  #We estimate the SD, in this case, from the t that compares the 2 conditions in exp2, and then we attribute it to both conditions
  #n1=n2=16, x1=64.8, x2=77.5, t=9.4
  else if ((unique_ID=="Grieser1989" & expt_num!="exp1") {
    special<-as.numeric(unlist(strsplit(as.character(special_cases_measures),";")))
    pooled_SD <- abs(x_1[1]-special[2])/(special[5]*sqrt((n_1[1]+special[1])/(n_1[1]*special[1])))
    d <- (x_1 - x_2) / pooled_SD  
    return(d)
  }
  
  
