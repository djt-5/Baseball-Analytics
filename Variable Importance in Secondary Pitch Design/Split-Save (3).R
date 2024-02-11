{
  #Split based on sequence combinations for RHP vs RHH
  library(tidyverse)
  RHP_RHH <- read_csv("RHP_RHH.csv")
  RHP_RHH_List <- split(RHP_RHH, RHP_RHH$Sequence)
  
  #Save
  
  write_csv(RHP_RHH_List[[1]],"FC_CH.csv")
  write_csv(RHP_RHH_List[[3]],"FC_CU.csv")
  write_csv(RHP_RHH_List[[4]],"FC_FS.csv")
  write_csv(RHP_RHH_List[[5]],"FC_KC.csv")
  write_csv(RHP_RHH_List[[7]],"FC_SL.csv")
  write_csv(RHP_RHH_List[[8]],"FC_ST.csv")
  write_csv(RHP_RHH_List[[10]],"FF_CH.csv")
  write_csv(RHP_RHH_List[[12]],"FF_CU.csv")
  write_csv(RHP_RHH_List[[13]],"FF_FS.csv")
  write_csv(RHP_RHH_List[[14]],"FF_KC.csv")
  write_csv(RHP_RHH_List[[17]],"FF_SL.csv")
  write_csv(RHP_RHH_List[[18]],"FF_ST.csv")
  write_csv(RHP_RHH_List[[20]],"SI_CH.csv")
  write_csv(RHP_RHH_List[[22]],"SI_CU.csv")
  write_csv(RHP_RHH_List[[23]],"SI_FS.csv")
  write_csv(RHP_RHH_List[[24]],"SI_KC.csv")
  write_csv(RHP_RHH_List[[26]],"SI_SL.csv")
  write_csv(RHP_RHH_List[[27]],"SI_ST.csv")
  write_csv(RHP_RHH_List[[28]],"SI_SV.csv")
}

{
  #RHP vs LHH
  library(tidyverse)
  RHP_LHH <- read_csv("RHP_LHH.csv")
  RHP_LHH_List <- split(RHP_LHH, RHP_LHH$Sequence)
  
  #Save
  
  write_csv(RHP_LHH_List[[1]],"FC_CH.csv")
  write_csv(RHP_LHH_List[[3]],"FC_CU.csv")
  write_csv(RHP_LHH_List[[4]],"FC_FS.csv")
  write_csv(RHP_LHH_List[[5]],"FC_KC.csv")
  write_csv(RHP_LHH_List[[7]],"FC_SL.csv")
  write_csv(RHP_LHH_List[[8]],"FC_ST.csv")
  write_csv(RHP_LHH_List[[10]],"FF_CH.csv")
  write_csv(RHP_LHH_List[[12]],"FF_CU.csv")
  write_csv(RHP_LHH_List[[13]],"FF_FS.csv")
  write_csv(RHP_LHH_List[[14]],"FF_KC.csv")
  write_csv(RHP_LHH_List[[17]],"FF_SL.csv")
  write_csv(RHP_LHH_List[[18]],"FF_ST.csv")
  write_csv(RHP_LHH_List[[20]],"SI_CH.csv")
  write_csv(RHP_LHH_List[[22]],"SI_CU.csv")
  write_csv(RHP_LHH_List[[23]],"SI_FS.csv")
  write_csv(RHP_LHH_List[[24]],"SI_KC.csv")
  write_csv(RHP_LHH_List[[26]],"SI_SL.csv")
  write_csv(RHP_LHH_List[[27]],"SI_ST.csv")
}

{
  #LHP vs RHH
  library(tidyverse)
  LHP_RHH <- read_csv("LHP_RHH.csv")
  LHP_RHH_List <- split(LHP_RHH, LHP_RHH$Sequence)
  
  #Save
  
  write_csv(LHP_RHH_List[[1]],"FC_CH.csv")
  write_csv(LHP_RHH_List[[3]],"FC_CU.csv")
  write_csv(LHP_RHH_List[[7]],"FC_SL.csv")
  write_csv(LHP_RHH_List[[10]],"FF_CH.csv")
  write_csv(LHP_RHH_List[[12]],"FF_CU.csv")
  write_csv(LHP_RHH_List[[13]],"FF_FS.csv")
  write_csv(LHP_RHH_List[[14]],"FF_KC.csv")
  write_csv(LHP_RHH_List[[16]],"FF_SL.csv")
  write_csv(LHP_RHH_List[[17]],"FF_ST.csv")
  write_csv(LHP_RHH_List[[19]],"SI_CH.csv")
  write_csv(LHP_RHH_List[[20]],"SI_CU.csv")
  write_csv(LHP_RHH_List[[22]],"SI_KC.csv")
  write_csv(LHP_RHH_List[[24]],"SI_SL.csv")
  write_csv(LHP_RHH_List[[25]],"SI_ST.csv")
}

{
  #LHP vs LHH
  library(tidyverse)
  LHP_LHH <- read_csv("LHP_LHH.csv")
  LHP_LHH_List <- split(LHP_LHH, LHP_LHH$Sequence)
  
  #Save
  
  write_csv(LHP_LHH_List[[1]],"FC_CH.csv")
  write_csv(LHP_LHH_List[[2]],"FC_CU.csv")
  write_csv(LHP_LHH_List[[8]],"FF_CH.csv")
  write_csv(LHP_LHH_List[[9]],"FF_CU.csv")
  write_csv(LHP_LHH_List[[11]],"FF_KC.csv")
  write_csv(LHP_LHH_List[[13]],"FF_SL.csv")
  write_csv(LHP_LHH_List[[14]],"FF_ST.csv")
  write_csv(LHP_LHH_List[[16]],"SI_CH.csv")
  write_csv(LHP_LHH_List[[17]],"SI_CU.csv")
  write_csv(LHP_LHH_List[[19]],"SI_KC.csv")
  write_csv(LHP_LHH_List[[21]],"SI_SL.csv")
}

