## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX 


cvd_state_LatentInfections = function(curDate, stname, jhuStateDat, jhuStateDat2, 
                                      stpopulationData, ifr0, ifrL, ifrU){
  #options(bitmapType = 'cairo')
  # (no pop adj.)
  ## X population: USstatesPopulation_USCensusBureau_simplified 
  data(state)
  myStAbb = c(state.abb, 'DC')
  myStName = c(state.name, 'District of Columbia')
  
  stInd = which(myStAbb == stname)
  if (length(stInd) == 0){ return() }
  
  outPath = file.path(getwd(), 'output', 'states', stname, 
                      paste0(curDate))
  if (!dir.exists(outPath)) dir.create(outPath, recursive = T)
  outPath2 = file.path(getwd(), 'output', 'states_current')
  if (!dir.exists(outPath2)) dir.create(outPath2, recursive = T)
  
  
  stfullname = myStName[stInd]
  stfullname2 = paste0('.', stfullname)
  #stpopulationData = read.csv('USstatesPopulation_USCensusBureau_simplified.csv')
  stInd2 = which(stpopulationData$States == stfullname2)
  Xpop = stpopulationData$Est2019_Population[stInd2]
  
  # head(covidtrackingDat) 
  head(jhuStateDat)

  #
  sid = which( (jhuStateDat$StateAbb == stname) & !is.na(jhuStateDat$StateAbb) )
  stdat1 = jhuStateDat[sid, ]
  #stdat2 = stdat1[nrow(stdat1):1, ]
  stdat1_body = stdat1[, c(3:ncol(stdat1))]
  stdat3 = data.frame(date = colnames(stdat1_body), val = cbind(as.numeric(stdat1_body)))
  colnames(stdat3) = c('date', 'positive')
  tail(stdat3)
  # deaths
  sid = which( (jhuStateDat2$StateAbb == stname) & !is.na(jhuStateDat2$StateAbb) )
  stdat12 = jhuStateDat2[sid, ]
  stdat1_body2 = stdat12[, c(3:ncol(stdat12))]   # good job JHU
  val = cbind(as.numeric(stdat1_body2))
  stdat3$death = val
  stdat3$numTests = NaN
  stdat3$recovered = NaN
  tail(stdat3)
  #if (!all(is.na(stdat3$recovered))) {
  stdat3$recovered[is.nan(stdat3$recovered)] = 0
  if (all(stdat3$recovered == 0)) {stdat3$recovered = NaN}
  #}
  stdat3$death[is.na(stdat3$death)] = 0
  
  # preprocessing: make sure non-decreasing TS
  # all(cummax(stdat3$positive) == stdat3$positive)
  stdat3$positive = cummax(stdat3$positive)
  stdat3$death = cummax(stdat3$death)
  
  
  # data since 20200314 ->0305 xx
  #iddeath = which(stdat3$death > 0)[1]
  #id0 = max(1, iddeath + 2)
  id0 = which(stdat3$date == 'X3.13.20')
  ids = seq(from=id0, to = nrow(stdat3), by = 1)
  stdat4 = stdat3[ids, ]
  #stdat4 = stdat3
  #
  date1 = stdat4$date
  #date_mmdd =       # date1 %% 10000
  #date_mm = as.numeric(substr(date1, 2, 2))           # date_mmdd %/% 100
  #date_dd = as.numeric(substr(date1, 4, 5))           # date_mmdd %% 100 
  tmp = unlist(strsplit(as.character(date1), '[.]'))
  mm_tmp = tmp[seq(1, length(tmp), by=3)]
  date_mm = as.numeric(substr(mm_tmp, 2, 3))  
  date_dd = as.numeric(tmp[seq(2, length(tmp), by=3)])
  date_yy = as.numeric(tmp[seq(3, length(tmp), by=3)])
  
  mydates = as.Date('2020-03-05')
  for (i in 1:length(date1)){
    mydates[i] = as.Date(paste0("20", date_yy[i], "-", date_mm[i], "-", date_dd[i]))
  }
  
  #
  stdat4$mydates = mydates
  
  
  ## Daily new confirmed cases
  dif_cases = c(NA, diff(stdat4$positive))  
  dif_tests = c(NA, diff(stdat4$numTests))  
  dif_recovered = c(NA, diff(stdat4$recovered))  
  dif_death = c(NA, diff(stdat4$death))  
  #d_cases = dif_cases/2
  #d_tests = dif_tests/2
  stdat4$dif_cases = dif_cases
  stdat4$dif_tests = dif_tests
  stdat4$dif_recovered = dif_recovered
  stdat4$dif_death = dif_death
  
  # 08/03
  weight0 = rep(1,7)/7
  stdat4$positive7 = naFilter(stdat4$positive, weight0)
  stdat4$dif_pos7 = pmax(0, c(NA, diff(stdat4$positive7)))
  stdat4$numTests7 = filter(stdat4$numTests, weight0, 'convolution', sides=1)
  stdat4$dif_tst7 = c(NA, diff(stdat4$numTests7))
  
  stdat4$testPosRate7 = stdat4$dif_pos7 / stdat4$dif_tst7 * 100
  
  # pctTestPos7 -> testPos7 -> testPosRate7
  
  
  ##
  ##  modules
  ##
  print('==')
  print(stname)
  print('==')
  outdat = describe_df(curDate, stname, stdat4, Xpop, outPath, outPath2, ifr0, ifrL, ifrU)
  
  #return(outdat) 
}
  

## 
## EOF
