## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX 


cvd_country_LatentInfections = function(curDate, cname, jhudat, jhudat2,  
                                   countryNamePop, ifr0, ifrL, ifrU){

  outPath = file.path(getwd(), 'output', 'countries', cname, 
                      paste0(curDate))
  if (!dir.exists(outPath)) dir.create(outPath, recursive = T)
  outPath2 = file.path(getwd(), 'output', 'countries_current')
  if (!dir.exists(outPath2)) dir.create(outPath2, recursive = T)
  #
  head(jhudat) 
  class(jhudat)
  names(jhudat)  
  
  # X population: countryNamePop 
  mycname = countryNamePop$Region
  stInd = which(mycname == paste0(cname))
  if (length(stInd) == 0){ return() }
  Xpop = countryNamePop$pop2019[stInd]
  
  # convert url dataset into a data frame  
  ind = which((jhudat$Country.Region == cname) & (jhudat$Province.State == ''))
  stdat1 = jhudat[ind, ]
  stdat1_body = stdat1[, c(5:ncol(stdat1))]
  stdat3 = data.frame(date = colnames(stdat1_body), val = cbind(as.numeric(stdat1_body)))
  colnames(stdat3) = c('date', 'positive')
  tail(stdat3)
  # deaths
  ind = which((jhudat2$Country.Region == cname) & (jhudat2$Province.State == ''))
  stdat12 = jhudat2[ind, ]
  stdat1_body2 = stdat12[, c(5:ncol(stdat12))]   # 
  val2 = cbind(as.numeric(stdat1_body2))
  stdat3$death = val2
  stdat3$numTests = NA 
  stdat3$recovered = NaN;       # recovered is not used
  tail(stdat3)

  stdat3$death[is.na(stdat3$death)] = 0
  
  # preprocessing: make sure non-decreasing TS
  # all(cummax(stdat3$positive) == stdat3$positive)
  stdat3$positive = cummax(stdat3$positive)
  stdat3$death = cummax(stdat3$death)
  stdat3$numTests = cummax(stdat3$numTests)

  # starting date V2
  iddeath = which(stdat3$death > 0)[1]
  id0 = max(1, iddeath + 2)

  ids = seq(from=id0, to = nrow(stdat3), by = 1)
  stdat4 = stdat3[ids, ]
  #
  date1 = stdat4$date
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
  # so far, url dataset is preprocessed and prepared into the input dataFrame, stdat4
  
  # core algorithm based on Expectation-Maximization
  # describe_df() in cvd_subftns.R
  print('==')
  print(cname)
  print('==')
  outdat = describe_df(curDate, cname, stdat4, Xpop, outPath, outPath2, ifr0, ifrL, ifrU)
}


## 
## EOF