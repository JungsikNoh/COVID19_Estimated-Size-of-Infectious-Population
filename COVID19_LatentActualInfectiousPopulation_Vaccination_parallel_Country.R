## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX
## 
## Run COVID19_LatentActualInfectiousPopulation.R -> summariesOverRegions.R
##      -> reporter.R
#
# Updates: 
# J Noh, 08/24/2020. Adjust caption size due to transition to 'cairo' graphics. 

##
##  1. source functions ####################################
##  

options(bitmapType = 'cairo')

#curDate = Sys.Date(); print(curDate)
curDate = '2021-06-23'
print(curDate)

#setwd( )
print(getwd())

source(file.path(getwd(), 'cvd_subftns.R'))
source(file.path(getwd(), 'cvd_county_LatentInfections.R'))
source(file.path(getwd(), 'cvd_state_LatentInfections.R'))
source(file.path(getwd(), 'cvd_country_LatentInfections.R'))

library(ggplot2)
library(data.table)
library(formattable)
library(ggpubr)
#library(RColorBrewer)
#library(ggsci)
library(wesanderson)
library(tseries)
library(penalized)

library(parallel)
library(doParallel)

##
##  2. Fetch data   ###########################################
##

## Countries
# fetch JHU
urlJhu = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
#urlJhu = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
jhudat = read.csv(urlJhu, head=T)

# china curation
idPrv = which(jhudat$Country.Region=='China')
chinaPrv = jhudat[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='China', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat = rbind(chinaDP, jhudat[-idPrv, ])
head(jhudat)
# canada curation
idPrv = which(jhudat$Country.Region=='Canada')
chinaPrv = jhudat[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='Canada', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat = rbind(chinaDP, jhudat[-idPrv, ])
print(head(jhudat))

write.csv(jhudat, file.path(getwd(), 'JHU_CSSE_covid19_confirmed_global.csv'))

##
## fetch CSSE deaths_global
urlJhu2 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
jhudat2 = read.csv(urlJhu2, head=T)

# china curation
idPrv = which(jhudat2$Country.Region=='China')
chinaPrv = jhudat2[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat2)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='China', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat2 = rbind(chinaDP, jhudat2[-idPrv, ])
head(jhudat2)
# canada curation
idPrv = which(jhudat2$Country.Region=='Canada')
chinaPrv = jhudat2[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat2)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='Canada', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat2 = rbind(chinaDP, jhudat2[-idPrv, ])
head(jhudat2)

write.csv(jhudat2, file.path(getwd(), 'JHU_CSSE_covid19_deaths_global.csv'))

##
## fetch CSSE recovered_global
urlJhu3 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
jhudat3 = read.csv(urlJhu3, head=T)

# china curation
idPrv = which(jhudat3$Country.Region=='China')
chinaPrv = jhudat3[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat3)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='China', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat3 = rbind(chinaDP, jhudat3[-idPrv, ])
head(jhudat3)
# canada curation
idPrv = which(jhudat3$Country.Region=='Canada')
chinaPrv = jhudat3[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat3)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='Canada', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat3 = rbind(chinaDP, jhudat3[-idPrv, ])
head(jhudat3)

write.csv(jhudat3, file.path(getwd(), 'JHU_CSSE_covid19_recovered_global.csv'))


## States
## fetch states data from covidtracking.com
#url2 = 'https://covidtracking.com/api/v1/states/daily.csv'
#covidtrackingDat = read.csv(url2, head=T)
## daily input dataset
#write.csv(covidtrackingDat, file.path(getwd(), 'covidtracking_dot_com.csv'))
#print(head(covidtrackingDat[, 1:7]))


Sys.sleep(5)

## TX counties
## fetch TX county data from JHU
url_jhuCounty = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
jhuCountyDat = read.csv(url_jhuCounty, head=T)
# counties in Texas
ind = (jhuCountyDat$Province_State == 'Texas')
TXcountyDat = jhuCountyDat[ind, ]
# daily input dataset
write.csv(TXcountyDat, file.path(getwd(), 'TXcounty_confirmed_JHUCSSE.csv'))
print(tail(TXcountyDat))


## fetch TX county deaths from JHU
url_jhuCounty2 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv'
jhuCountyDat2 = read.csv(url_jhuCounty2, head=T)
# counties in Texas
ind = (jhuCountyDat2$Province_State == 'Texas')
TXcountyDat2 = jhuCountyDat2[ind, ]
# daily input dataset
write.csv(TXcountyDat2, file.path(getwd(), 'TXcounty_deaths_JHUCSSE.csv'))
print(tail(TXcountyDat2))


## Population data
# csv input files
basicDatasetsDir = file.path(getwd(), 'basicDatasets')
populationData = read.csv(file.path(basicDatasetsDir, 'usItalyKorea_Population2020UN.csv'))
stpopulationData =
  read.csv(file.path(basicDatasetsDir, 'USstatesPopulation_USCensusBureau_simplified.csv'))

# UNpop2019Dat
UNpop2019Dat = read.csv(file.path(basicDatasetsDir, 'UN_WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES_2019.csv'))

# read TX county population 
popTXcounty= read.csv(file.path(basicDatasetsDir, 'worldpopulationReviewdotcom_2020_TexasCounty.csv'))



##
## 3. Global parameters   ############################
##

#totalCases_threshold_toSetStart = 100 
# Infection Fatality Rate
#ifr0 = 0.01
ifr0 = 0.0066 
ifrL = 0.0039
ifrU = 0.0133



## 
##  4. Top 50 countries   ###################################
## 

t1 = Sys.time()
numCntr = 50

jhudatL = jhudat[, c(2, ncol(jhudat))]
head(jhudatL)
scases = sort(jhudatL[, 2], index.return = T, decreasing = T)
jhudatL2 = jhudatL[scases$ix, ]
print(jhudatL2[1:numCntr, ])
namesTop20 = as.character(jhudatL2$Country.Region[1:numCntr])

# Korea is not in top 25 (04/21 revised)
namesTop20 = c(namesTop20, 'Korea, South')

# name curation -.-;; 
namesTop20_1 = namesTop20
namesTop20_1[(namesTop20 == 'US')] = 'United States of America'
namesTop20_1[(namesTop20 == 'Korea, South')] = 'Republic of Korea'
namesTop20_1[(namesTop20 == 'Iran')] = 'Iran (Islamic Republic of)'
namesTop20_1[(namesTop20 == 'Russia')] = 'Russian Federation'
namesTop20_1[(namesTop20 == 'Bolivia')] = 'Bolivia (Plurinational State of)'
namesTop20_1[(namesTop20 == 'Venezuela')] = 'Venezuela (Bolivarian Republic of)'



# pop preprocessing
countryNamePop = data.frame(Region = namesTop20, namesTop20_1, pop2019 = 1:length(namesTop20))
for (i in 1:nrow(countryNamePop)){
  pop0 = UNpop2019Dat$X2019[which(UNpop2019Dat$Region == namesTop20_1[i])]
  countryNamePop$pop2019[i] = pop0
}
print(countryNamePop$Region)


t1=Sys.time()
# run countries 
cl = makeCluster(51)
registerDoParallel(cl)
clusterCall(cl, function() options(bitmapType = 'cairo'))
print(cl)

outLst_country = list()

outLst_country <- foreach(i = 1:(numCntr+1), 
                    .packages = c('ggplot2', 'data.table','formattable',
                                  'ggpubr','RColorBrewer','ggsci','wesanderson','tseries','penalized')) %dopar% {  
  stname = as.character(countryNamePop$Region[i])
  tmp = cvd_country_LatentInfections(curDate, stname, jhudat, jhudat2, jhudat3, 
                                countryNamePop, ifr0, ifrL, ifrU)
}
stopCluster(cl)
  
t2=Sys.time(); print(t2-t1)




## 
##  5. TX Counties from JHU   ##############################
## 

numCnty = 15 


# county sorting
TXcountyDatL = TXcountyDat[, c(6, ncol(TXcountyDat))]
head(TXcountyDatL)
scases = sort(TXcountyDatL[, 2], index.return = T, decreasing = T)
TXcountyDatL2 = TXcountyDatL[scases$ix, ]
print(TXcountyDatL2[1:numCnty, ])
sortedCounties = as.character(TXcountyDatL2$Admin2[1:numCnty])
print(sortedCounties)


# run counties 
cl = makeCluster(15)
registerDoParallel(cl)
clusterCall(cl, function() options(bitmapType = 'cairo'))
print(cl)

outLst_county = list()
#for (i in 1:numCnty)  {
outLst_county <- foreach(i = 1:numCnty, 
                            .packages = c('ggplot2', 'data.table','formattable',
                                          'ggpubr','RColorBrewer','ggsci','wesanderson','tseries','penalized')) %dopar% {    
  cntyname = sortedCounties[i]
  cvd_county_LatentInfections(curDate, cntyname, TXcountyDat, TXcountyDat2, 
                                     popTXcounty, ifr0, ifrL, ifrU)
  #outLst_county[[i]] = tmp
}
stopCluster(cl)

t2=Sys.time(); print(t2-t1)


## Summaries Over regions

##
## 0. set up  ####

# curDate = '2020-08-10'
print(curDate)

library(ggplot2)
library(ggrepel)
library(matlab)
#library(RColorBrewer)

source(file.path(getwd(), 'summariesOverRegions_subftns.R'))

##
##  1. country_summary ####
##

rgnNames = countryNamePop$Region[1:50]
popVec = countryNamePop$pop2019[1:50]

outLst_country = list(); 
for (i in 1:length(rgnNames)){
  stname = rgnNames[i]
  opath = file.path(getwd(), 'output', 'countries', stname, paste0(curDate))
  oDF = read.csv(file=file.path(opath, paste0(stname, '_outputDF.csv')))
  outLst_country[[i]] = oDF
}

myLst = outLst_country[1:50]
 
outsumPath = file.path(getwd(), 'output', 'country_summary', paste0(curDate))
if (!dir.exists(outsumPath)) dir.create(outsumPath, recursive = T)
output_current = file.path(getwd(), 'output', 'countries_current' )

## plot summaries
cvd_plotSummaries()



##
##  3. TX_county_summary   ####
##

rgnNames = sortedCounties 

# pop preprocessing  
popVec = as.numeric()
myStName = popTXcounty$CTYNAME
for (i in 1:length(sortedCounties)){
  cntyname = sortedCounties[i]
  stInd = which(myStName == paste0(cntyname, ' County'))
  Xpop = popTXcounty$Pop[stInd]
  #
  popVec[i] = Xpop
}
print(rgnNames)
print(popVec)

outLst_file = list()
for (i in 1:length(rgnNames)){
  stname = rgnNames[i]
  opath = file.path(getwd(), 'output', 'TX_counties', stname, paste0(curDate))
  oDF = read.csv(file=file.path(opath, paste0(stname, '_outputDF.csv')))
  outLst_file[[i]] = oDF
}
myLst = outLst_file 

outsumPath = file.path(getwd(), 'output', 'TX_county_summary', paste0(curDate))
if (!dir.exists(outsumPath)) dir.create(outsumPath, recursive = T)
output_current = file.path(getwd(), 'output', 'TX_counties_current' )

## plot summaries

cvd_plotSummaries(cumAsctRate_nudge_x = 10, cumAsctRate_nudge_y = 1)


##  
##  4. Fetch vaccination data   ###########################################
##
library(matlab)
outsumPath = file.path(getwd(), 'output', 'country_summary', paste0(curDate))
if (!dir.exists(outsumPath)) dir.create(outsumPath, recursive = T)
output_current = file.path(getwd(), 'output', 'countries_current' )

## Countries
# fetch from OWID
url1 = 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv'
dat1 = read.csv(url1, head=T)
datCountry = dat1[, c('location', 'date', 'people_vaccinated_per_hundred','people_fully_vaccinated_per_hundred',
                      'people_vaccinated', 'people_fully_vaccinated','total_vaccinations_per_hundred')]

## sort vaccination data

curDateVac = as.Date(curDate) - 2
ind = which(datCountry$date ==  as.character(curDateVac))
datCountryLast = datCountry[ind, ]

# vaccination data curation
# name curation -.-;; 
location2 = as.character(datCountryLast$location)
location2[(datCountryLast$location == 'United States')] = 'US'
location2[(datCountryLast$location == 'South Korea')] = 'Korea, South'
datCountryLast2 = datCountryLast
datCountryLast2$location = location2

## load regns2.csv 
outsumPathVac = file.path(getwd(), 'output', 'country_summary', paste0(curDateVac))
fname = file.path(outsumPathVac, 'regns2.csv')
regns2 = read.csv(fname)

## match vac countries and regns2 names

ind = match(regns2[, 1], location2)
sum(!is.na(ind))
regns2Vac = data.frame(region = regns2$X, cumAsctRate = regns2$cumAsctRate, 
                       pctEstTotalCases = regns2$pctEstTotalCases, pctVaccinated = NA, pctFullyVaccinated = NA)
for (i in 1:nrow(regns2Vac)){
  k = ind[i]
  if (!is.na(k)){
    regns2Vac$pctVaccinated[i] = datCountryLast2$people_vaccinated_per_hundred[k]
    regns2Vac$pctFullyVaccinated[i] = datCountryLast2$people_fully_vaccinated_per_hundred[k]
  } 
}

## Vaccinated or Once-infected

regns2Vac$pctVaccinatedInfected = 
  regns2Vac$pctVaccinated + regns2Vac$pctEstTotalCases - regns2Vac$pctVaccinated/100 * regns2Vac$pctEstTotalCases
regns2Vac$pctFullyVaccinatedInfected = 
  regns2Vac$pctFullyVaccinated + regns2Vac$pctEstTotalCases - regns2Vac$pctFullyVaccinated/100 * regns2Vac$pctEstTotalCases

hit1 = round(100*(1-1/3))
hit2 = round(100*(1-1/4))
#hit3 = round(100*(1-1/5))

## summary plot of the vaccinated or once-infected

n = nrow(regns2Vac) 
regns2Vac$regionSorted = factor(regns2Vac$region, levels = regns2Vac$region)
indNA = which(is.na(regns2Vac$pctVaccinatedInfected))
regns2Vac0 =  regns2Vac[-indNA, ]
n1 = nrow(regns2Vac0)

ord1 = order(regns2Vac0$pctVaccinatedInfected, decreasing = T)
levelsorted = regns2Vac0$regionSorted[ord1] 
regns2Vac0$name2 = factor(regns2Vac0$regionSorted, levels = levelsorted)

midCap = floor(n * 3/5)
mycol = c(n:1)
myjet = jet.colors(n+ midCap + 1)
myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
myjet2 = myjet[n:1]
myjet2[indNA] = NA
myjet3 = myjet2[-indNA]

{
  s11 <- ggplot(data = regns2Vac0) + 
    geom_col(aes(x = name2, y = pctVaccinatedInfected, fill = regionSorted ), alpha=1) +
    geom_hline(yintercept = hit1, linetype = 'dashed', color = 'gray', size=2) +
    geom_hline(yintercept = hit2, linetype = 'solid', color = 'gray', size=2) +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 3)", 
             x = n1-9, y=hit1+3, col = 'red', fontface = 'italic') +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 4)", 
             x = n1-9, y=hit2+3, col = 'red', fontface = 'italic') +
    theme_bw() +
    scale_fill_manual("", breaks = as.character(c(1:n1)), values = myjet3) +
    scale_y_continuous(limits = c(0, 85), breaks = seq(10, 100, 10)) +
    
    labs(y='% of population', x='', 
         title='% of People Vaccinated at Least 1 Dose or Once Infected',
         subtitle = paste0('As of ', curDateVac) ) + 
    theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
          plot.subtitle = element_text(size=rel(1.3)),
          #plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
          #axis.text.y = element_text(color = mpal[2]),  
          axis.title = element_text(size=rel(1.5)), 
          #axis.title.y = element_text(color=mpal[2]),  
          legend.position = 'none', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1))) 
  
  print(s11)
  s11name = paste0('pctVaccinatedInfected.png')
  png(file.path(outsumPath, s11name), width=8, height=4, units = "in", res=300)
  Sys.sleep(1)
  print(s11)
  Sys.sleep(1)
  dev.off()
  Sys.sleep(1)
}


## summary plot of the vaccinated

n = nrow(regns2Vac) 
regns2Vac$regionSorted = factor(regns2Vac$region, levels = regns2Vac$region)
indNA = which(is.na(regns2Vac$pctVaccinated))
regns2Vac0 =  regns2Vac[-indNA, ]
n1 = nrow(regns2Vac0)

ord1 = order(regns2Vac0$pctVaccinated, decreasing = T)
levelsorted = regns2Vac0$regionSorted[ord1] 
regns2Vac0$name2 = factor(regns2Vac0$regionSorted, levels = levelsorted)

midCap = floor(n * 3/5)
mycol = c(n:1)
myjet = jet.colors(n+ midCap + 1)
myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
myjet2 = myjet[n:1]
myjet2[indNA] = NA
myjet3 = myjet2[-indNA]

{
  s13 <- ggplot(data = regns2Vac0) + 
    geom_col(aes(x = name2, y = pctVaccinated, fill = regionSorted ), alpha=1) +
    geom_hline(yintercept = hit1, linetype = 'dashed', color = 'gray', size=2) +
    geom_hline(yintercept = hit2, linetype = 'solid', color = 'gray', size=2) +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 3)", 
             x = n1-9, y=hit1+3, col = 'red', fontface = 'italic') +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 4)", 
             x = n1-9, y=hit2+3, col = 'red', fontface = 'italic') +
    theme_bw() +
    scale_fill_manual("", breaks = as.character(c(1:n1)), values = myjet3) +
    scale_y_continuous(limits = c(0, 85), breaks = seq(10, 100, 10)) +
    
    labs(y='% of population', x='', 
         title='% of People Vaccinated at Least 1 Dose',
         subtitle = paste0('As of ', curDateVac) ) + 
    theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
          plot.subtitle = element_text(size=rel(1.3)),
          #plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
          #axis.text.y = element_text(color = mpal[2]),  
          axis.title = element_text(size=rel(1.5)), 
          #axis.title.y = element_text(color=mpal[2]),  
          legend.position = 'none', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1))) 
  
  print(s13)
  s13name = paste0('pctVaccinated.png')
  png(file.path(outsumPath, s13name), width=8, height=4, units = "in", res=300)
  Sys.sleep(1)
  print(s13)
  Sys.sleep(1)
  dev.off()
  Sys.sleep(1)
}


## summary plot of the fully vaccinated or once-infected

n = nrow(regns2Vac) 
regns2Vac$regionSorted = factor(regns2Vac$region, levels = regns2Vac$region)
indNA = which(is.na(regns2Vac$pctFullyVaccinatedInfected))
regns2Vac0 =  regns2Vac[-indNA, ]
n1 = nrow(regns2Vac0)

ord1 = order(regns2Vac0$pctFullyVaccinatedInfected, decreasing = T)
levelsorted = regns2Vac0$regionSorted[ord1] 
regns2Vac0$name2 = factor(regns2Vac0$regionSorted, levels = levelsorted)

midCap = floor(n * 3/5)
mycol = c(n:1)
myjet = jet.colors(n+ midCap + 1)
myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
myjet2 = myjet[n:1]
myjet2[indNA] = NA
myjet3 = myjet2[-indNA]

{
  s15 <- ggplot(data = regns2Vac0) + 
    geom_col(aes(x = name2, y = pctFullyVaccinatedInfected, fill = regionSorted ), alpha=1) +
    geom_hline(yintercept = hit1, linetype = 'dashed', color = 'gray', size=2) +
    geom_hline(yintercept = hit2, linetype = 'solid', color = 'gray', size=2) +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 3)", 
             x = n1-9, y=hit1+3, col = 'red', fontface = 'italic') +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 4)", 
             x = n1-9, y=hit2+3, col = 'red', fontface = 'italic') +
    theme_bw() +
    scale_fill_manual("", breaks = as.character(c(1:n1)), values = myjet3) +
    scale_y_continuous(limits = c(0, 85), breaks = seq(10, 100, 10)) +
    
    labs(y='% of population', x='', 
         title='% of People Fully Vaccinated or Once Infected',
         subtitle = paste0('As of ', curDateVac) ) + 
    theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
          plot.subtitle = element_text(size=rel(1.3)),
          #plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
          #axis.text.y = element_text(color = mpal[2]),  
          axis.title = element_text(size=rel(1.5)), 
          #axis.title.y = element_text(color=mpal[2]),  
          legend.position = 'none', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1))) 
  
  print(s15)
  s15name = paste0('pctFullyVaccinatedInfected.png')
  png(file.path(outsumPath, s15name), width=8, height=4, units = "in", res=300)
  Sys.sleep(1)
  print(s15)
  Sys.sleep(1)
  dev.off()
  Sys.sleep(1)
}


## summary plot of the fully vaccinated

n = nrow(regns2Vac) 
regns2Vac$regionSorted = factor(regns2Vac$region, levels = regns2Vac$region)
indNA = which(is.na(regns2Vac$pctFullyVaccinated))
regns2Vac0 =  regns2Vac[-indNA, ]
n1 = nrow(regns2Vac0)

ord1 = order(regns2Vac0$pctFullyVaccinated, decreasing = T)
levelsorted = regns2Vac0$regionSorted[ord1] 
regns2Vac0$name2 = factor(regns2Vac0$regionSorted, levels = levelsorted)

midCap = floor(n * 3/5)
mycol = c(n:1)
myjet = jet.colors(n+ midCap + 1)
myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
myjet2 = myjet[n:1]
myjet2[indNA] = NA
myjet3 = myjet2[-indNA]

{
  s14 <- ggplot(data = regns2Vac0) + 
    geom_col(aes(x = name2, y = pctFullyVaccinated, fill = regionSorted ), alpha=1) +
    geom_hline(yintercept = hit1, linetype = 'dashed', color = 'gray', size=2) +
    geom_hline(yintercept = hit2, linetype = 'solid', color = 'gray', size=2) +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 3)", 
             x = n1-9, y=hit1+3, col = 'red', fontface = 'italic') +
    annotate('text', label = "A Herd Immunity Threshold (if R0 = 4)", 
             x = n1-9, y=hit2+3, col = 'red', fontface = 'italic') +
    theme_bw() +
    #scale_fill_manual("", breaks = as.character(c(1:n)), values = myjet[n:1]) +
    scale_fill_manual("", breaks = as.character(c(1:n1)), values = myjet3) +
    
    scale_y_continuous(limits = c(0, 85), breaks = seq(10, 100, 10)) +
    #scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
    labs(y='% of population', x='', 
         title='% of People Fully Vaccinated',
         subtitle = paste0('As of ', curDateVac) ) + 
    theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
          plot.subtitle = element_text(size=rel(1.3)),
          #plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)),
          axis.text = element_text(size = rel(1.2)),
          axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
          #axis.text.y = element_text(color = mpal[2]),  
          axis.title = element_text(size=rel(1.5)), 
          #axis.title.y = element_text(color=mpal[2]),  
          legend.position = 'none', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1)))
  
  print(s14)
  s14name = paste0('pctFullyVaccinated.png')
  png(file.path(outsumPath, s14name), width=8, height=4, units = "in", res=300)
  Sys.sleep(1)
  print(s14)
  Sys.sleep(1)
  dev.off()
  Sys.sleep(1)
}

## output_current

file.copy(from= file.path(outsumPath, 'pctVaccinatedInfected.png'), to=output_current, overwrite = T, recursive = T)
file.copy(from= file.path(outsumPath, 'pctVaccinated.png'), to=output_current, overwrite = T, recursive = T)
file.copy(from= file.path(outsumPath, 'pctFullyVaccinatedInfected.png'), to=output_current, overwrite = T, recursive = T)
file.copy(from= file.path(outsumPath, 'pctFullyVaccinated.png'), to=output_current, overwrite = T, recursive = T)

##
##  End: Vaccinatation toward herd immunity
##



## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX
## 

# assume input datasets are already loaded to workspace
print(getwd())
print(curDate)

##
##  country report  ####
##

head(countryNamePop)

# curation
cname = as.character(countryNamePop$Region)

# name curation for white spaces
whspInd = rep(0, nrow(countryNamePop))
cname2 = rep(NA, nrow(countryNamePop))
for (i in 1:(numCntr+1)){
  x = cname[i]
  y = chartr(' ', '\u00a0', x)
  whspInd[i] = (x != y)
  cname2[i] = y
} 
# change filename
outPath3 = file.path(getwd(), 'output', 'countries_current')
for (i in which(whspInd==1)){
  #print(i)
  listoffiles = list.files(path = outPath3, pattern = as.character(cname[i]))
  listoffiles2 = chartr(cname[i], cname2[i], listoffiles)
  if (length(listoffiles) != 0) {
    file.copy(file.path(outPath3, listoffiles), 
              file.path(outPath3, listoffiles2), overwrite = T)
  }
}


## 1
repname1 = 'REPORT_COUNTRY.md'

sink(file.path(getwd(), repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:numCntr){
  cat(paste0(i, '. ', cname2[i], ' <p>\n'))
    fnametmp1 = paste0(cname2[i], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(cname2[i], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
    fnametmp3 = paste0(cname2[i], '_estInfections.png')
    getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(cname2[i], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
# one more country
  cat(paste0(cname2[51], ' <p>\n'))
    fnametmp1 = paste0(cname2[51], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(cname2[51], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
    fnametmp3 = paste0(cname2[51], '_estInfections.png')
    getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(cname2[51], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')

sink()



## 2
repname1 = 'REPORT_COUNTRY_RECENT8w.md'

sink(file.path(getwd(), repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:numCntr){
  cat(paste0(i, '. ', cname2[i], ' <p>\n'))
  fnametmp1 = paste0(cname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(cname2[i], '_Recent_NewCasesEstConfirmed.png')
  getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(cname2[i], '_Recent_estInfections.png')
  getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(cname2[i], '_Recent_estInfectionsNewCases.png')
  getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
# one more country
cat(paste0(cname2[51], ' <p>\n'))
fnametmp1 = paste0(cname2[51], '_newCases7d.png')
getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
fnametmp2 = paste0(cname2[51], '_Recent_NewCasesEstConfirmed.png')
getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
cat(paste0('> ', getImg1, getImg2))
cat('\n\n')

fnametmp3 = paste0(cname2[51], '_Recent_estInfections.png')
getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
fnametmp4 = paste0(cname2[51], '_Recent_estInfectionsNewCases.png')
getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
cat(paste0('> ', getImg3, getImg4))
cat('\n\n', '<p>&nbsp;</p>', '\n\n')

sink()


## 3
repname1 = 'Daily_confirmed_raw_country.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in seq(1, numCntr, 2)){
  cat(paste0('|  ', i, '. ', cname2[i], '  |  ', i+1, '. ', cname2[i+1], '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(cname2[i], '_newCases.png')
  fnametmp2 = paste0(cname2[i+1], '_newCases.png')
  cat(paste0(  '|  ![img](/output/countries_current/', fnametmp1, ')  ', 
               '|  ![img](/output/countries_current/', fnametmp2, ')  |  \n\n'  ))
}
# one more country
  cat(paste0( cname2[51], '  \n'))
  fnametmp1 = paste0(cname2[51], '_newCases.png') 
  cat(paste0( '<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ', 
              '   \n\n'  ))
   
sink()


## 4
repname1 = 'Daily_rates_country.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:numCntr){
  cat(paste0(i, '. ', cname2[i], ' <p>\n'))
  fnametmp1 = paste0(cname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(cname2[i], '_cnvd_AscertainmentRate.png')
  getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(cname2[i], '_estCumIncidence.png')
  getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(cname2[i], '_estTransmissionRate.png')
  getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
# one more country
cat(paste0(cname2[51], ' <p>\n'))
fnametmp1 = paste0(cname2[51], '_newCases7d.png')
getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
fnametmp2 = paste0(cname2[51], '_cnvd_AscertainmentRate.png')
getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
cat(paste0('> ', getImg1, getImg2))
cat('\n\n')

fnametmp3 = paste0(cname2[51], '_estCumIncidence.png')
getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
fnametmp4 = paste0(cname2[51], '_estTransmissionRate.png')
getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
cat(paste0('> ', getImg3, getImg4))
cat('\n\n', '<p>&nbsp;</p>', '\n\n')

sink()




##
##  county report   ####
##

head(sortedCounties)

# curation
Ctname = as.character(sortedCounties)

# name curation for white spaces
whspInd = rep(0, length(sortedCounties))
Ctname2 = rep(NA, length(sortedCounties))
for (i in 1:(numCnty)){
  x = Ctname[i]
  y = chartr(' ', '\u00a0', x)
  whspInd[i] = (x != y)
  Ctname2[i] = y
} 
# change filename
outPath3 = file.path(getwd(), 'output', 'TX_counties_current')
for (i in which(whspInd==1)){
  #print(i)
  listoffiles = list.files(path = outPath3, pattern = as.character(Ctname[i]))
  listoffiles2 = chartr(Ctname[i], Ctname2[i], listoffiles)
  if (length(listoffiles) != 0) {
    file.copy(file.path(outPath3, listoffiles), 
              file.path(outPath3, listoffiles2), overwrite = T)
  }
}


## 1
repname3 = 'REPORT_TX_COUNTY.md'

sink(file.path(getwd(), repname3))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15) \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(Ctname)){
  cat(paste0(i, '. ', Ctname2[i], ' <p>\n'))
    fnametmp1 = paste0(Ctname2[i], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(Ctname2[i], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/TX_counties_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
    fnametmp3 = paste0(Ctname2[i], '_estInfections.png')
    getImg3 = paste0('<img src="/output/TX_counties_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(Ctname2[i], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/TX_counties_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
sink()



## 2
repname3 = 'REPORT_TX_COUNTY_RECENT8w.md'

sink(file.path(getwd(), repname3))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15) \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(Ctname)){
  cat(paste0(i, '. ', Ctname2[i], ' <p>\n'))
  fnametmp1 = paste0(Ctname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(Ctname2[i], '_Recent_NewCasesEstConfirmed.png')
  getImg2 = paste0('<img src="/output/TX_counties_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(Ctname2[i], '_Recent_estInfections.png')
  getImg3 = paste0('<img src="/output/TX_counties_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(Ctname2[i], '_Recent_estInfectionsNewCases.png')
  getImg4 = paste0('<img src="/output/TX_counties_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
sink()



## 3
repname1 = 'Daily_confirmed_raw_TX_county.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15) \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in seq(1, length(Ctname2)-1, 2)){
  cat(paste0('|  ', i, '. ', Ctname2[i], '  |  ', i+1, '. ', Ctname2[i+1], '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(Ctname2[i], '_newCases.png')
  fnametmp2 = paste0(Ctname2[i+1], '_newCases.png')
  cat(paste0(  '|  ![img](/output/TX_counties_current/', fnametmp1, ')  ', 
               '|  ![img](/output/TX_counties_current/', fnametmp2, ')  |  \n\n'  ))
}
cat(paste0('|  ', 15, '. ', Ctname2[15], '  |  ', 16, '. NA', '  |  \n'))
cat(paste0('|  :---   |   :---   |  \n'))

fnametmp1 = paste0(Ctname2[15], '_newCases.png') 
cat(paste0(  '|  <img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ', 
             '|   NA  |  \n\n'  ))

sink()


## 4
repname1 = 'Daily_rates_TX_county.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15)  \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(Ctname2)){
  cat(paste0(i, '. ', Ctname2[i], ' <p>\n'))
  fnametmp1 = paste0(Ctname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(Ctname2[i], '_cnvd_AscertainmentRate.png')
  getImg2 = paste0('<img src="/output/TX_counties_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(Ctname2[i], '_estCumIncidence.png')
  getImg3 = paste0('<img src="/output/TX_counties_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(Ctname2[i], '_estTransmissionRate.png')
  getImg4 = paste0('<img src="/output/TX_counties_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}

sink()



##  EOF





## EOF    #######
