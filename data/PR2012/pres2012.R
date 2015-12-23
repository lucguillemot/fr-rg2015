library(dplyr)
pres <- read.csv('pres2012.csv')
pres$DEPT <- as.character(pres$DEPT)
pres$COMMUNE <- as.character(pres$COMMUNE)
for (i in 1:length(pres$DEPT)) {
      if (nchar(pres$DEPT[i])<2) {
            pres$DEPT[i] <- paste("0", pres$DEPT[i], sep="")
      }
      if (nchar(pres$COMMUNE[i])<2) {
            pres$COMMUNE[i] <- paste("00", pres$COMMUNE[i], sep="")
      }
      if (nchar(pres$COMMUNE[i])<3) {
            pres$COMMUNE[i] <- paste("0", pres$COMMUNE[i], sep="")
      }
}

pres <- mutate(pres, INSEE=paste(pres$DEPT, pres$COMMUNE, sep=""))
pres$DEPT <- NULL
pres$COMMUNE<-NULL

#pres$PCwinner <- apply(pres, 1, max)
pres <- pres %>% mutate(PCwinner=ifelse(lepen>sarkozy&lepen>hollande, lepen,
                                        ifelse(sarkozy>hollande,sarkozy, hollande))) %>% mutate(winner=ifelse(lepen>sarkozy&lepen>hollande, "lepen",
                                      ifelse(sarkozy>hollande,"sarkozy", "hollande")))

write.csv(pres, file="DATAPRES2012.csv")

pop = read.csv('pop.csv')

# GEt the decile values
quantile(pop$POPULATION, probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
quantile(pres$PCwinner, probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
quantile(pres$lepen, probs=c(0.2,0.4,0.6,0.8,1))

# Get the JENKS thresholds
library(classInt)
classIntervals(pop$POPULATION, n=10, style="jenks")
