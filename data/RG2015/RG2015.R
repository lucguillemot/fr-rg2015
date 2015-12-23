library(dplyr)
library(ggplot2)
library(classInt)

####################################################
## FIRST ROUND #####################################
####################################################

# Get Regions codes
regCodes <- read.csv("CodesRegions2016.csv")
names(regCodes) <- c("INSEE", "REG2016") #rename the variables.

# Read electoral data for the first round
reg <- read.csv('Reg_15_Resultats_Communes_T1.csv')

# Add zeros to Departement and Communes INSEE codes.
## change type to character to use the fonction nchar()
reg$Code.du.département <- as.character(reg$Code.du.département)
reg$Code.de.la.commune <- as.character(reg$Code.de.la.commune)

for (i in 1:length(reg$Code.du.département)) {
      if (nchar(reg$Code.du.département[i])<2) {
            reg$Code.du.département[i] <- paste("0", reg$Code.du.département[i], sep="")
      }
      if (nchar(reg$Code.de.la.commune[i])<2) {
            reg$Code.de.la.commune[i] <- paste("00", reg$Code.de.la.commune[i], sep="")
      }
      if (nchar(reg$Code.de.la.commune[i])<3) {
            reg$Code.de.la.commune[i] <- paste("0", reg$Code.de.la.commune[i], sep="")
      }
}

## create a column with the unique INSEE code
reg <- mutate(reg, INSEE=paste(reg$Code.du.département, reg$Code.de.la.commune, sep=""))
reg$Code.de.la.commune<-NULL
# select first columns with not-lists-related data + Abstention rate
regdata <- select(reg,INSEE,DPT=Code.du.département,NAME=Libellé.de.la.commune,ABST1=X..Abs.Ins)
# Add population data
pop <- read.csv('../POP2012/POP2012.csv')
regdata <- merge(regdata,pop, by="INSEE")
# select list-related columns
regT1 <- select(reg, INSEE, contains("Nuance.Liste"), contains("X..Voix.Exp"), contains("X..Voix.Ins"))

#Find the max value
regT1$max <- apply(regT1[15:27], 1, which.max) # where is the max value
regT1 <- regT1 %>% mutate(colonne=max+1) # Column index of the label of the max value
for (j in 1:length(regT1$INSEE)) {
      # get the label of max value (the list with the maximum of votes)
      regT1$winnerT1[j] <- as.character(regT1[j,regT1$colonne[j]])
}

# find the values for 'LFN' (Liste Front National)
regT1$Ifn <- apply(regT1[2:14], 1, function(x) which(x=="LFN"))
regT1 <- regT1 %>% mutate(Indexfn=as.numeric(Ifn)+14, IndexfnInscrits=as.numeric(Ifn)+27)
for (h in 1:length(regT1$INSEE)) {
      regT1$PCFNT1[h] <- as.numeric(regT1[h,regT1$Indexfn[h]])
      regT1$PCFNT1INSCRITS[h] <- as.numeric(regT1[h,regT1$IndexfnInscrits[h]])
}

# join the two tables with relevant variables
## select the relevant variable
regWinnerFnT1 <- select(regT1, INSEE, winnerT1, PCFNT1, PCFNT1INSCRITS)
reg <- merge(regdata,regWinnerFnT1, by="INSEE")
reg <- merge (reg, regCodes, by="INSEE")

# display the number of occurrences of each list
table(reg$winnerT1) 

# get the decile values
quantile(regdata$ABST1, probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
quantile(reg$PCFNT1, probs=c(0.2,0.4,0.6,0.8,1))

# Groupe les listes par affinités politiques. (le FN est tout seul)
RG2015X <- reg %>% mutate(winT1=ifelse(winnerT1=="LEXG"|winnerT1=="LCOM"|winnerT1=="LFG","EXG",winnerT1))
RG2015X <- RG2015X %>% mutate(winT1=ifelse(winnerT1=="LSOC"|winnerT1=="LUG"|winnerT1=="LRDG"|winnerT1=="LDVG","GAUCHE",winT1))
RG2015X <- RG2015X %>% mutate(winT1=ifelse(winnerT1=="LVEG"|winnerT1=="LVEC"|winnerT1=="LECO","ECOLO",winT1))
RG2015T1 <- RG2015X %>% mutate(winT1=ifelse(winnerT1=="LLR"|winnerT1=="LUD"|winnerT1=="LDLF"|winnerT1=="LDVD","DROITE",winT1))

# House keeping (delete and reorder columns)
RG2015T1$winnerT1 <- NULL
RG2015T1 <- RG2015T1[,c(1,3,2,8,5,9,4,6,7)]

# Export data for the first round
write.csv(RG2015T1, file="DATARG2015T1.csv")


table(RG2015T1$winT1)
####################################################
## SECOND ROUND ####################################
####################################################

reg <- read.csv('Reg_15_Resultats_Communes_T2.csv')

# Add zeros to Departement and Communes INSEE codes.
## change type to character to use the fonction nchar()
reg$Code.du.département <- as.character(reg$Code.du.département)
reg$Code.de.la.commune <- as.character(reg$Code.de.la.commune)

for (i in 1:length(reg$Code.du.département)) {
      if (nchar(reg$Code.du.département[i])<2) {
            reg$Code.du.département[i] <- paste("0", reg$Code.du.département[i], sep="")
      }
      if (nchar(reg$Code.de.la.commune[i])<2) {
            reg$Code.de.la.commune[i] <- paste("00", reg$Code.de.la.commune[i], sep="")
      }
      if (nchar(reg$Code.de.la.commune[i])<3) {
            reg$Code.de.la.commune[i] <- paste("0", reg$Code.de.la.commune[i], sep="")
      }
}
## create a column with the unique INSEE code
reg <- mutate(reg, INSEE=paste(reg$Code.du.département, reg$Code.de.la.commune, sep=""))
reg$Code.de.la.commune<-NULL

# select first column with abstention rate
regT2ABST <- select(reg,INSEE,ABST2=X..Abs.Ins)
# select list-related columns
regT2 <- select(reg, INSEE, contains("Nuance.Liste"), contains("X..Voix.Exp"))

#Find the max value
regT2$max <- apply(regT2[6:9], 1, which.max) # where is the max value
regT2 <- regT2 %>% mutate(colonne=max+1) # Column index of the label of the max value
for (j in 1:length(regT2$INSEE)) {
      regT2$winnerT2[j] <- as.character(regT2[j,regT2$colonne[j]])
}

# find the value for 'LFN' (Liste Front National)
regT2$Ifn <- apply(regT2[2:5], 1, function(x) which(x=="LFN"))
regT2 <- regT2 %>% mutate(Indexfn=as.numeric(Ifn)+5)
for (h in 1:length(regT2$INSEE)) {
      regT2$PCFNT2[h] <- as.numeric(regT2[h,regT2$Indexfn[h]])
}

# join the two tables with relevant variables
## select the relevant variable
regWinnerFnT2 <- select(regT2, INSEE, PCFNT2,winnerT2)
regT2 <- merge(regT2ABST,regWinnerFnT2, by="INSEE")

unique(regT2$winner)

# Groupe les listes par affinités politiques. (le FN est tout seul)
RG2015T2 <- regT2 %>% mutate(winnerT2=ifelse(winnerT2=="LDVG"|winnerT2=="LUG"|winnerT2=="LSOC","GAUCHE",winnerT2))

# display the number of occurrences of each list
table(RG2015T2$winner)

write.csv(RG2015T2, file="DATARG2015T2.csv")


####################################################
## DIFFERENCE BETWEEN ROUNDS #######################
####################################################

# Merge first and second round data
RG2015 <- merge(RG2015T1,RG2015T2, by="INSEE")

# Search data for Marseille
RG2015[RG2015$INSEE==13055,]

# Calculate difference in ABSTENTION and PCfn between the two rounds
RG2015<-RG2015 %>% mutate(DIFF_ABS=ABST2-ABST1, DIFF_PCFN=PCFNT2-PCFNT1)

# export data for both rounds.
write.csv(RG2015, "DATARG2015.csv")

table(RG2015$winnerT2)
####################################################
## POST PRODUCTION #################################
####################################################

# get the decile values
quantile(RG2015$DIFF_ABS, probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
quantile(RG2015$PCFNT1INSCRITS, probs=c(0.2,0.4,0.6,0.8,1))


#plot FN / Abstention
P5000 <- filter(RG2015, as.integer(POP) > 5000)
P5000Reg <- filter(RG2015, REG2016==32|REG2016==93)
P5000Or<-arrange(P5000, desc(ABS))
g<-ggplot(P5000Reg, aes(DIFF_ABS,DIFF_PCFN))
g+geom_point(aes(color=as.factor(REG2016)))

# Number of municipalities with less than 1000 inhabitants
sum(pop$POP<1000)

