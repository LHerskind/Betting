# load data into data.frame

########################################################
home_team <- "Nancy"
away_team <- "Sochaux"
########################################################

#Engelsk
#df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/E0.csv')

#Spansk
#df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/SP1.csv')

#Italiensk
#df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/I1.csv')

#Tysk
#df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/D1.csv')

#Fransk
#df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/F1.csv')
df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/F2.csv')

#Holland
#df <- read.csv('http://www.football-data.co.uk/mmz4281/1516/N1.csv')

# munge data into format compatible with glm function
df <- apply(df, 1, function(row){ data.frame(
#Splitter dataen op så vi får to rækker, således at vi har et udfald for ude og hjemme      
    team=c(row['HomeTeam'], row['AwayTeam']), 
    opponent=c(row['AwayTeam'], row['HomeTeam']),
    #FTHF = full time home goal 
    FTgoals=c(row['FTHG'], row['FTAG']),
    home=c(1, 0))
})
df <- do.call(rbind, df)
df$FTgoals <- as.numeric(as.character(df$FTgoals))

# fit the model
FTmodel <- lm(FTgoals ~ home + team + opponent, data=df)

profitmargin <- 0.05
margin <- 1+profitmargin
    
av_home_goals_FT <- predict(FTmodel, data.frame(home=1, team=home_team , opponent=away_team), type="response")
av_away_goals_FT <- predict(FTmodel, data.frame(home=0, team=away_team, opponent=home_team), type="response")

# Sandsynligheder for de enkelte udfald, poisson fordeling
home_goals_FT <- dpois(0:10, av_home_goals_FT)
away_goals_FT <- dpois(0:10, av_away_goals_FT)

# convert probability vectors into score matrix
matrixFT <- home_goals_FT %o% away_goals_FT

#Handicap full time
away_FTA <- c(sum(diag(matrixFT[,2:11]))) 
home_FTA <- c(sum(diag(matrixFT[2:11,])))
for(i in 3:11){
    away_FTA <- c(away_FTA, sum(diag(matrixFT[,i:11]))  )
    home_FTA <- c(home_FTA, sum(diag(matrixFT[i:11,])))
}
home_FT <- sum(home_FTA)
away_FT <- sum(away_FTA)
draw_FT <- 1-home_FT - away_FT

HC_FT <- c("","0","-","1", "Fair odds:", signif(margin/(home_FT-sum(home_FTA[1]) ),3), signif(margin/home_FTA[1],3), signif(margin/(away_FT+sum(home_FTA[1])),3))
HC_FT <- c(HC_FT, "","1","-","0", "Fair odds:", signif(margin/(home_FT+sum(away_FTA[1]) ),3), signif(margin/away_FTA[1],3), signif(margin/(away_FT-sum(away_FTA[1])),3))

pm <- t (matrix(c(
    "1X2 FT:",home_team,"Uafgjort",away_team,
    "Fair odds:", signif(margin/home_FT,3), signif(margin/draw_FT,3), signif(margin/away_FT,3),
    "" , "", "", "",
    HC_FT
    ),nrow=4,ncol=7))
df2 <- data.frame(pm)
print.data.frame(df2)
