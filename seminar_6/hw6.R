library(plyr)
library(data.table)

dt = count(df, "team")
teams = dt[dt$freq >= 200, ]$team
df = baseball[baseball$team %in% teams, ]

res = matrix(0, 32, 32, dimnames = list(teams, teams))
team_size = matrix(0, 32, 32, dimnames = list(teams, teams))

for(i in 1:32) {
 for(k in 1:32) {
   res[i,k] = intersect(teams[i], teams[k], df)
   team_size[i,k] = team_size_sum(teams[i], teams[k], df)
 } 
}

res = 1 - res

mds = cmdscale(res, k=2)
plot(mds, pch=19, cex=0.001*team_size)
text(mds, rownames(mds), adj=c(1.2,1.2), col = 'red')

team_size_sum = function(t1, t2, df) {
  return(length(unique(df[df$team == as.character(t1), "id"])) + 
    length(unique(df[df$team == as.character(t1), "id"])))
}

intersect = function(t1, t2, df) {
  s1 = unique(df[df$team == as.character(t1), "id"])
  s2 = unique(df[df$team == as.character(t2), "id"])
  # Find players that play for both teams
  num_common_players = length(s1[which(s1 %in% s2)]) 
  
  # Divide by number of players in smaller team  
  if(length(s1) < length(s2)) {
    return(num_common_players / length(s1))
  } else {
    return(num_common_players / length(s2))
  }
}
