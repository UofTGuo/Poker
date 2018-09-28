library("holdem")

#winning
loganandtim = function (numattable1, crds1, currentbet, mychips1,blinds1){
  a1 = 0
  M = (mychips1 / blinds1)
  if (M < 3){a1 = mychips1}
  if (M >= 3 && M < 6.5) {
    if(crds1[1,1] == crds1[2,1]){a1 = mychips1}
    if(crds1[1,1] - crds1[2,1] == 1 && crds1[1,2] == crds1[2,2]){a1 = mychips1}
    if(crds1[2,1] - crds1[1,1] == 1 && crds1[1,2] == crds1[2,2]){a1 = mychips1}
  }
  if (M >= 6.5) {
    if(crds1[1,1] == crds1[2,1]){a1 = mychips1}
  }
  return(a1)
}

# Example
result = c()
for(i in 1:100){
  numattable1 = 2
  data = deal1(numattable1)
  crds1 = data$plnum1
  currentbet = 10
  mychips1 = 100
  blinds1 = 2
  result[i] = loganandtim(numattable1, crds1, currentbet, mychips1, blinds1)
}
