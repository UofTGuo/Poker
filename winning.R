library("holdem")

#winning
loganandtim = function (numattable1, crds1, board1, round1, currentbet, 
                        mychips1,
                        pot1, roundbets, blinds1, chips1, ind1, dealer1, tablesleft){
  a1 = 0
  M = (mychips1 / blinds1)
  if (M < 3) a1 = mychips1
  if (M >= 3 && M < 6.5) {
    if(crds1[1,1] == crds1[2,1]) a1 = mychips1
    if(crds1[1,1] - crds1[2,1] == 1 && crds1[1,2] == crds1[2,2]) a1 = mychips1
    if(crds1[2,1] - crds1[1,1] == 1 && crds1[1,2] == crds1[2,2]) a1 = mychips1
  }
  if (M >= 6.5) {
    if(crds1[1,1] == crds1[2,1]) a1 = mychips1
  }
  a1
}

# Example
numattable1 = 2
data = deal1(numattable1)