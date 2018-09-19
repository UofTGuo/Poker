#losing
peanutgallery = function (numattable1, crds1, board1, round1, 
                          currentbet, mychips1, pot1, roundbets, blinds1, chips1, ind1, 
                          dealer1, tablesleft){
  a1 = 0
  if (mychips1 < 3 * blinds1)  a1 = mychips1
  if ((crds1[1, 1] == 14) && (crds1[2, 1] == 14)) a1 = mychips1
  if (crds1[1, 2] == crds1[2, 2] && runif(1) > 0.6) a1 = mychips1
  if ((crds1[1, 1] == 10) && (crds1[2, 1] == 9)) {
    u = runif(1)
    if (u < 0.56)  a1 = mychips1
    if (u >= 0.56)  a1 = 0
  }
  if (currentbet == blinds1) a1 = mychips1
  if ((crds1[1, 1] == crds1[2, 1])&&(crds1[1, 1] >11.5)&&(numattable1 < 10)) a1 = mychips1
  a1
}  ## end of peanutgallery