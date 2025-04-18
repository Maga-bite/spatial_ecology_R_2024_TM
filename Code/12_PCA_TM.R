### Principle component analysis ###

  # Doing a multivariate analysis to extract the principle component 1 (PC1) to explains most of the variability
  # Calculating the stadard deviation of the PC1

# - Setting things up - #
  library(terra)
  library(imageRy)
  
  im.list()
  sent <- im.import("sentinel.png")  
  pairs(sent)
    # We can see that sentinel 2 and 3 are strongly correlated with a 0.98 correlation
  
# - Perform a pca on sent with the im.pca() function and measure the variability - #
  sentpc <- im.pca(sent)
  pc1 <- sentpc$PC1
  plot(sentpc)
  
  pc1sd <- focal(pc1, w=3, fun=sd)
  plot(pc1sd)
