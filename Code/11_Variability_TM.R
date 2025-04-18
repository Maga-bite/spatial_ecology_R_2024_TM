### Measurement of RS based variability ###

# - Setting things up - #
  library(imageRy)
  library(terra)
  library(viridis)

  im.list()

  sent <- im.import("sentinel.png")
    # Band 1 = NIR
    # Band 2 = red
    # Band 3 = green
  im.plotRGB(sent, 1, 2, 3)
 
# - Measure standard deviation with focal() function - #
  
  # We extract the mean value of NIR of a n x n window of pixels and get the standard deviation,
    # which is placed in the center pixel
  # The window is moved by one and the same process happens
  # Corners and boarders have less pixels than n x n
  # A map is then created pixel by pixel
  
  nir <- sent[[1]]
  sd3 <- focal(nir, matrix(1/9, 3, 3), fun=sd)
    # We choose a 3 (rows) x 3 (columns) pixels window
    # Could avoid the matrix() function, but if using another function than focal(), it might be useful
    # fun for the function that will use the values in our window to create 1 numerical value
    # sd is standard deviation
 plot(sd3)

  # Measure the standard deviation over 7x7 pixels window
  sd4 <- focal(nir, 7, fun=sd)
  plot(sd4)
  
  # Place both plots next to each other
  par(mfrow=c(1,2))
  plot(sd3)
  plot(sd4)
  
