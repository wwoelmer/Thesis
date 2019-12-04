# create animation of chl time series from continuum data

library(magick)
library(pdftools)


newlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo.png"), "x150")
oldlogo <- image_scale(image_read("https://developer.r-project.org/Logo/Rlogo-3.png"), "x150")
frames <- image_morph(c(oldlogo, newlogo), frames = 10)
image_animate(frames)

apr <- image_scale(image_read_pdf('./Materials for R mapping/29Apr19_RCC_chla.pdf'))
may <- image_scale(image_read_pdf('./Materials for R mapping/30May19_RCC_chla.pdf'))
frame <- image_morph(c(apr, may))
image_animate(frame)
