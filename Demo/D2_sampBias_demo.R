
# install_github(repo = "azizka/sampbias")
library(sampbias)

#load example data
occ <- read.csv("input/borneo_mammals.csv", sep = "\t")

#run sampbias
bias.out <- SamplingBias(x = occ)

#sumamrize results
summary(bias.out)

#Visualize
par (ask = T)
plot(bias.out)

#Show graphical User interface https://azizka.shinyapps.io/sampbias/