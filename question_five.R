# Loads ggplot2.

library(ggplot2)

# Reads in and summarises data.

dsData = read.csv("question-5-data/Cui_etal2014.csv")
summary(dsData)

# Creates variables for relevant columns.

virionVol = dsData$Virion.volume..nm.nm.nm.
genomeLen = dsData$Genome.length..kb.

# Plots virion volume against genome length.

ggplot(data = dsData, aes(x = genomeLen, y = virionVol)) +
  geom_point() +
  labs(x = "Genome length (kb)", y = "Virion volume (nm³)")

# Log transformation is necessary here.

logVirionVol = log(virionVol)
logGenomeLen = log(genomeLen)

# Plots graph with log transformed variables.

ggplot(data = dsData, aes(x = logGenomeLen, y = logVirionVol)) +
  geom_point() +
  labs(x = "Log genome length (kb)", y = "Log virion volume (nm³)")

# Creates linear model

model = lm(logVirionVol ~ logGenomeLen)
summary(model)

# Replicates plot given in question sheet.

ggplot(data = dsData, aes(x = logGenomeLen, y = logVirionVol)) +
  geom_point() +
  labs(x = "log[genome length (kb)]", y = "log[virion volume (nm³)]") +
  geom_smooth(method = lm)

# Estimates volume of virus with given genome length.

vol = 1181.8 * (300 ^ 1.5152)
vol