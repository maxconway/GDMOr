# My take on CAI

library(stringr)
library(genomes)


# Using functions pulled from CAI_alaSharp, we pull two tables.
ftp <- readNCBIftp('Geobacter')
ftpURL <- urlParserftoTONCBI(ftp$ftp[7])
anngenes <- getAnnotation(ftpURL)
DNA <- getDNA(ftpURL)

CAIs <- getCAIs(DNA,anngenes)
