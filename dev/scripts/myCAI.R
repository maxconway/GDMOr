# My take on CAI

library(stringr)


# Using functions pulled from CAI_alaSharp, we pull two tables.
ftp <- readNCBIftp('Geobacter')
ftpURL <- urlParserftoTONCBI(ftp$ftp[7])
anngenes <- getAnnotation(ftpURL)
DNA <- getDNA(ftpURL)

#' Calculate the CAI for each gene
#' 
#' @param DNA a DNAString object, giving the whole genome
#' @param anngenes a GRanges object, detailing how genes match to the genome
#' @param referenceGenes the genes selected as being highly expressed enough to form a reference set
getCAIs <- function (DNA, anngenes, referenceGenes = anngenes@elementMetadata@listData$locus) {
	
	# make a list of amino acids, and their codons
	codontable <- list(Ala = c('GCT', 'GCC', 'GCA', 'GCG'),
										 Arg = c('CGT', 'CGC', 'CGA', 'CGG', 'AGA', 'AGG'),
										 Asn = c('AAT', 'AAC'),
										 Asp = c('GAT', 'GAC'),
										 Cys = c('TGT', 'TGC'),
										 Gln = c('CAA', 'CAG'),
										 Glu = c('GAA', 'GAG'),
										 Gly = c('GGT', 'GGC', 'GGA', 'GGG'),
										 His = c('CAT', 'CAC'),
										 Ile = c('ATT', 'ATC', 'ATA'),
										 Leu = c('TTA', 'TTG', 'CTT', 'CTC', 'CTA', 'CTG'),
										 Lys = c('AAA', 'AAG'),
										 Met = c('ATG'),
										 Phe = c('TTT', 'TTC'),
										 Pro = c('CCT', 'CCC', 'CCA', 'CCG'),
										 Ser = c('TCT', 'TCC', 'TCA', 'TCG', 'AGT', 'AGC'),
										 Thr = c('ACT', 'ACC', 'ACA', 'ACG'),
										 Trp = c('TGG'),
										 Tyr = c('TAT', 'TAC'),
										 Val = c('GTT', 'GTC', 'GTA', 'GTG'),
										 # 									 START = c('ATG'),
										 STOP = c('TAA', 'TGA', 'TAG')
	)
	# And prepare a vector of codons for making factors
	codons <- unlist(codontable, use.names=FALSE)
	
	
  # anngenes contains a dataframe of genes with their positions.
  # DNA is the DNA sequence.
  
  # using these, we cna find the sequence for each gene:
  genesView <- Views(DNA, anngenes@ranges)
  
  # correct the senses and convert to characters
  genes <- ifelse(as.character(anngenes@strand)=='+',
  								as.character(genesView),
  								as.character(reverseComplement(genesView))
  								)
	
	# add names
	names(genes) <- anngenes@elementMetadata@listData$locus
  
  # now factorize into codons
  genes <- lapply(genes, function(code){
  	factor(substring(code,seq.int(1,str_length(code),3),seq.int(3,str_length(code),3)),codons)
  })
  
  # Find out total number of usages
  globaltable <- table(unlist(genes[anngenes@elementMetadata@listData$locus %in% referenceGenes]))
  codonweights <- lapply(codontable, function(codons){
  	aminoacid <- globaltable[codons]
  	aminoacid/max(aminoacid)
  })
  # And clean up to a nice vector
  names(codonweights) <- NULL
  codonweights <- unlist(codonweights)
	
	# now it's time for the actual calculation
	CAIs <- sapply(genes, function(codons){
		exp(mean(log(codonweights[codons]),na.rm=TRUE))
	})
	
}

ping()


