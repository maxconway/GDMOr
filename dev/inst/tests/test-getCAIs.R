context('getCAIs')
skip("imports not possible")

# Set up defaults
names <- c('one','two','three')
DNA <- DNAString(paste(rep('A',27),collapse=''))
ranges <- IRanges(start=c(1,10,19),
									width=9,
									names=names
)
strand = Rle('+',length(names))
granges <- GRanges(seqnames = c('test1'),
									 ranges = ranges,
									 strand = strand
)
granges@elementMetadata$locus <- names

test_that('runs', {
	sample <- getCAIs(DNA,granges)
	hypothesis <- c(one=1,two=1,three=1)
	expect_that(sample, equals(hypothesis))
})

test_that('realistic case one', {
	DNA <- DNAString('GATGATGATGATGATGATGACGACGAC')
	
	sample <- getCAIs(DNA,granges)
	hypothesis <- c(one=1,two=1,three=1/2)
	expect_that(sample, equals(hypothesis))
})

test_that('strands work', {
	strand(granges) <- c('+','-','+')
	DNAString <- DNAString('AAAAAAAAATTTTTTTTTAAAAAAAAA')
	
	sample <- getCAIs(DNA,granges)
	hypothesis <- c(one=1,two=1,three=1)
	expect_that(sample, equals(hypothesis))
})