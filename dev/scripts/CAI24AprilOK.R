library(GEOquery)
library(limma)
library(RCurl) 
library(genomes)
library(lmtest)

#################################################################
# Genetic Code ##
#################################################################
df.syn<-c("")
s <- c("tct","tcc","tca","tcg","agt","agc")
df.syn <- rbind(s)
f <- c("ttt","ttc","*","*","*","*")
df.syn <- rbind(df.syn,f)
l <- c("tta","ttg","ctt","ctc","cta","ctg")
df.syn <- rbind(df.syn,l)
y <- c("tat","tac","*","*","*","*")
df.syn <- rbind(df.syn,y)
#stopc <- c("taa","tag","tga","*","*","*")
#df.syn <- rbind(df.syn,stopc)
c <- c("tgt","tgc","*","*","*","*")
df.syn <- rbind(df.syn,c)
w <- c("tgg","*","*","*","*","*")
df.syn <- rbind(df.syn,w)
p <- c("cct","ccc","cca","ccg","*","*")
df.syn <- rbind(df.syn,p)
q <- c("caa","cag","*","*","*","*")
df.syn <- rbind(df.syn,q)
r <- c("cgt","cgc","cga","cgg","aga","agg")
df.syn <- rbind(df.syn,r)
i <- c("att","atc","ata","*","*","*")
df.syn <- rbind(df.syn,i)
m <- c("atg","*","*","*","*","*")
df.syn <- rbind(df.syn,m)
t <- c("act","acc","aca","acg","*","*")
df.syn <- rbind(df.syn,t)
n<- c("aat","aac","*","*","*","*")
df.syn <- rbind(df.syn,n)
k <- c("aaa","aag","*","*","*","*")
df.syn <- rbind(df.syn,k)
v <- c("gtt","gtc","gta","gtg","*","*")
df.syn <- rbind(df.syn,v)
a <- c("gct","gcc","gca","gcg","*","*")
df.syn <- rbind(df.syn,a)
d <- c("gat","gac","*","*","*","*")
df.syn <- rbind(df.syn,d)
e <- c("gaa","gag","*","*","*","*")
df.syn <- rbind(df.syn,e)
g <- c("ggt","ggc","gga","ggg","*","*")
df.syn <- rbind(df.syn,g)
h <- c("cat","cac","*","*","*","*")
df.syn <- rbind(df.syn,h)


aa <- list("s","f","l","y","c","w","p","q","r","i","m","t","n","k","v","a","d","e","g","h")
##################################################################
#fetching ftp information Bacteria
#################################################################
readNCBIftp<- function(str1)
{

  x1<-gsub('([[:punct:]])|\\s+','_',paste(str1))
  
  
  w <- data.frame(ftp= character(0))
  
  
  url = "ftp://ftp.ncbi.nih.gov/genomes/Bacteria/" 
  filenames = getURL(url, ftplistonly = TRUE, crlf = TRUE) 
  filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
  
  flag<-0
  
  ##Cerca il pid
  for(i in 1: length(filenames))
  {
    if(any(grep(x1,paste(filenames[i]))) )  
    {         
      
      w <- rbind(w,  data.frame(ftp = paste(filenames[i])))
      ##print(paste("Selected: ",filenames[i]))
      flag<-1
    }
    
    
    
  }
  
  return(w)
}



getTopGene <- function(str){

  gds1<-getGEO(str)
  eset<- GDS2eSet(gds1, do.log2=TRUE)
  
  #print(pData(eset))
  
  
  gpl <- getGEO(Meta(gds1)$platform)
  
  
  MA <- GDS2MA(gds1, GPL=gpl)
  #plotMA(MA$M)
  
  MA$M <- normalizeBetweenArrays(MA$M)
  #plotMA(MA$M)
  fit<-lmFit(MA$M)
  fit<-eBayes(fit)
  #View(MA$genes)
  #View(topTable(fit))
  
  # Sortby p
  tb<-topTable(fit, coef=NULL, number=length(fit), genelist=MA$genes, adjust.method="BH",
           sort.by="p", resort.by=NULL, p.value=1, lfc=0, confint=FALSE)

  return(tb)
  
}

###############################################
# Select a gene
###############################################
SelGene <- function(an, tb, vv, genelist, DNA)
{
    
  
    if(nchar(vv) > 0)
    {
      
      #sub<-substr(vv, 3, nchar(vv))
      
      for(i in 1: length(anngenes[,1]))
      {
        
        if(any(grep(vv, paste(values(anngenes)$gene[i]))))
        {
                
          a<-ranges(anngenes[i])
          min <- min(a)
          max <- max(a)
          loc <- paste(values(anngenes)$locus[i])
          desc <- paste(values(anngenes)$description[i])
          gene <- paste(values(anngenes)$gene[i])
          ss <- strand(anngenes[i])
          y <- paste(runValue(ss))
          cds<-substr(DNA, min, max)
          
          l<-findStartCodon(cds, y)
          cds<-l[[2]]
 
          
         # print(cds)
          
           break
        }#if
        
        
      }#for
      
      
      
    }#if
    
    

    for(j in 1: length(tb[,1]))
    {
      
      if(any(grep(paste(loc),paste(tb$ORF[j]))))
      {
        
        
        
       # print(paste(tb$ORF[j]))
       # print(loc)
        jj<- as.numeric(grep("^ORF$", colnames(tb)))
        ll<-as.numeric(grep("^logFC$", colnames(tb)))
        ae<- as.numeric(grep("^AveExpr$", colnames(tb)))
        tt<- as.numeric(grep("^t$", colnames(tb)))
        pv<- as.numeric(grep("^P.Value$", colnames(tb)))
        ap<- as.numeric(grep("^adj.P.Val$", colnames(tb)))
        bb<- as.numeric(grep("^B$", colnames(tb)))
        gg<- as.numeric(grep("^Gene.Symbol$", colnames(tb)))
        
        vv <- paste(tb[[j,jj]])
        
        log <- paste(tb[[j,ll]])
        
        ave <- paste(tb[[j,ae]])
        tts <- paste(tb[[j,tt]])
        pva <- paste(tb[[j,pv]])
        adp <- paste(tb[[j,ap]])
        bbb <- paste(tb[[j,bb]])
        ggg <- paste(tb[[j,gg]])
        
        
        
        genelist<-rbind(genelist, data.frame(locus = loc,
                                         cds = paste(cds),
                                         desc = desc,
                                         log = log,
                                         avexpr = ave,
                                         ttest = tts,
                                         pval = pva,
                                         adjpval = adp,
                                         B = bbb,
                                         gene = paste(gene),
                                         CAI = 0
        ))
        
        
        
        
        
        
        
        break
        
        
        
        
        
        
        
        
        
      }#if
      
      
    }#for
  

  

  #View(gtlist)
  return(genelist)

  
  
}




findStartCodon <- function(cds, y)
{
  ##Codon start also the non-standard
  ##tuned for e.coli.
  vett <- c("ATG","GTG","ATT")
  
  
  
  if(y == "-")
  {
    cds<-reverse(cds)
    
    
  }
  
  flag<-0
  jk<-0
  for(i in 1:length(vett))
  {   
    
    mk<-matchPattern(paste(vett[i]),cds)
    #print(mk)
    if(length(paste(mk))!=0){
      jk<-mk[1]@ranges@start
    
    }
    
    if(jk!=1)
    {
      
      cds <- reverse(cds)
      mk<-matchPattern(paste(vett[i]),cds)
      #print(mk)
      if(length(paste(mk))!=0){
        jk<-mk[1]@ranges@start
        
      }
      if(jk!=1)
      {
      cds<-reverse(cds)}
      else{ flag<-1
            
            break}
      
    } else{ flag<-1
            
            break}
    
  }
  
  
  if(flag==0)
  {
    cds<-complement(cds)
    
    jk<-0
    for(i in 1:length(vett))
    {   
      
      mk<-matchPattern(paste(vett[i]),cds)
      #print(mk)
      if(length(paste(mk))!=0){
        jk<-mk[1]@ranges@start
        
      }
      
      if(jk!=1)
      {
        
        cds <- reverse(cds)
        mk<-matchPattern(paste(vett[i]),cds)
        print(mk)
        if(length(paste(mk))!=0){
          jk<-mk[1]@ranges@start
          
        }
        if(jk!=1)
        {
         cds<-reverse(cds)}
        else{ flag<-1
              
              break}
        
      } else{ flag<-1
              
              break}
      
    }
    
  }
  
  l <- list(flag, cds)
  
  return(l)
  
}



GetTopList <- function(anngenes,tb, n, gtlist, DNA)
{
  
  count<-0
  for(i in 1: length(tb[,1]))
  {
    
    jj<- as.numeric(grep("^ORF$", colnames(tb)))
    ll<-as.numeric(grep("^logFC$", colnames(tb)))
    ae<- as.numeric(grep("^AveExpr$", colnames(tb)))
    tt<- as.numeric(grep("^t$", colnames(tb)))
    pv<- as.numeric(grep("^P.Value$", colnames(tb)))
    ap<- as.numeric(grep("^adj.P.Val$", colnames(tb)))
    bb<- as.numeric(grep("^B$", colnames(tb)))
    gg<- as.numeric(grep("^Gene.Symbol$", colnames(tb)))
    
    vv <- paste(tb[[i,jj]])
    #print(vv)
    log <- paste(tb[[i,ll]])
    ave <- paste(tb[[i,ae]])
    tts <- paste(tb[[i,tt]])
    pva <- paste(tb[[i,pv]])
    adp <- paste(tb[[i,ap]])
    bbb <- paste(tb[[i,bb]])
    ggg <- paste(tb[[i,gg]])
    
    if(nchar(vv) > 0)
    {
      
      #sub<-substr(vv, 3, nchar(vv))
      
      for(i in 1: length(anngenes[,1]))
      {
        ## to modify the regular expression is not generalized
        if(any(grep(vv, paste(values(anngenes)$locus[i]))))
        {
          count<-count+1
          flag<-1
          a<-ranges(anngenes[i])
          min <- min(a)
          max <- max(a)
          
          ss <- strand(anngenes[i])
          y <- paste(runValue(ss))
          
         # print(y)
          cds<-substr(DNA, min, max)
          l<- findStartCodon(cds,y)
          if(l[[1]] ==1)
         
          #values(met[i])$subseq <- paste(substr(genoma, min, max))
          cds = l[[2]]
          
            
          gtlist<-rbind(gtlist, data.frame(locus = paste(values(anngenes)$locus[i]),
                                           cds = paste(cds),
                                           desc = paste(values(anngenes)$description[i]),
                                           log = log,
                                           avexpr = ave,
                                           ttest = tts,
                                           pval = pva,
                                           adjpval = adp,
                                           B = bbb,
                                           gene = ggg
          ))
          
          
          break
        }#if
        
        
      }#for
      
      
      
    }#if
    
    
    
    if(count == n)
    {break}
    
    
    
  }#
  #View(gtlist)
  return(gtlist)
  
}




######## ftp #############################################
#.asn  genome record in asn.1 format 
#.faa	protein sequences in fasta format, text file
#.ffn	protein coding portions of the genome segments
#.fna	genome fasta sequence
#.frn	rna coding portions of the genome segments
#.gbk	genome in genbank file format 
#.gff	genome features
#.ptt	protein table
#.rnt	rna table
#.rpt	summary report
#.val	binary file (genome project?)
##########################################################


getAnnotation <- function(ds)
{
  ann<- read.ncbi.ftp(paste(ds), "gff")
  values(ann)$subseq[1] <- "ND"
  
return(ann)
}



getDNA <- function(ds)
{
  
  dna <- read.ncbi.ftp(paste(ds), "fna")
  return(dna)
}




urlParserftoTONCBI<- function(str1)
{
  
  return(paste(
    gsub("ftp://ftp.ncbi.nih.gov/genomes/Bacteria/",
         "",
         paste(str1))))
  
}


#######################################################################
# codon frequencies -> RSCU-> Wi-> CAI functions
#######################################################################

charString <-function(text)
{

  n <- 3
  b<- substring(text, seq(1, nchar(text)-1, 3), seq(3, nchar(text), 3))
  
  return(b)
}


getCodonAbsFreq <-function(str)
{
  output<-charString(str)
  df <- data.frame()
  df<-rbind(df,cbind(output))
  codon = df$output    
  ## Xi = number of occurrences of codon i
  codon.freq = table(codon)
  
  return(codon.freq)
  
}

###############not used.
getCodonRelFreq<-function(str)
{
  
  output<-charString(str)  
  df <- data.frame()
  df<-rbind(df,cbind(output))
  codon = df$output 
  ## Xi = number of occurrences of codon i
  codon.freq = table(codon)
  codon.relfreq = codon.freq / nrow(df)
  
  return(codon.relfreq)
  
}


##RCSU#######################################################
getRSCU<-function(codon.freq)
{
  rscud <- data.frame(Codon= character(0), aacid= integer(0), rscu = numeric(0), occ = numeric(0))
  names(codon.freq)<-tolower(names(codon.freq))
  #calcolo rscu
  for(a1 in 1:length(aa))
  {
    #print(a1)
    #print(codon.freq)
    u<-df.syn[paste(aa[a1]),]
    u<-u[!is.element(u, c("*"))]
    #print(u)
    ca<-codon.freq[u]
    names(ca)<-tolower(names(ca))
    
    #print(ca)
    
    if(sum(!is.na(ca))>0){
      maxv <- 0
      sum<-0
      
      ca <- ca[!is.na(ca)]
      for(dd in 1:length(ca))
      {
        sum <- ca[[dd]] + sum

      }
      denominatore <- sum/length(ca)
      
      for(dd1 in 1:length(ca))
      {

        rc<- ca[[dd1]]/denominatore
       
        rscud <- rbind(rscud,data.frame(Codon= paste(names(ca[dd1])),
                                        aacid = paste(aa[a1]),
                                        rscu=rc,
                                        occ=ca[[dd1]])) 
      }
        
      
    }
  }##for
  
  
  return(rscud)
  
  
}

####################################################################
############  select Ribosomal cds's and their correlated expression
####################################################################
selectRib <- function(gtlist, riblist)
{
  
  
  temp <- data.frame(locus<-character(0), seq<-character(0), desc<-character(0), log = numeric(0)
                        , aveexpr = numeric(0), ttest = numeric(0), pval = numeric(0), adjpval = numeric(0)
                        , B = numeric(0), gene=character(0))
  
  for(i in 1: length(gtlist$desc))
  {
    
    if(any(grep("ribosomal", paste(gtlist$desc[i]))))
    {
      
      
      temp<-rbind(temp, data.frame(locus = paste(gtlist$locus[i]),
                                           cds = paste(gtlist$cds[i]),
                                           desc = paste(gtlist$desc[i]),
                                           log = paste(gtlist$log[i]),
                                           avexpr = paste(gtlist$avexpr[i]),
                                           ttest = paste(gtlist$ttest[i]),
                                           pval = paste(gtlist$pval[i]),
                                           adjpval = paste(gtlist$adjpval[i]),
                                           B = paste(gtlist$B[i]),
                                           gene = paste(gtlist$gene[i])
      ))
    }
    
    
    
  }
  
  
  
  for(i in 1: length(temp$desc))
  {
    ## probably is not allways sufficient this type of selection with only
    ## this two types of keywords.
    if(any(grep("protein", paste(temp$desc[i]))))
    {
      
      
      #print(paste(temp$desc[i]))
      
      riblist<-rbind(riblist, data.frame(locus = paste(temp$locus[i]),
                                         cds = paste(temp$cds[i]),
                                         desc = paste(temp$desc[i]),
                                         log = paste(temp$log[i]),
                                         avexpr = paste(temp$avexpr[i]),
                                         ttest = paste(temp$ttest[i]),
                                         pval = paste(temp$pval[i]),
                                         adjpval = paste(temp$adjpval[i]),
                                         B = paste(temp$B[i]),
                                         gene = paste(temp$gene[i])
      ))
    }
    
    
    
  }
  
  
  
  
  
  
  
  return(riblist)
  
}




#optimized
getRSCUTable1 <- function(riblist)
{
  
  a <-""
  
  for(i in 1:length(riblist[,1]))
  {
    a <- paste(a, paste(riblist$cds[[i]]), sep="")
    
  }
  
  codon.freq <-getCodonAbsFreq(a)
  names(codon.freq)<-tolower(names(codon.freq))
  
  rc<-getRSCU(codon.freq)
  
  return(rc)
  
}



getRelativeAdapt <- function(tbrscu)
{
  
  
  naa <- table(tbrscu$aa)
  start<- 1
  w <- data.frame(Codon= character(0), aacid= integer(0), rscu = numeric(0), occ = numeric(0), w = numeric(0))
  
  
  for(j in 1: length(naa))
  {
    stopc<- start + naa[[j]]
    max <- 0
    
    for(k in start:(stopc-1))
    {
      
      if(as.numeric(tbrscu$rscu[k]) > max)
      {
        
        max <- as.numeric(tbrscu$rscu[k])
      }
      
    }
    
    for(m in start:(stopc-1))
    {
      
      
      wi<-as.numeric(tbrscu$rscu[m])/as.numeric(max)

     
     w <- rbind(w, data.frame(Codon= tbrscu$Codon[m],
                              aacid= tbrscu$aacid[m], 
                              rscu = tbrscu$rscu[m], 
                              occ = tbrscu$occ[m], 
                              w = as.numeric(wi)
                              ))
      
      
    }
    
    start <- stopc
    
    
  }
    
  
  return(w)
}



getCAI<-function(dtwi1, gttest)
{
  
 codon.abfreq <- getCodonAbsFreq(paste(gttest$cds))
 
 met<-as.numeric(grep("^ATG$", names(codon.abfreq)))
 if(length(met)!=0)
   codon.abfreq<-codon.abfreq[-met]
 
 wmet<-as.numeric(grep("atg", dtwi1$Codon))
 if(length(wmet)!=0)
   dtwi1<-dtwi1[-wmet,]
 
 trip<-as.numeric(grep("^TGG$", names(codon.abfreq)))
 if(length(trip)!=0)
   codon.abfreq<-codon.abfreq[-trip]
 
 wtrip<-as.numeric(grep("tgg", dtwi1$Codon))
 if(length(wtrip)!=0)
   dtwi1<-dtwi1[-wtrip,]
 

 s1<-as.numeric(grep("^TAA$", names(codon.abfreq)))
 if(length(s1)!=0)
   codon.abfreq<-codon.abfreq[-s1]
 s2<-as.numeric(grep("^TAG$", names(codon.abfreq)))
 if(length(s2)!=0)
   codon.abfreq<-codon.abfreq[-s2]
 s3<-as.numeric(grep("^TGA$", names(codon.abfreq)))
  if(length(s3)!=0)
   codon.abfreq<-codon.abfreq[-s3]
 
 names(codon.abfreq)<-tolower(names(codon.abfreq))
 l<-sum(codon.abfreq)
 wix<-0
 m<-0
 
 for(i in 1:length(dtwi1$occ))
 {
   
   er<-codon.abfreq[dtwi1$Codon[i]]

   ind<-grep(names(er[1]), dtwi1$Codon)
   
   if(!is.na(er[[1]])){
     ko <- as.numeric(paste(er[[1]]))

    for(j in 1:ko){

     ko1<- as.numeric(dtwi1$w[ind])

     wix <- wix + log(ko1)
     
    }

   }
   
 }
   
wix <- wix/l
wix<- exp(wix)
  
  return(wix)
  
}


## ESCHERICHIA COLI
## For example: we want to study the bacteria e.coli
##
## http://www.ncbi.nlm.nih.gov/genome/?term=Escherichia%20Coli%20K%2012
##
## we can search an e.coli updated dataset across the ftp NCBI
## by a keyword like "coli"

ftp<-readNCBIftp("coli")

## we want to study a ref genoma K-12
## we can select this type of genoma in the ftp-list 
## (in this case: ftp$ftp[17] for Escherichia coli strain K-12 substrain MG1655)

ftpurl<- urlParserftoTONCBI(ftp$ftp[17])

### We need a GDS and a Genoma correlated.

anngenes <- getAnnotation(ftpurl)
DNA<-getDNA(ftpurl)

## we can fetch the expression dataset fully annotated by ncbiGDS's
## http://www.ncbi.nlm.nih.gov/gds/?term=Escherichia+coli+K+12+GDS2768
##
tb1 <- getTopGene("GDS2768") 


### GetTopList fetch the gene expression and the cds by their locus tag.

gtlist <- data.frame(locus<-character(0), seq<-character(0), desc<-character(0), log = numeric(0)
                     , aveexpr = numeric(0), ttest = numeric(0), pval = numeric(0), adjpval = numeric(0)
                     , B = numeric(0), gene<- character(0))

## 30 minuts for 1000 genes, the method must be improved.

gtlist <- GetTopList(anngenes, tb1, 10, gtlist, DNA) 

## If we want to add another GDS in gtlist:
#tb1 <- getTopGene("GDSxxxx")
#gtlist <- GetTopList(anngenes, tb1, 10, gtlist, DNA)

##########################################################################
# No-ribosomal criterion
# tbrcsu1<-getRSCUTable1(gtlist)
# dtwi <- getRelativeAdapt(tbrcsu1)
# etc...
##########################################################################
###
### Ribosomal Criterion
###
##########################################################################

riblist <- data.frame(locus<-character(0), seq<-character(0), desc<-character(0), log = numeric(0)
                      , aveexpr = numeric(0), ttest = numeric(0), pval = numeric(0), adjpval = numeric(0)
                      , B = numeric(0), gene=character(0))

riblist<-selectRib(gtlist, riblist)
tbrcsu1<-getRSCUTable1(riblist)
dtwi <- getRelativeAdapt(tbrcsu1)


##########################################################################
## codon usage for this test-set:

pr<-c("rpsU","rpsA", "rimK", "cspC", "hsdS", "lacI", "trpR",
      "gpmA", "hns", "hupA", "icdA", "ilvC", "lpp", "metK", "mopA",
       "ompA", "ompC","ompF", "ppa", "ptsH", "tig","tufA","yjgF")

genelist <- data.frame(locus<-character(0), seq<-character(0), desc<-character(0), log = numeric(0)
                 , aveexpr = numeric(0), ttest = numeric(0), pval = numeric(0), adjpval = numeric(0)
                 , B = numeric(0), gene=character(0), CAI=numeric(0) )

for(k in 1: length(pr))
{
  gg <- data.frame(locus<-character(0), seq<-character(0), desc<-character(0), log = numeric(0)
                         , aveexpr = numeric(0), ttest = numeric(0), pval = numeric(0), adjpval = numeric(0)
                         , B = numeric(0), gene=character(0), CAI=numeric(0) )
  
  gg1<- SelGene(anngenes, tb1, pr[k], gg, DNA)
  gg1$CAI<-getCAI(dtwi,gg1)
  genelist<-rbind(genelist, gg1)
}


View(genelist)
#View(riblist)
##Test1 11 April PDFs OK Pietro
##View(genelist[,c(1,3,4,7,11)])
##View(riblist[,c(1,3,4,5,6,7,8,9,10)])

#############################################################################
##
##    Correlation between CAI and GeneExpression 
##
##    Ordinary Regression Model and Correctness 
##
##    CAI = Dipendent Variable
##    gene expression = Indipendent Variable
##
##    Test 24 April Ok Pdf Attached
#############################################################################

log<-as.numeric(type.convert(paste(genelist$log)))
cai<-as.numeric(genelist$CAI)
plot(cai~log , xlab="log-expr", ylab="CAI", pch=".")
text(x=log, y=cai, labels = genelist[1:23,1], cex=.80, xpd=TRUE)

c(mean(log), mean(cai))
points(mean(log), mean(cai), pch="O", col="blue")
print(a1<-cov(log,cai)/sd(log)^2)
print(a0<- mean(cai)-a1*mean(log))
abline(a0,a1,col="blue")


pval<-as.numeric(genelist$pval)
avexp<-as.numeric(genelist$avexpr)

fm1<-lm(formula = cai ~ log)

summary(fm1)

X<-model.matrix(fm1)

coef(fm1)

plot(fm1)
text(x=log, y=cai, labels = genelist[1:23,1], cex=.80, xpd=TRUE)

X<-model.matrix(fm)
plot(summary(fm1,correlation=T)$correlation)

residui<- residuals(fm1)
t.test(residui)

shapiro<-shapiro.test(residui)


modello<-as.numeric(genelist$CAI)~as.numeric(type.convert(paste(genelist$log)))

testbp<-bptest(modello,data=genelist)
testbp
dw<-dwtest(modello,data=genelist)
dw
