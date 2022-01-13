
# brown bears

## 1) identify which positions are SNPs

### read data
setwd("~/PopGen/Imperial_College/GenomicsBioinformatics/genomics_and_bioinformatics/Practicals/Practicals2021/Genomics_Bionformatics_Practicals_2021/")
data <- read.csv("./bears.csv", stringsAsFactors=F, header=F, colClasses=rep("character", 10000))
dim(data)
data[,5]
data[1,(c(1:100))]



### SNPs are positions where you observed more than one allele
### the easiest thing is to loop over all sites and record the ones with two alleles

data[,3]
data[,262]
unique(data[,3])
length(unique(data[,3]))
length(unique(data[,3]))==2
unique(data[,262])
length(unique(data[,262]))
length(unique(data[,262]))==2





snps <- c()
for (i in 1:ncol(data)) {
	if (length(unique(data[,i]))==2){
	  snps <- c(snps, i)
	}
}
### this works to retain the indexes of SNPs; a smartest way would not involve doing a loop but using `apply` functions
cat("\nNumber of SNPs is", length(snps))

### reduce the data set
data <- data[,snps]
dim(data)


## 2) calculate, print and visualise allele frequencies

### alleles in this SNP
alleles <- unique(data[,1])
cat("\nSNP", "with alleles", alleles)

0 0 0 0 1 
1 0 0 1 0

## frequencies of the alleles
freq_a1<-length(which(data[,1]==alleles[1]))/nrow(data)
freq_a2<-length(which(data[,1]==alleles[2]))/nrow(data)

### the minor allele is the one in lowest frequency
minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
freq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]

cat(" the minor allele is",minor_allele ,"and the minor allele frequency (MAF) is", freq_minor_allele)





### again we can loop over each SNP and easily calculate allele frequencies
frequencies <- c()
for (i in 1:ncol(data)) {

        ### alleles in this SNP
        alleles <- sort(unique(data[,i]))
        cat("\nSNP", i, "with alleles", alleles)
        
        ## frequencies of the alleles
        freq_a1<-length(which(data[,i]==alleles[1]))/nrow(data)
        freq_a2<-length(which(data[,i]==alleles[2]))/nrow(data)
        
        ### the minor allele is the one in lowest frequency
        minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
        freq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]

        cat(" the minor allele is",minor_allele ,"and the minor allele frequency (MAF) is", freq_minor_allele)

        frequencies <- c(frequencies, freq_minor_allele)
}
### we can plot is as a histogram
hist(frequencies)
### or simply the frequencies at each position
plot(frequencies, type="h")


## 3) calculate and print genotype frequencies

### alleles in the first SNP
alleles <- unique(data[,1])
cat("\nSNP", i, "with alleles", alleles)

## frequencies of the alleles
freq_a1<-length(which(data[,1]==alleles[1]))/nrow(data)
freq_a2<-length(which(data[,1]==alleles[2]))/nrow(data)

### the minor allele is the one in lowest frequency
minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
freq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]

genotype_counts <- c(0, 0, 0)

nsamples <- 20
for (j in 1:nsamples) {
  ### indexes of haplotypes for individual j (haplotype indices)
  haplotype_index <- c( (j*2)-1, (j*2) )
  ### count the minor allele instances
  genotype <- length(which(data[haplotype_index, 1]==minor_allele)) 
  ##
  genotype_index=genotype+1
  ### increase the counter for the corresponding genotype
  genotype_counts[genotype_index] <- genotype_counts[genotype_index] + 1
}
cat(" and genotype frequencies", genotype_counts)



### again, we can loop over each SNPs and each individual and print the genotype frequencies
nsamples <- nrow(data)/2
for (i in 1:ncol(data)) {

  alleles <- sort(unique(data[,i]))
  cat("\nSNP", i, "with alleles", alleles)
  
  ## frequencies of the alleles
  freq_a1<-length(which(data[,i]==alleles[1]))/nrow(data)
  freq_a2<-length(which(data[,i]==alleles[2]))/nrow(data)
  
  ### as before, as there is no "reference" allele, we calculate the frequencies of the minor allele
  ### the minor allele is the one in lowest frequency
  minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
  freq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]

  ### genotypes are major/major major/minor minor/minor
	genotype_counts <- c(0, 0, 0)

	for (j in 1:nsamples) {
	  ### indexes of haplotypes for individual j (haplotype indices)
	  haplotype_index <- c( (j*2)-1, (j*2) )
	  ### count the minor allele instances
	  genotype <- length(which(data[haplotype_index, i]==minor_allele)) 
	  ##
	  genotype_index=genotype+1
	  ### increase the counter for the corresponding genotype
	  genotype_counts[genotype_index] <- genotype_counts[genotype_index] + 1
	}
	cat(" and genotype frequencies", genotype_counts)
}


## 4) calculate and print homozygosity and heterozygosity

### we can reuse the previous code and easily calculate the heterozygosity
nsamples <- 20
for (i in 1:ncol(data)) {

        ### alleles in this SNPs
        alleles <- sort(unique(data[,i]))
        cat("\nSNP", i, "with alleles", alleles)
        
        ## frequencies of the alleles
        freq_a1<-length(which(data[,i]==alleles[1]))/nrow(data)
        freq_a2<-length(which(data[,i]==alleles[2]))/nrow(data)
        
        ### as before, as there is no "reference" allele, we calculate the frequencies of the minor allele
        ### the minor allele is the one in lowest frequency
        minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
        freq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]
        
        ### genotypes are major/major major/minor minor/minor
        genotype_counts <- c(0, 0, 0)

        for (j in 1:nsamples) {
          ### indexes of haplotypes for individual j (haplotype indices)
          haplotype_index <- c( (j*2)-1, (j*2) )
          ### count the minor allele instances
          genotype <- length(which(data[haplotype_index, i]==minor_allele)) 
          ##
          genotype_index=genotype+1
          ### increase the counter for the corresponding genotype
          genotype_counts[genotype_index] <- genotype_counts[genotype_index] + 1
        }
        cat(" and heterozygosity", genotype_counts[2]/nsamples)
        cat(" and homozygosity", 1-genotype_counts[2]/nsamples)
}


## 5) test for HWE, with calculating of expected genotype counts

nonHWE <- c() # to store indexes of SNPs deviating from HWE
nsamples <- 20
for (i in 1:ncol(data)) {

  
        ### alleles in this SNPs
        alleles <- sort(unique(data[,i]))
        cat("\nSNP", i, "with alleles", alleles)
        
        ## frequencies of the alleles
        freq_a1<-length(which(data[,i]==alleles[1]))/nrow(data)
        freq_a2<-length(which(data[,i]==alleles[2]))/nrow(data)
        
        ### as before, as there is no "reference" allele, we calculate the frequencies of the minor allele
        ### the minor allele is the one in lowest frequency
        minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
        ffreq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]
        
        ### from the frequency, I can calculate the expected genotype counts under HWE p^2,2pq,q^2
	      genotype_counts_expected <- c( (1-freq_minor_allele)^2, 2*freq_minor_allele*(1-freq_minor_allele), freq_minor_allele^2) * nsamples

	      ### genotypes are major/major major/minor minor/minor
	      genotype_counts <- c(0, 0, 0)

	      for (j in 1:nsamples) {
	        ### indexes of haplotypes for individual j (haplotype indices)
	        haplotype_index <- c( (j*2)-1, (j*2) )
	        ### count the minor allele instances
	        genotype <- length(which(data[haplotype_index, i]==minor_allele)) 
	        ##
	        genotype_index=genotype+1
	        ### increase the counter for the corresponding genotype
	        genotype_counts[genotype_index] <- genotype_counts[genotype_index] + 1
	      }

      	### test for HWE: calculate chi^2 statistic
      	chi <- sum( (genotype_counts_expected - genotype_counts)^2 / genotype_counts_expected )
      
      	## pvalue
      	pv <- 1 - pchisq(chi, df=1)
        cat(" with pvalue for test against HWE", pv)
      
      	## retain SNPs with pvalue<0.05
      	if (pv < 0.05) nonHWE <- c(nonHWE, i)

}



## 6) calculate, print  and visualise inbreeding coefficients for SNPs deviating from HWE

### assuming we ran the code for point 5, we already have the SNPs deviating
F <- c()
nsamples <- 20
for (i in nonHWE) {

        ### alleles in this SNPs
        alleles <- sort(unique(data[,i]))
        cat("\nSNP", i, "with alleles", alleles)
        
        ## frequencies of the alleles
        freq_a1<-length(which(data[,i]==alleles[1]))/nrow(data)
        freq_a2<-length(which(data[,i]==alleles[2]))/nrow(data)
        
        ### as before, as there is no "reference" allele, we calculate the frequencies of the minor allele
        ### the minor allele is the one in lowest frequency
        minor_allele<-alleles[which.min(c(freq_a1,freq_a2))]
        freq_minor_allele<-c(freq_a1,freq_a2)[which.min(c(freq_a1,freq_a2))]
        
        ### from the frequency, I can calculate the expected genotype counts under HWE p^2,2pq,q^2
        genotype_counts_expected <- c( (1-freq_minor_allele)^2, 2*freq_minor_allele*(1-freq_minor_allele), freq_minor_allele^2) * nsamples
        
        ### genotypes are major/major major/minor minor/minor
        genotype_counts <- c(0, 0, 0)

        for (j in 1:nsamples) {
          ### indexes of haplotypes for individual j (haplotype indices)
          haplotype_index <- c( (j*2)-1, (j*2) )
          ### count the minor allele instances
          genotype <- length(which(data[haplotype_index, i]==minor_allele)) 
          ##
          genotype_index=genotype+1
          ### increase the counter for the corresponding genotype
          genotype_counts[genotype_index] <- genotype_counts[genotype_index] + 1
        }

	### calculate inbreeding coefficient
	inbreeding <- ( 2*freq_minor_allele*(1-freq_minor_allele) - (genotype_counts[2]/nsamples) ) / ( 2*freq_minor_allele*(1-freq_minor_allele) )
	F <- c(F, inbreeding)
	cat(" with inbreeding coefficient", inbreeding)
}
### plot
hist(F)
plot(F, type="h")




