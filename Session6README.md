# DS-BIO
echo "# DS-BIO" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin git@github.com:MarcosRamez/DS-BIO.git
git push -u origin main

#R-CODE
install.packages("tidyverse")
library(tidyverse)
genotypes<-read_tsv("~/Desktop/DS-BIO/SESSION_6/GWAS_data/Chr12_Genotypes.tsv")
map<-read_tsv("~/Desktop/DS-BIO/SESSION_6/GWAS_data/Chr12_Map.tsv")
phenotypes<-read_tsv("~/Desktop/DS-BIO/SESSION_6/GWAS_data/Phenotype_Height.tsv") #naming the data in enviroment


ggplot(phenotypes, aes(x=Height)) + geom_histogram()
ggplot(map, aes(x=Position)) + geom_histogram()

row1 = genotypes[2,] ###converting it into vector
one_geno = as.numeric(row1)
one_geno = as.numeric(genotypes[2,])

one_geno = as.numeric(genotypes[2,])

mean(one_geno)
?mean

one_geno
mean(one_geno, na.rm=TRUE)/2

mean(as.numeric(genotypes[2,]), na.rm=TRUE)/2 ####nesting

apply(genotypes,1,mean,na.rm=TRUE)/2

allele_frequencies = apply(genotypes,1,mean,na.rm=TRUE)/2
plot_data = tibble(allele_frequencies)

ggplot(plot_data, aes(allele_frequencies)) + geom_histogram() #### same function as below
ggplot(tibble(allele_frequencies), aes(x=allele_frequencies)) + geom_histogram() #### same function as top

phenotypes$geo = one_geno
phenotypes$one_geo = as.numeric(one_geno)

ggplot(phenotypes, aes(x=one_geno, y=Height)) +
  geom_jitter(width = 0.2) +
  geom_smooth(method = 'lm')
#using pipiline code to remove geo colum ad saving it in test dataframe
test <- phenotypes %>%
  select(-geo)

phenotypes <- phenotypes %>%
  select(-geo)

rm(test)
phenotypes %>%
  select(-geo)%>%
  ggplot( aes(x=one_geno, y=Height)) +
  geom_jitter(width = 0.1) +
  geom_smooth(method = 'lm')

?geom_smooth

str(phenotypes)

phenotypes$Sample_Name

corTestResult = cor.test(phenotypes$Height, phenotypes$one_geo)
corTestResult$p.value

one_geno = as.numeric (genotypes[2,])
test_result = cor.test(phenotypes$one_geo, phenotypes$Height)
test_results$p.value #we want to repeat it a hundred times

### we did a sample function
example_function = function(inputA, inputB){
  step1 = inputA^2 + inputB
  step1 + 5
}
example_function(3,3)

###to get the p-value we did an association test
#we did a function
association_test = function(inputA, inputB){
  test_result = cor.test(inputA, inputB)
  test_result$p.value
}
association_test(one_geno, phenotypes$Height)
map$pvalues= apply(genotypes, 1, association_test,inputB=phenotypes$Height)
ggplot(map, aes(x=Position, y=-log10(pvalues))) + geom_point()
