devtools::install_github("carloscinelli/benford.analysis", build_vignettes = TRUE)
library(benford.analysis) 
# loads package
x=data(corporate.payment) # loads data
bfd.cp <- benford(corporate.payment$Amount)
plot(bfd.cp)

write.csv(data.frame(corporate.payment$Amount),file = "ben.csv")
