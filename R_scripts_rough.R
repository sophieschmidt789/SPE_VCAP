library(tidyr)
library(tidyverse)
library(readxl)
library(arsenal)
library(dplyr)
library(tidyverse)
library(stats)
library(reshape)
library(reshape2)
library(openxlsx)
library(readr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~For REAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Data Set 1: Supplemental_dataset_s1.xlsx
#1. Use merged dataset of three sheets for ear, flagleaf, and tassel
sheet = excel_sheets("Supplemental_dataset_S1.xlsx")
data_set_1 = lapply(setNames(sheet, sheet), 
                    function(x) read_excel("Supplemental_dataset_S1.xlsx", sheet=x))                    
data_set_1 = bind_rows(data_set_1, .id="Sheet") #all 3 sheets have been added together, resulting in 18,718 rows with 'Sheet' column containing tissue label

data_set_1 <- data_set_1 %>% 
  mutate(tissue = Sheet) # this makes a column titled tissue containing the tisue ID (originally called sheet but wouldn't let me change column name)


#2. Convert NA/x absence/presence system to a 1/0 binary where x = 1 and NA = 0
data_set_101 <- mutate(data_set_1,across(c(9:20),
                                         ~factor(ifelse(.x == "NA",0 ,1)))) #I created a new data set where all the NA/X notation for presence/absence of SPE gene is turned into a O/1 biary where 0 = NA and 1 = x
#head(data_set_101)
#view(data_set_101) #data_set_101 is what I should use as my working dataset for now for data set 1           

#3. Add B and X SPE specifier together, 0 = absence, 1 = presence and label crosses in universal manner for all 3 data sets
colnames(data_set_101)
unique(data_set_101$SPE_B_A554_) #shows that column contains all 0s and 1s
data_set_101$SPE_B_A554_<-as.numeric(data_set_101$SPE_B_A554_) #changes column from 2/factor identifier to numeric values, however, all 0 turn to 1 and all 1 turn to 2
data_set_101$SPE_X_A554_<-as.numeric(data_set_101$SPE_X_A554_) #same for all other columns
data_set_101$SPE_B_H84_<-as.numeric(data_set_101$SPE_B_H84_)
data_set_101$SPE_X_H84_<-as.numeric(data_set_101$SPE_X_H84_)
data_set_101$SPE_B_H99_<-as.numeric(data_set_101$SPE_B_H99_)
data_set_101$SPE_X_H99_<-as.numeric(data_set_101$SPE_X_H99_)
data_set_101$SPE_B_Mo17_<-as.numeric(data_set_101$SPE_B_Mo17_)
data_set_101$SPE_X_Mo17_<-as.numeric(data_set_101$SPE_X_Mo17_)
data_set_101$SPE_B_Oh43_<-as.numeric(data_set_101$SPE_B_Oh43_)
data_set_101$SPE_X_Oh43_<-as.numeric(data_set_101$SPE_X_Oh43_)
data_set_101$SPE_B_W64A_<-as.numeric(data_set_101$SPE_B_W64A_)
data_set_101$SPE_X_W64A_<-as.numeric(data_set_101$SPE_X_W64A_)

view(data_set_101) #see that all data is now in 1/2

data_set_101$B73xA554<-rowSums(data_set_101[9:10])#adds columns together into new column, however, 2 represents 0 and 3 represents 1  
data_set_101$B73xH84<-rowSums(data_set_101[11:12])#same for H84 cross, makes one column
data_set_101$B73xH99<-rowSums(data_set_101[13:14])
data_set_101$B73xMo17<-rowSums(data_set_101[15:16])
data_set_101$B73xOh43<-rowSums(data_set_101[17:18])
data_set_101$B73xW64A<-rowSums(data_set_101[19:20])
#view(data_set_101) #see it adds 6 new columns titled for the appropriate cross, is addition of X and B columns from earlier, though values are 2/3

data_set_101 <-
  data_set_101 %>% 
  mutate(B73xA554 = as.numeric(data_set_101$B73xA554 >2),
         B73xH84 = as.numeric(data_set_101$B73xH84 >2),
         B73xH99 = as.numeric(data_set_101$B73xH99 >2),
         B73xMo17 = as.numeric(data_set_101$B73xMo17 >2),
         B73xOh43 = as.numeric(data_set_101$B73xOh43 >2),
         B73xW64A = as.numeric(data_set_101$B73xW64A >2)) #mutates B73xA554 column from 2/3 to 0/1 

names(data_set_101)[65] <- "dataset" #this is renaming Data Set column to dataset for ease of access 

#Data Set 2: Supplemental8_SPE_PAV_gene_list (COPY).xlsx ~~~~ This one is a little wonky as it is a vertical list - look up how to go vertical to horizontal
data_set_2 <- read_xlsx("Supplemental8_SPE_PAV_gene_list (COPY).xlsx")
data_set_2<-mutate(data_set_2,cross=str_c(Female,Male, sep = "x")) #creates new column titled 'cross' that contains Female and Male cross united by x from columns 'Male' and 'Female'
view(data_set_2) #confirms that new column shows up
unique(data_set_2$cross) #shows a total of 89 unique crosses #need to make melted data set first???????????
data_set_2_test<-as.tibble(data_set_2)
data_set_2_mod <- data_set_2_test %>% select(geneID, cross, tissue) #simplifying data set to just have geneID, cross, and Tissue columns
#view(data_set_2_mod)
data_set_2_melt <- melt(data_set_2_mod, id = c("geneID", "tissue")) #melting data set
data_set_2_melt <- data_set_2_melt %>% add_column(Presence = 1) #adding a column of just ones to indicate presnece for when I cast the data? #I didn't actually need this
#view(data_set_2_melt)

data_set_2_cast <- cast(data_set_2_melt, geneID + tissue ~ value, mean, fill=0, add.missing=FALSE) #this casts the data, filling all missing values with 0
data_set_2_cast <- replace(data_set_2_cast, is.na(data_set_2_cast), 1) #replaces all NA values with 1
data_set_2_cast <- data_set_2_cast %>% add_column(dataset = 2) #this adds column for dataset identifier
colnames(data_set_2_cast)
data_set_2_cast <- as.data.frame(data_set_2_cast)
str(data_set_2_cast)#yay! shows 1 for presence 0 for absence 

#Data Set 3 (edited): data_set_3
#In V3 right now, convert to V4
V3V4_conversions <- read_xlsx("V3 V4 conversions.xlsx") #create dataframe for v3v4 conversions
v3data_set_3 <- read_xlsx("data_set_3.xlsx") #create dataframe for data set 3 that's currently v3
filter(V3V4_conversions, v3geneID %in% v3data_set_3$"v3geneID") #results in tibble with 38 747 rows, this is finding matches from v3geneid in master spreadsheet to genes in the gene_ID colum in data set 3
v4data_set_3 <- v3data_set_3 %>% inner_join(V3V4_conversions,by="v3geneID") #this is building a data frame with a matched column for the v4geneID to the v3geneID from the key
colnames(v4data_set_3) #shows that there is a v4geneID column after everything else, also this old script that also worked??? Not sure, ask Sam v4data_set_3 <- %>% inner_join(x=v3data_set_3,y=(V3V4_conversions),by="v3geneID")

#To lump B and X SPE specifier in all three root samples, will need to add all 6 columns together to form one column for each cross
v4data_set_3$B73xA554a<-rowSums(v4data_set_3[,2:7]) #this is adding a new column B73xA554 that has the row sum of columns 2 through 7. 
v4data_set_3$B73xH84a<-rowSums(v4data_set_3[,8:13]) #add new column for B73xH84 sum
v4data_set_3$B73xH99a<-rowSums(v4data_set_3[,14:19]) #add new column for B73xH99 sum
v4data_set_3$B73xMo17a<-rowSums(v4data_set_3[,20:25]) #add new column for B73xMo17 sum
v4data_set_3$B73xOh43a<-rowSums(v4data_set_3[,26:31]) #add new column for B73xOh43 sum
v4data_set_3$B73xW64Aa<-rowSums(v4data_set_3[,32:37]) #add new column for B73xW64A sum
v4data_set_3 #view to confirm that the column showed up
view(v4data_set_3) #yay!

#Turn the added columns for each cross into a TRUE FALSE binary system to indicate presence or absence:
colnames(v4data_set_3)
v4data_set_3$B73xA554 <- as.numeric(v4data_set_3$B73xA554a > 0) #This adds a new column for B73xA554 and changes all values greater than 0 to '1', making a binary system compared to a valued system. Can turn to T/F or Y/N at a later time. 
v4data_set_3$B73xH84 <- as.numeric(v4data_set_3$B73xH84a > 0) #0/1 binary for B73xH84
v4data_set_3$B73xH99 <- as.numeric(v4data_set_3$B73xH99a > 0) #0/1 binary
v4data_set_3$B73xMo17 <- as.numeric(v4data_set_3$B73xMo17a > 0) #0/1 binary
v4data_set_3$B73xOh43 <- as.numeric(v4data_set_3$B73xOh43a > 0) #0/1 binary
v4data_set_3$B73xW64A <- as.numeric(v4data_set_3$B73xW64Aa > 0) #0/1 binary
view(v4data_set_3) #shows new column B73xA554binary at end with only values of 0 and 1
#ummmm trial collumn "newbinary" and "B73xbinary" and "B73xw648" is still showing up in this. Need to delete. 
#colnames(v4data_set_3) #new binary column names all show up here!

names(v4data_set_3)[38] <- "dataset"

#Add a column for tissue with word 'root' all the way down
tissue <- c("root")
v4data_set_3 <- v4data_set_3 %>% 
  add_column(tissue = "root")
colnames(v4data_set_3)
colnames(v4data_set_3)[42]<-"match_ratio" #re-naming Match Ratio column for ease of commands
#To remove all matches that are all 1-to-0 matches
v4data_set_3<-filter(v4data_set_3, match_ratio !="1-to-0")
#view(v4data_set_3)

#~~~~~Now to merge all three data sets into one data set~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1. Stack data_set_101 and v4data_set_3 so that columns allign by geneID, tissue, and all 6 crosses. 
colnames(v4data_set_3)[39]<-"geneID" #this changed the  v4geneID column to just geneID so it can stack with data frame 1 below
stacked_1_and3<-dplyr::bind_rows(data_set_101, v4data_set_3) #this stacks 1 and 3 by geneID, tissue, Data Set, and the 6 common crosses
view(stacked_1_and3)

#2. Merge stacked dataset and data_set_2_cast
merged_data_set <- data_set_2_cast %>% 
  full_join(stacked_1_and3, by = c("geneID", "tissue", "B73xMo17")) #this joins by 3 columns so there aren't 2 B73xMo17 columns in the final
view(merged_data_set) #showing a list
str(merged_data_set)

#3. Pull out geneID, tissue, and all cross columns for simplified list 
colnames(merged_data_set)
view(merged_data_set)
merged_data_set_simple <- select(merged_data_set, c(geneID, tissue, B73xW64A, B73xOh43, B73xH99, B73xH84, B73xA554, B73xMo17, B73xPH207, B84xB73, B84xMo17, B84xPH207, CR1HtxB73, CR1HtxMo17, 
                                                    CR1HtxPH207, DKFAPWxB73, DKFAPWxMo17, DKFAPWxPH207, H99xB73, H99xMo17, H99xPH207, LH123HtxB73, LH123HtxMo17, LH123HtxPH207, LH145xB73, LH145xMo17, LH145xPH207, LH156xB73, LH156xMo17, LH85xB73, 
                                                    LH85xMo17, LH85xPH207, LH93xB73, LH93xMo17, LH93xPH207, Mo17xB73, Mo17xOh43, Mo17xPH207, Mo17xPHG29, Mo44xB73, Mo44xMo17, Mo45xB73, Mo45xMo17, Mo45xPH207, MoGxB73, MoGxMo17, NC230xB73, NC230xMo17, 
                                                    NC230xPH207, NKH8431xB73, NKH8431xMo17, NKH8431xPH207, NKS8326xB73, NKS8326xMo17, NKS8326xPH207, Ny821xB73, Ny821xMo17, Ny821xPH207, Oh43xB73, Oh43xMo17, Oh43xPH207, PH207xB73, PH207xMo17, PHB47xB73, 
                                                    PHB47xMo17, PHB47xPH207, PHG29xB73, PHG29xMo17, PHG29xPH207, PHG35xB73, PHG35xMo17, PHG35xPH207, PHG39xB73, PHG39xMo17, PHG39xPH207, PHG47xB73, PHG47xMo17, PHG47xPH207, PHG50xB73, PHG50xMo17, PHG50xPH207, 
                                                    PHG72xB73, PHG72xMo17, PHG72xPH207, PHJ40xB73, PHJ40xMo17, PHJ40xPH207, PHN11xB73, PHN11xMo17, PHN11xPH207, PHW65xB73, PHW65xMo17, PHW65xPH207, W64AxB73, W64AxMo17, W64AxPH207))
view(merged_data_set_simple)
str(merged_data_set_simple)
#write.xlsx(merged_data_set_simple, "merged_data_set.xlsx")


#4. Set up commands for loop #to figure out commands for loop, then add under forloop, change 1 to id variable. Then create new object to save results from temp
results_dataframetrial <- tibble(geneID = NA, number_SPE_crosses = NA) #this creates an empty dataframe to pipe the outcomes into
unique_gene_list <- unique(merged_data_set$geneID) #pulls unique gene list from merged dataset to use for loop
str(unique_gene_list) #pulls 36587 unique genes
temp <- filter(merged_data_set_simple, geneID==unique_gene_list[4]) #creates df 'temp' that will be overwritten by making a separate tbbl with each unique geneID from the list.
tempsums <- select(temp,-c(geneID, tissue)) %>% #the select function selects all the numerical columns (crosses) from the temp tbbl so there are no strings
  colSums(., na.rm = TRUE) %>% as.data.frame() #this sums down each cross column and saves as a dataframe and creates a tible with cross in one column and sum in the other
colnames(tempsums)[1]<- "x" #sets title of sums column to x instead of '.' which was confusing the program
tempsums$x <- ifelse(tempsums$x>0,1,0) #turns all values in the sums column to 1 if they are greater than 0, effectivley creating a 0/1 binary to allow to sum number of crosses
results_dataframetrial <- add_row(results_dataframetrial, geneID = unique_gene_list[4], number_SPE_crosses=colSums(tempsums)) #adds rows to empty tbbl from before, geneID is pulled from unique gene list and number of SPE crosses is the column sum of x from tempsums binary system
view(results_dataframetrial)

#5. Run the loop on [1:5] to see if it's spitting out what I want

results_dataframe <- tibble(geneID = NA, number_SPE_crosses = NA) #makes empty data frame
str(results_dataframe)
for (id in 1:5) {
  temp <- filter(merged_data_set_simple, geneID==unique_gene_list[id])
  tempsums <- select(temp,-c(geneID, tissue)) %>% 
    colSums(., na.rm = TRUE) %>% as.data.frame()
  colnames(tempsums)[1]<- "x"
  tempsums$x <- ifelse(tempsums$x>0,1,0)
  results_dataframe <- add_row(results_dataframe, geneID = unique_gene_list[id], number_SPE_crosses=colSums(tempsums))
}
view(results_dataframe) #should work

#6. Run the loop on the whole unique geneID
unique_gene_list <- unique(merged_data_set$geneID) #pulls list of unique genes from the master data set
results <- tibble(geneID = NA, number_SPE_crosses = NA) #makes empty data frame with geneID and number_SPE_crosses column
for (id in 1:length(unique_gene_list)) { #uses 'id' variable from 1 to the length of the unique gene list (so basically the number of unique genes)
  temp <- filter(merged_data_set_simple, geneID==unique_gene_list[id]) #creates temporary tbbl for a unique gene ID across all the tissues, will be overwritten each time
  tempsums <- select(temp,-c(geneID, tissue)) %>% #removed geneID and tissue columns because they're strings
    colSums(., na.rm = TRUE) %>% as.data.frame() #creates tbble that sums down each cross column, resulting has one column with all crosses and the next column with the sum 
  colnames(tempsums)[1]<- "x" #renames sums column to x because it was named as '.' when saved as dataframe and R was confused
  tempsums$x <- ifelse(tempsums$x>0,1,0) #turns column with sums to a 0/1 binary which will allow me to add them up to get the total number of crosses that the gene appears as SPE in
  results <- add_row(results, geneID = unique_gene_list[id], number_SPE_crosses=unname(colSums(tempsums))) #creates a df where geneID is pulled form the uniqe list and the nubmer of SPE crosses is the sum of the temp column that is 0/1 binary
  #for some reason it was saving as named numbers so unname is turning named number to just a number so we can do further analysis
}
results <- na.omit(results) #omits NA from row one of the empty results tbble 
view(results) 
str(results)

#to see the frequencies in a table
frequency <- table(results$number_SPE_crosses) %>% as.data.frame() #shows the amount of genes that show up in one SPE cross and saves as dataframe
str(frequency)


#histogram of all genes, 0 bin included
hist(results$number_SPE_crosses, 
     main = "Histogram for SPE Count Frequency", 
     right = FALSE, #makes the bins left closed which includes zero as a bin
     xlab = "Number of Crosses", 
     ylab = "Number of SPE Genes Present",
     border = "gray0",
     col = "chartreuse4",
     xlim = c(0,50),
     breaks = 55,
) 

#histogram of all genes, 0 bin NOTTTT included
results_0 <- results[!apply(results==0, 1, any),] #removes all genes that have 0 value in spe number column
#frequency1 <- table(results_0$number_SPE_crosses) #confimrs that they were removed
#view(frequency1)
hist(results_0$number_SPE_crosses, 
     main = "SPE Count Frequency", 
     right = FALSE, #makes the bins left closed which includes zero as a bin
     xlab = "Number of Crosses", 
     ylab = "Number of SPE Genes Present",
     border = "gray0",
     col = "darkolivegreen3",
     xlim = c(0,50),
     breaks = 55,
) 


#To make a histogram for frequency of SPE genes across all tissues
#Determine which genes are SPE by summing across each row:
merged_data_set_simple$number_crosses <- rowSums(merged_data_set_simple[3:96], na.rm = TRUE) #creates new column adding all of the SPE crosses per tissue/geneID row
merged_data_set_simple$SPE_YN <- ifelse(merged_data_set_simple$number_crosses>0,1,0) #creates new column for 0/1 binary to indicate if gene is SPE or not
view(merged_data_set_simple)

#set up
unique_tissue_list <- unique(merged_data_set$tissue) #makes list of unique tissues in tbble (8)
tissue_results <- tibble(tissue = NA, number_SPE_genes = NA) #makes empty tbble to pipe data into
temps <- filter(merged_data_set_simple, tissue==unique_tissue_list[5]) #creates df to be overwritten 'temps' by pulling all genes for a set tissue call
#view(temp)
tempsum <- select(temps, c(SPE_YN)) %>% #selects just the SPE_YN column from the temps tbble 
  colSums(., na.rm = TRUE) %>% as.data.frame() #Sums down SPE_YN, will give a number of genes that are SPE (meaning they have a 1 in that column) and then saves as dataframe
view(tempsum)

# Actual for loop for tissue frequency
for (t in 1:length(unique_tissue_list)) { #uses variable t from 1 to the number of unique gene tissues (8)
  temps <- filter(merged_data_set_simple, tissue==unique_tissue_list[t]) #creates df to be overwritten 'temps' by pulling all genes for a set tissue call
  tempsum <- select(temps, c(SPE_YN)) %>% #selects just the SPE_YN column from the temps tbble
    colSums(., na.rm = TRUE) %>% as.data.frame() #Sums down SPE_YN, will give a number of genes that are SPE (meaning they have a 1 in that column) and then saves as dataframe
  colnames(tempsum)[1]<- "x" #renames the column with the sums from '.' to 'x' because it was confusing R
  tissue_results <- add_row(tissue_results, #adds row to empty tissue results tbble from earlier
                            tissue = unique_tissue_list[t], #tissue will fill from unique tissue list
                            number_SPE_genes = tempsum) #number of SPE genes will fill from tempsum tbble
}
results <- na.omit(tissue_results) #removes the NA from the first row to clean up df
view(tissue_results) #go back in and remove NA!!!!!
print(tissue_results)

# To make barplot for tissue frequencies
numbers <- c(8591, 5457, 4482, 4074,  3777, 3288, 3188, 2841)
tissuesss <- c("Root", "Endosperm", "Leaf", "Internode",  "Seedling", "Tassel", "Ear", "Flag Leaf")
view(numbers)
barplot(numbers, 
        names.arg=tissuesss,
        xlab = "Tissues",
        ylab = "Number of SPE Genes",
        space = 0.08,
        ylim = c(0, 10000),
        cex.names = 1.2,
        cex.axis = 1.2,
        border = "gray0",
        col = "darkolivegreen3")

#Make venndiagram for overlap of same genes identified as SPE between studies

#1. Add columns for each publication that a gene is found in

merged_data_set_simple$in_pub1 <- merged_data_set_simple$geneID %in% data_set_101$geneID #adds new column to dataset called in_pub1 that contains TRUE if that geneid is found in dataset 1 and false if it is not
merged_data_set_simple <- merged_data_set_simple %>% 
  mutate(in_pub1 = str_replace(in_pub1, "TRUE", "pub1")) #changes TRUE/FALSE binary in column in_pub one and replaces TRUE with pub1, false still remains false

merged_data_set_simple$in_pub2 <- merged_data_set_simple$geneID %in% data_set_2_cast$geneID
merged_data_set_simple <- merged_data_set_simple %>% 
  mutate(in_pub2 = str_replace(in_pub2, "TRUE", "pub2"))

merged_data_set_simple$in_pub3 <- merged_data_set_simple$geneID %in% v4data_set_3$geneID
merged_data_set_simple <- merged_data_set_simple %>% 
  mutate(in_pub3 = str_replace(in_pub3, "TRUE", "pub3"))

#2. Making final column in_pubs with strings combined
merged_data_set_simple <- mutate(merged_data_set_simple,
                                 in_pubs=str_c(in_pub1, in_pub2, in_pub3, sep = ":")) #creating new column in_pubs that contains a string of the values from the in_pub columns

view(merged_data_set_simple)
all_unique_genes <- unique(merged_data_set_simple$geneID)#total unique genes SPE and nonSPE
str(all_unique_genes) #pulls 36597 genes
unique(merged_data_set_simple$in_pubs) #shows 7 unique values "FALSE:pub2:pub3"  "pub1:pub2:pub3" "FALSE:pub2:FALSE" "pub1:pub2:FALSE"  "pub1:FALSE:pub3"  "pub1:FALSE:FALSE""FALSE:FALSE:pub3"

#3. Make unique list of genes that are only IDed as SPE (1 in )

all_SPE_genes <- merged_data_set_simple[merged_data_set_simple$SPE_YN == '1', ] #pulling out all rows that have a 1 value in SPE_YN column
unique_SPE_genes <- unique(all_SPE_genes$geneID)
view(unique_SPE_genes)
str(unique_SPE_genes) #pulled a total of 13871 unique genes

#4. pull out each unique string for pubs into individual dataframes and then just run unique to see how many unique SPE Dgene IDs there are?
pub1 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "pub1:FALSE:FALSE", ] #this is pulling out all rows with gene that only shows up in pub 1
pub1 <- pub1[pub1$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub1unique <- unique(pub1$geneID) # = 655 unique genes, this is pulling out all unique gene values in pub 1 
#view(pub1unique)

pub2 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "FALSE:pub2:FALSE", ] #this is pulling out all rows with gene that only shows up in pub 1
pub2 <- pub2[pub2$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub2unique <- unique(pub2$geneID) # = 1183 unique genes

pub3 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "FALSE:FALSE:pub3", ] #this is pulling out all rows with gene that only shows up in pub 1
pub3 <- pub3[pub3$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub3unique <- unique(pub3$geneID) 

pub1_pub2 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "pub1:pub2:FALSE", ] #this is pulling out all rows with gene that only shows up in pub 1
pub1_pub2 <- pub1_pub2[pub1_pub2$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub1_pub2unique <- unique(pub1_pub2$geneID)  # = 1627 SPEgenes only in pub 1 and 2

pub2_pub3 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "FALSE:pub2:pub3", ] #this is pulling out all rows with gene that only shows up in pub 1
pub2_pub3 <- pub2_pub3[pub2_pub3$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub2_pub3unique <- unique(pub2_pub3$geneID) # = 2580 genes in pub 2 and 3

pub1_pub3 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "pub1:FALSE:pub3", ] #this is pulling out all rows with gene that only shows up in pub 1
pub1_pub3 <- pub1_pub3[pub1_pub3$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub1_pub3unique <- unique(pub1_pub3$geneID) # = 1791 genes across pub 1 and 3

pub1_pub2_pub3 <- merged_data_set_simple[merged_data_set_simple$in_pubs == "pub1:pub2:pub3", ] #this is pulling out all rows with gene that only shows up in pub 1
pub1_pub2_pub3 <- pub1_pub2_pub3[pub1_pub3$SPE_YN == '1', ] #this is pulling out all rows that have an SPE gene, signified by a 1 in SPE YN column
pub1_pub2_pub3unique <- unique(pub1_pub2_pub3$geneID) = # 4172 genes in all 3 publications
  str(pub1_pub2_pub3unique)

# ~~~~~~~~~~~~~~~~~~~VCAP SET UP!!!!!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1. Make 2 separate df for genes that have been ID as SPE (1 in SPE_YN) and genes that haven't (0 in SPE_YN)
#a. All SPE genes
all_SPE_genes <- merged_data_set_simple[merged_data_set_simple$SPE_YN == '1', ] #pulling out all rows that have a 1 value in SPE_YN column
unique_SPE_genes <- unique(all_SPE_genes$geneID) %>% as.data.frame() #finding unique gene values and saving as DF
colnames(unique_SPE_genes)[1]<- "v4geneID" #changing column names
view(unique_SPE_genes)

#b. All non-SPE genes
all_nonSPE_genes <- merged_data_set_simple[merged_data_set_simple$SPE_YN == '0', ]
unique_nonSPE_genes <- unique(all_nonSPE_genes$geneID) %>% as.data.frame()
colnames(unique_nonSPE_genes)[1]<- "v4geneID"
view(unique_nonSPE_genes)

#2. Save unique_SPE_genes and unique_nonSPE_genes as tsv. files
write_tsv(unique_SPE_genes, "unique_SPE_genes.tsv") #saves in lab work folder
write_tsv(unique_nonSPE_genes, "unique_nonSPE_genes.tsv")

#3. Save v4/v5 conversions as tsv. file

v4_v5 <- read.delim("B73v4_B73v5_header.txt")
write_tsv(v4_v5, "b73v4_v5_conversions.tsv")

head(v4_v5)
view(v4_v5)
unique_v4IDs <- unique(v4_v5$v4geneID) %>% as.data.frame()
str(unique_v4IDs) #a total of 38,153 unique v4 genes in the doc compared to 40,962 entries

#to find how many shared v4 ids between unique SPE genes and v4/v5 conversion file
unique_SPE_genes <- read_tsv("unique_SPE_genes.tsv") %>% as.data.frame()
view(unique_SPE_genes)
view(unique_v4IDs)
colnames(unique_v4IDs)[1] = "v4geneID"
view(unique_v4IDs)
shared_values <- inner_join(unique_SPE_genes, 
                            unique_v4IDs, 
                            by = NULL)
view(shared_values)

#to find how many shared v4 ids between unique nonSPE genes and v4/v5 conversion file
unique_nonSPE_genes <- read_tsv("unique_nonSPE_genes.tsv") %>% as.data.frame()
view(unique_nonSPE_genes)
shared_valuesNON <- inner_join(unique_nonSPE_genes, 
                               unique_v4IDs, 
                               by = NULL)
view(shared_valuesNON)

#To make a cleaned tsv file for v4/v5 conversions by removing all rows with repeated genes from v4
v4_v5conversions <- read_tsv("b73v4_v5_conversions.tsv") %>% as.data.frame()# makes master df
str(v4_v5conversions)#shows saved as data frame
view(v4_v5conversions)
#1. First creating a clean df of the just the v4geneID column by removing all values that are duplicated
v4geneIDs <- v4_v5conversions$v4geneID %>% as.data.frame() #pulls just v4 geneID column from master conversion df
colnames(v4geneIDs)[1] = "v4geneID" #renames column to we can perform inner join later
view(v4geneIDs) #confirms
v4geneID_clean <- v4geneIDs[!(duplicated(v4geneIDs) | duplicated(v4geneIDs, fromLast = TRUE)), ] %>% as.data.frame() #removes all rows that are duplicated
colnames(v4geneID_clean)[1] = "v4geneID" #changes column name so inner join can be performed
view(v4geneID_clean)#shows 36186 rows, compared to the total unique values from original of 38153, means completely removed duplicates

#2 Inner join clean v4 gene ID list with original data frame so all rows are carried along
cleanv4_v5conversions <- inner_join(v4geneID_clean, v4_v5conversions, by = c("v4geneID")) #inner join clean column with total dataframe to pull out all the rows
view(cleanv4_v5conversions) #shows 36,186 which matches original clean list

#Now to match SPE gene IDs to the clean v4/v5 conversion df and save as TSV
unique_SPE_genes <- read_tsv("unique_SPE_genes.tsv") %>% as.data.frame() #df of list of v4 SPE genes
view(unique_SPE_genes) #proof, shows 13,871 rows
SPE_v4V5_clean <- inner_join(unique_SPE_genes, cleanv4_v5conversions, by = c("v4geneID")) %>% as.data.frame() #inner join with clean conversion df
view(SPE_v4V5_clean)#worked and shows 11,909 rows
write_tsv(SPE_v4V5_clean, "SPE_v4v5_clean.tsv")

#To match nonSPE gene IDs to the vlean v4/v5 conversion df and save as TSV
unique_nonSPE_genes <- read_tsv("unique_nonSPE_genes.tsv") %>% as.data.frame() #list of all nonSPE v4 genes
view(unique_nonSPE_genes) #proof, shows 29,464 rows
nonSPE_v4v5_clean <- inner_join(unique_nonSPE_genes, cleanv4_v5conversions, by = c("v4geneID")) %>% as.data.frame()#inner join with clean conversion df
view(nonSPE_v4v5_clean) #worked shows 25, 712 rows
write_tsv(nonSPE_v4v5_clean, "nonSPE_v4v5_clean.tsv")

# From quality checks
clean_SPE_genes <- read_tsv("SPE_v4v5_clean.tsv") %>% as.data.frame() 
view(clean_SPE_genes) #11,909 v5 spe gene entries
clean_nonSPE_genes <- read_tsv("nonSPE_v4v5_clean.tsv") %>% as.data.frame()
view(clean_nonSPE_genes) #25,716 v5 nonspe gene entries

#To remove 1:0 V4:V5 gene conversions
#1. Pull out all rows that have a "-1" value in the v5start column
clean_SPE_genes <- subset(clean_SPE_genes, v5start!="-1") #pulling out all rows that have a -1 in v5 start - indicates 1:0 v4:v5
view(clean_SPE_genes) #shows 9,630 genes compared to 13,871 spe genes in the original v4 dataset

clean_nonSPE_genes <- subset(clean_nonSPE_genes, v5start!="-1")
view(clean_nonSPE_genes) #shows 23,716 genes compared to 29,464 in the original v4 dataset


#making BED files for SPE genes and NonSPE genes "chrom", "chromStart", "chromEnd", "geneID", "strand"

#first remove chr from chromosomes
clean_SPE_genes$v5chromosome <- str_replace(clean_SPE_genes$v5chromosome, "chr", "")
clean_nonSPE_genes$v5chromosome <- str_replace(clean_nonSPE_genes$v5chromosome, "chr", "")
view(clean_nonSPE_genes)

bedSPE <- clean_SPE_genes[,c('v5chromosome', 'v5start', 'v5stop', 'v5geneID', 'v5..', 'v5length')] #pulling out columns I want for BED file format
names(bedSPE)<-NULL #removes column names?
view(bedSPE) #looks good
str(bedSPE)
write.table(bedSPE, "SPE.genes.bed") #saves as BED file in my R labwork directory on pc
df <- read_bed

bedNONSPE <- clean_nonSPE_genes[,c('v5chromosome', 'v5start', 'v5stop', 'v5geneID', 'v5..', 'v5length')]
names(bedNONSPE)<-NULL
print(bedNONSPE)
view(bedNONSPE)#looks good
write.table(bedNONSPE, "NONSPE.genes.bed")

write_tsv(bedNONSPE, "NONSPE.genes5.tsv")
write.table(bedNONSPE, "NONSPE.genes.bed")

nonSPE <- read_tsv("NONSPE.genes5.tsv")
view(nonSPE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Compling TPM background averages~~~~~~~~~~~~~~~~~~~~~~~~
#1. Make B73 reference with all 1:0 1:many pangene : B73v5ID so we can inner join all by pangene
B73 <- read_tsv("B73.tsv.txt") %>% as.data.frame() # 47,735 rows
B73 <- B73[!grepl('chr', B73$Geneid),] 
view(B73)#33,599, removes all pan genes that don't match with a v5 gene id - includes chr in name
#2. Load in all 25 other files for TPM counts
B97 <- read_tsv("B97_full-tpm.tsv.txt") %>% as.data.frame()
CML103 <- read_tsv("CML103.tsv.txt") %>% as.data.frame()
CML228 <- read_tsv("CML228.tsv.txt") %>% as.data.frame()
CML247 <- read_tsv("CML247.tsv.txt") %>% as.data.frame()
CML277 <- read_tsv("CML277.tsv.txt") %>% as.data.frame()
CML322 <- read_tsv("CML322.tsv.txt") %>% as.data.frame()
CML333 <- read_tsv("CML333.tsv.txt") %>% as.data.frame()
CML52 <- read_tsv("CML52.tsv.txt") %>% as.data.frame()
CML69 <- read_tsv("CML69.tsv.txt") %>% as.data.frame()
HP301 <- read_tsv("HP301.tsv.txt") %>% as.data.frame()
IL14H <- read_tsv("IL14H.tsv.txt") %>% as.data.frame()
Ki11 <- read_tsv("Ki11.tsv.txt") %>% as.data.frame()
Ki3 <- read_tsv("Ki3.tsv.txt") %>% as.data.frame()
Ky21 <- read_tsv("Ky21.tsv.txt") %>% as.data.frame()
M162W <- read_tsv("M162W.tsv.txt") %>% as.data.frame()
M37W <- read_tsv("M37W.tsv.txt") %>% as.data.frame()
Mo18W <- read_tsv("Mo18W.tsv.txt") %>% as.data.frame()
MS71 <- read_tsv("MS71.tsv.txt") %>% as.data.frame()
NC350 <- read_tsv("NC350.tsv.txt") %>% as.data.frame()
NC358 <- read_tsv("NC358.tsv.txt") %>% as.data.frame()
Oh43 <- read_tsv("Oh43.tsv.txt") %>% as.data.frame()
Oh7b <- read_tsv("Oh7b.tsv.txt") %>% as.data.frame()
P39 <- read_tsv("P39.tsv.txt") %>% as.data.frame()
Tx303 <- read_tsv("Tx303.tsv.txt") %>% as.data.frame()
Tzi8 <- read_tsv("Tzi8.tsv.txt") %>% as.data.frame()

#3. Set up function to remove rows other than pangeneid and TPM data (geneid, chr, start, entd, strand, length)
format_TPM <- function(arg1) { #this makes the function. arg1 is a variable that stays consistant and doesn't matter what it's called
  new <- select(arg1, -c(Geneid, Chr, Start, End, Strand, Length)) #new is a dataframe that is made by removing Geneid, chr, start, end, strand, lenth
  return(new)
}
view(Tzi8)
#3.1 now to do this for all 25...
B97 <- format_TPM(B97) #works!
CML103 <- format_TPM(CML103)
CML228 <- format_TPM(CML228)
CML247 <- format_TPM(CML247)
CML277 <- format_TPM(CML277)
CML322 <- format_TPM(CML322)
CML333 <- format_TPM(CML333)
CML52 <- format_TPM(CML52)
CML69 <- format_TPM(CML69)
HP301 <- format_TPM(HP301)
IL14H <- format_TPM(IL14H)
Ki11 <- format_TPM(Ki11)
Ki3 <- format_TPM(Ki3)
Ky21 <- format_TPM(Ky21)
M162W <- format_TPM(M162W)
M37W <- format_TPM(M37W)
Mo18W <- format_TPM(Mo18W)
MS71 <- format_TPM(MS71)
NC350 <- format_TPM(NC350)
NC358 <- format_TPM(NC358)
Oh43 <- format_TPM(Oh43)
Oh7b <- format_TPM(Oh7b)
P39 <- format_TPM(P39)
Tx303 <- format_TPM(Tx303)
Tzi8 <- format_TPM(Tzi8)
view(Tzi8)

#4. innerjoin all onto B73 by PanGeneID using tidyvrse reduce
TPM_lists = list(B73, B97, CML103, CML228, CML247, CML277, CML322, CML333, CML52, CML69, HP301, IL14H, Ki11, Ki3, Ky21, M162W, M37W, Mo18W, MS71, NC350, NC358, Oh43, Oh7b, P39, Tx303, Tzi8) #writes all dataframes as list
str(TPM_lists)
all_TPMs <- TPM_lists %>% reduce(left_join, by='PanGeneID') %>% .[!grepl('chr', .$Geneid),] #more efficient, uses left_join because all pan-genes aren't present in B73 by PanGeneID column
str(all_TPMs)
#view(all_TPMs) #there are NA values because some pangenes weren't present in all of the genotypes
write_tsv(all_TPMs, "all_TPMs.tsv")

all_TPMs <- read_tsv("all_TPMs.tsv") %>% as.data.frame()
head(all_TPMs)

#5. Take averages of all tissues:
# test B73$embryo_avg <- rowMeans(B73 %>% select(contains("embryo")), na.rm=TRUE) 
# view(B73) #worked
all_TPMs$embryo_avg <- rowMeans(all_TPMs %>% select(contains("embryo")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$endosperm_avg <- rowMeans(all_TPMs %>% select(contains("endosperm")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$root_avg <- rowMeans(all_TPMs %>% select(contains("root")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$shoot_avg <- rowMeans(all_TPMs %>% select(contains("shoot")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$anther_avg <- rowMeans(all_TPMs %>% select(contains("anther")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$leafbase_avg <- rowMeans(all_TPMs %>% select(contains("base")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$leafmiddle_avg <- rowMeans(all_TPMs %>% select(contains("middle")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$leaftip_avg <- rowMeans(all_TPMs %>% select(contains("tip")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$ear_avg <- rowMeans(all_TPMs %>% select(contains("ear")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'
all_TPMs$tassel_avg <- rowMeans(all_TPMs %>% select(contains("tassel")), na.rm=TRUE) #add new col for embryo average for all columns that contain string 'embryo'

all_TPMs$total_avg <- rowMeans(all_TPMs[ ,(8:509)], na.rm=TRUE) #average across all tissues from all samples
colnames(all_TPMs) #shows up
view(all_TPMs$total_avg) #values are there, everything is in scientific notation
head(all_TPMs) #values print out normal

#6. Save in bedfile format
colnames(all_TPMs)
bedTPM_avgs <- all_TPMs[,c('Chr', 'Start', 'End', 'Geneid', 'Strand', 'Length', 'embryo_avg', 'endosperm_avg', 'root_avg', 'shoot_avg', 'anther_avg', 'leafbase_avg', 'leafmiddle_avg', 'leaftip_avg', 'ear_avg', 'tassel_avg', 'total_avg')] #pulling out columns I want for BED file format
head(bedTPM_avgs) #shows just with ID info and tissue avgs
#6.1 first remove chr from chromosomes
bedTPM_avgs$Chr <- str_replace(bedTPM_avgs$Chr, "chr", "")
#6.2 write as BED and TSV
write.table(bedTPM_avgs, "TPM_avgs.bed")
write_tsv(bedTPM_avgs, "TPM_avgs.tsv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Distributions for length and expression levels in SPE and NONSPE in both our SPE/NON bed files and the TPM data
#1. Create distribution for all TPM frequency (all tissue average) and all length
TPM_avgs <- read_tsv("TPM_avgs.tsv") %>% as.data.frame()
view(TPM_avgs)
head(TPM_avgs$total_avg)
hist(TPM_avgs$total_avg, breaks = 10000, xlim = c(0,150), main = "Histogram for TPM tissue averages all genes", 
     xlab = "TPM Reads Average all Tissues", ylab = "# of genes in bin") #makes histogram for TPM expression
axis(side=1, at=seq(0,10000, 100))

view(TPM_avgs$Length)
hist(TPM_avgs$Length, breaks = 800, xlim = c(0,40000), main = "Histogram for gene length (TPM data)", 
     xlab = "Length", ylab = "# of genes in bin") #makes histogram for Length.
axis(side=1, at=seq(0,40000, 1000))


#2. Create distribution for SPE genes TPM frequency (all tissue average)
unique_SPE <- read_tsv("SPE.genes5.tsv") %>% as.data.frame()
v5_SPE <- select(unique_SPE, v5geneID, v5length) #pull out v5ID and length from conversion
view(v5_SPE) #9630 
str(v5_SPE)
TPM_avgs$Geneid <- substr(TPM_avgs$Geneid, start = 1, stop = 15) #turns to conventional v5 gene id by removing the last 5 characters
view(TPM_avgs)
colnames(v5_SPE)[1] <- ("Geneid") #rename v5 gene ID column for ease of inner join
unique_SPE_TPM <- inner_join(v5_SPE, TPM_avgs, by = "Geneid")
str(unique_SPE_TPM) #8237
view(unique_SPE_TPM)

hist(unique_SPE_TPM$total_avg, breaks = 10000, xlim = c(0,150), main = "Histogram for TPM tissue averages SPE genes", 
     xlab = "TPM Reads Average all Tissues", ylab = "# of genes in bin") #makes histogram for TPM expression SPE

#3. Create distribution for nonSPE genes TPM frequency (all tissue average)

unique_nonSPE <- read_tsv("NONSPE.genes5.tsv")
v5_nonSPE <- select(unique_nonSPE, v5geneID, v5length) #pull out v5ID and length from conversion
view(v5_nonSPE) # 23,716
colnames(v5_nonSPE)[1] <- ("Geneid")
unique_nonSPE_TPM <- inner_join(v5_nonSPE, TPM_avgs, by = "Geneid")
view(unique_nonSPE_TPM) #21,529 

hist(unique_nonSPE_TPM$total_avg, breaks = 10000, xlim = c(0,150), main = "Histogram for TPM tissue averages nonSPE genes", 
     xlab = "TPM Reads Average all Tissues", ylab = "# of genes in bin") #makes histogram for TPM expression nonSPE

write_tsv(unique_nonSPE_TPM, "unique_nonSPE_TPM.tsv") #contains v5 geneid, bed file length, tpm length
write_tsv(unique_SPE_TPM, "unique_SPE_TPM.tsv")

view(unique_SPE_TPM)
#4. Create distribution for SPE gene length from previous BED files (v5length column)
unique_SPE_TPM <- read_tsv("unique_SPE_TPM.tsv")
view(unique_SPE_TPM)

hist(unique_SPE_TPM$v5length, breaks = 10000, xlim = c(0,60000), main = "Histogram for SPE gene length from v5 conversions", 
     xlab = "Gene Length", ylab = "# of genes in bin") 


#5. Create distribution for SPE gene length from TPM files (Length column)
hist(unique_SPE_TPM$Length, breaks = 10000, xlim = c(0,60000), main = "Histogram for SPE gene lengths from TPM data", 
     xlab = "Gene Length", ylab = "# of genes in bin")

#5a - plot standard dev? actually just the difference
unique_SPE_TPM$Length_deviation <- unique_SPE_TPM$v5length - unique_SPE_TPM$Length #creates column with difference between v5 lengths and TPM
unique_SPE_TPM$Length_deviation <- abs(unique_SPE_TPM$Length_deviation) #absolute value, all were - beofre indicating larger TPM size
view(unique_SPE_TPM)
hist(unique_SPE_TPM$Length_deviation, breaks = 10000, xlim = c(0,10000), main = "Histogram for absolute value of SPE gene lengths deviation between v5 and TPM - origionally most were - indicating TPM gene legnths are on average larger", 
     xlab = "Difference (v5 - TPM)", ylab = "# of genes in bin")

#6. Create distribution for nonSPE gene length from previous BED files (v5length)
unique_nonSPE_TPM <- read_tsv("unique_nonSPE_TPM.tsv") 
view(unique_nonSPE_TPM)

hist(unique_nonSPE_TPM$v5length, breaks = 10000, xlim = c(0,60000), main = "Histogram for nonSPE gene length from v5 conversions", 
     xlab = "Gene Length", ylab = "# of genes in bin") 

#7. Create distribution for nonSPE gene length from TPM files (Length column)

hist(unique_nonSPE_TPM$Length, breaks = 10000, xlim = c(0,60000), main = "Histogram for nonSPE gene lengths from TPM data", 
     xlab = "Gene Length", ylab = "# of genes in bin")

#7a - plot standard dev? actually just the difference
unique_nonSPE_TPM$Length_deviation <- unique_nonSPE_TPM$v5length - unique_nonSPE_TPM$Length #creates column with difference between v5 lengths and TPM
unique_nonSPE_TPM$Length_deviation <- abs(unique_nonSPE_TPM$Length_deviation) #absolute value, all were - beofre indicating larger TPM size
view(unique_nonSPE_TPM)

hist(unique_nonSPE_TPM$Length_deviation, breaks = 10000, xlim = c(0,10000), main = "Histogram for absolute value of nonSPE gene lengths deviation between v5 and TPM - origionally most were - indicating TPM gene legnths are on average larger", 
     xlab = "Difference (v5 - TPM)", ylab = "# of genes in bin")

#~~~~~~~~~~~~~~~~Making Files in BED format for Bins with Length and TPM data

#1. SPE.TPM - LEFT JOINED BED FORMAT
TPM_avgs <- read_tsv("TPM_avgs.tsv") %>% as.data.frame()
TPM_avgs$Geneid <- substr(TPM_avgs$Geneid, start = 1, stop = 15) #remove T000etc from end of v5 gene ID
view(TPM_avgs)
TPM_avgs <- select(TPM_avgs, Geneid, total_avg) #selecting columns for id and avg tpm reads
unique_SPE <- read_tsv("SPE.genes5.tsv")
view(unique_SPE)
colnames(unique_SPE)[4] <- ("Geneid") #change v5 gene ID column name for left join
SPE.TPM <- left_join(unique_SPE, TPM_avgs, by = "Geneid")
view(SPE.TPM)
write_tsv(SPE.TPM, "SPE.TPM.tsv")

#2. nonSPE.TPM - LEFT JOINED IN BED FORMAT
TPM_avgs <- read_tsv("TPM_avgs.tsv") %>% as.data.frame()
TPM_avgs$Geneid <- substr(TPM_avgs$Geneid, start = 1, stop = 15) #remove T000etc from end of v5 gene ID
view(TPM_avgs)
TPM_avgs <- select(TPM_avgs, Geneid, total_avg)
unique_nonSPE <- read_tsv("NONSPE.genes5.tsv")
view(unique_nonSPE)
colnames(unique_nonSPE)[4] <- ("Geneid") #change v5 gene ID column name for left join
nonSPE.TPM <- left_join(unique_nonSPE, TPM_avgs, by = "Geneid")
colnames(nonSPE.TPM)
write_tsv(nonSPE.TPM, "nonSPE.TPM.tsv")

#Running sams script for bins
#1. Step 1: Bin by 0.1


SPE.TPM <- read_tsv("SPE.TPM.tsv")
#view(SPE.TPM) #9,630
nonSPE.TPM <- read_tsv("nonSPE.TPM.tsv")
#str(nonSPE.TPM) #23,716

#bin_by_L.10 <- function(df){
# df<-df %>% mutate(Lbin = case_when(v5length <= 2500 ~ "L.0-2500", #V4 stands for the column name of variable 1, in this case v5length
#                                    v5length > 2500 & v5length <= 5000 ~ "L.2500-5000",
#                                   v5length > 5000 & v5length <= 10000 ~ "L.5000-10000",
#                                v5length > 10000 ~ "L.10000-max"),
#                     TPMbin = case_when(total_avg <= 0.5 ~ "TPM.0-5", #V5 stands for column name of variable 2, in this case TPM reads
#                              total_avg > 0.5 & total_avg <= 2.5 ~ "TPM.5-2.5",
#                             total_avg > 2.5 & total_avg <= 5 ~ "TPM.2.5-5",
#                            total_avg > 5 & total_avg <= 15 ~ "TPM.5-15",
#                           total_avg > 15 & total_avg <= 25 ~ "TPM.15-25",
#                          total_avg > 25 ~ "TPM.25-max"))
#return(df)
#}

bin_by_L.10 <- function(df){
  df<-df %>% mutate(Lbin = case_when(v5length <= 900 ~ "L.0-900", #V4 stands for the column name of variable 1, in this case v5length
                                     v5length > 900 & v5length <= 1500 ~ "L.900-1500",
                                     v5length > 1500 & v5length <= 3000 ~ "L.1500-3000",
                                     v5length > 3000 ~ "L.3000-max"),
                    TPMbin = case_when(total_avg <= 0.5 ~ "TPM.0-5", #V5 stands for column name of variable 2, in this case TPM reads
                                       total_avg > 0.5 & total_avg <= 2.5 ~ "TPM.5-2.5",
                                       total_avg > 2.5 & total_avg <= 5 ~ "TPM.2.5-5",
                                       total_avg > 5 & total_avg <= 15 ~ "TPM.5-15",
                                       total_avg > 15 & total_avg <= 25 ~ "TPM.15-25",
                                       total_avg > 25 ~ "TPM.25-max",
                                       is.na(total_avg)==TRUE ~ "TPM.NA")) #adds bin for NA values by producing TF for values
  return(df)
}

nonSPE.distrib.TPM <- tibble(V1=NA, V2=NA, V3=NA,V4=NA,V5=NA,V6=NA,V7=NA,Lbin=NA,TPMbin=NA) ### MAKE SURE TO CLEAR TIBBLE


SPE.TPM <- bin_by_L.10(SPE.TPM) 
nonSPE.TPM <- bin_by_L.10(nonSPE.TPM)
view(SPE.TPM)
view(nonSPE.TPM)

SPE.distrib.TPM<-sample_n(SPE.TPM, 1000) #10%ish of the total SPE genes we are using

SPE_L_splitlist <- split(SPE.distrib.TPM, SPE.distrib.TPM$Lbin) # 10% tibble split by length bin variable
nonSPE_L_splitlist <- split(nonSPE.TPM, nonSPE.TPM$Lbin)

for(L in names(SPE_L_splitlist)){ #goes through each L bin
  s<-paste("^",L,"$",sep="") #gets name to exact match af bin
  t<-grep(s,names(SPE_L_splitlist)) #finds exact match for AF in MOA dataframe list
  b<-grep(s,names(nonSPE_L_splitlist))  #finds the exact match for AF in background dataframe list
  for(TPM in c("TPM.0-5", "TPM.5-2.5","TPM.2.5-5", "TPM.5-15", "TPM.15-25", "TPM.25-max", "TPM.NA")){ #for each possible TPM bin
    n<-filter(SPE_L_splitlist[[t]], TPMbin == TPM) %>% nrow() #number of MOA snps that need a matched background snp
    pool<-filter(nonSPE_L_splitlist[[b]], TPMbin == TPM) #find all the background snps that match that AF and distance bin
    if(nrow(pool) != 0 & n != 0){ #as long as there's 1 background SNP and MOA snp in that set of bins
      nonSPE<-sample_n(pool, n, replace = FALSE) #sample the number of MOA snps in the bin from the background pool
      nonSPE.distrib.TPM <- add_row(nonSPE.distrib.TPM, V1=nonSPE$v5chromosome,V2=nonSPE$v5start,V3=nonSPE$v5stop,V4=nonSPE$Geneid,V5=nonSPE$v5.., V6=nonSPE$v5length, V7=nonSPE$total_avg, Lbin=nonSPE$Lbin,TPMbin=nonSPE$TPMbin)  #write all that info to the background distribution table
    }
  }
}
View(nonSPE.distrib.TPM)#check for 1000 rows
view(SPE.distrib.TPM) #check for 1000 rows

nonSPE.distrib.TPM<-nonSPE.distrib.TPM[-1, ] #delete first row of NA values

write.table(SPE.distrib.TPM, file=paste0("SPE.distrib1000.TPM.bed"), sep="\t", row.names=FALSE, quote=FALSE)
write.table(nonSPE.distrib.TPM, file=paste0("nonSPE.distrib1000.TPM.bed"), sep="\t", row.names=FALSE, quote=FALSE)



#n<-filter(SPE_L_splitlist[[t]], TPMbin == TPM) %>% nrow() #number of MOA snps that need a matched background snp
view(n)

#pool<-filter(nonSPE_L_splitlist[[b]], TPMbin == TPM) #find all the background snps that match that AF and distance bin
view(pool)



#~~~~~~~~~~~~~~~~~~~~~~VCAP Figures~~~~~~~~~~~~~~~~~~~~~~~~~~~
H_perm0<-read_tsv("TPMbyL_allvall.h_estimates.txt") %>% as.data.frame()
view(H_perm0)
str(H_perm0)
names(H_perm0)[13]<-"Trait"

names(H_perm0)<- names(H_perm0) %>% str_replace_all(.,"_1","_SD")
names(H_perm0)[1]<-"Component" 
names(H_perm0)[2]<-"Her_K1" 
names(H_perm0)[3]<-"Her_K2" 
names(H_perm0)[4]<-"Her_K3" 
names(H_perm0)[5]<-"Her_Top" 
names(H_perm0)[6]<-"Her_ALL" 
names(H_perm0)[7]<-"Component" 
names(H_perm0)[8]<-"Her_K1SD" 
names(H_perm0)[9]<-"Her_K2SD" 
names(H_perm0)[10]<-"Her_K3SD" 
names(H_perm0)[11]<-"Her_TopSD" 
names(H_perm0)[12]<-"Her_ALLSD" 
H_perm0<-H_perm0 %>% select(-contains("Component"))

H_perm0<-mutate(H_perm0, 
                Va_SPE = Her_K1 / Her_ALL,
                Va_nonSPE = Her_K2 / Her_ALL,
                Va_Rest = Her_K3 / Her_ALL)

view(H_perm0)
##Calculate Va and plot

H_perm0 %>% select(c(contains("Va"),Trait)) %>% melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component") %>%
  ggplot(aes(x=Trait)) +
  geom_boxplot(aes(y=percent_Va, color=K_Component))+
  coord_flip()+
  theme(axis.text.y = element_text(size = 7))+
  xlab("Trait")+ylab("Va %")+
  scale_color_brewer(palette = "Set2")

melted_HpermALL<-H_perm0 %>% select(c(contains("Va"),Trait)) %>% melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component")
view(melted_HpermALL)

#Zeavolution mifugre

melted_stats_Hperm0<-H_perm0 %>% select(c(contains("Va"),Trait)) %>% melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component") %>% group_by(Trait,K_Component) %>% summarize(Mean = mean(percent_Va), Median = median(percent_Va), std_dev=sd(percent_Va)) 
view(melted_stats_Hperm0)

H_perm0 %>%
  ggplot(aes(x=reorder(Trait,Va_SPE)))+
  geom_point(aes(y=Va_Rest, color="3_Rest"), alpha = 0.3, size = 1)+
  geom_point(aes(y=Va_nonSPE, color="2_Background"), alpha = 0.3,size = 1)+
  geom_point(aes(y=Va_SPE, color="1_MOA"), alpha = 0.3,size = 1)+
  geom_point(data = melted_stats_Hperm0, aes(x=Trait, y=Mean, shape = K_Component)) + 
  scale_shape_manual(values=c(16, 6, 4))+
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_blank())+ #, color = H_perm_ALL$trait_colors
  xlab("")+ylab("Va %")+scale_color_brewer(palette = "Set2")+ggtitle(str_c("Estimated Va of All Traits with with 1 Permutation, ALLvALL"))+
  theme(legend.title = element_blank())



ggplot(H_perm0, aes(y=reorder(Trait,Va_SPE))) + 
  stat_density_ridges(aes(x=Va_Rest,fill="3_Rest"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.01)+
  stat_density_ridges(aes(x=Va_nonSPE,fill="2_nonSPE"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.01)+
  stat_density_ridges(aes(x=Va_SPE,fill="1_SPE"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.01)+
  ylab("Trait")+ xlab("Va %") +
  scale_fill_brewer(palette = "Set2")+
  geom_point(data = melted_stats_HpermALL, aes(y=Trait, x=Mean, shape = K_Component)) + 
  scale_shape_manual(values=c(16, 6, 4))+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())+
  ggtitle(str_c("Estimated Va of All Traits with 1 Permutation, ALLvALL"))+
  theme(legend.title = element_blank())

brewer.pal(n = 8, name = "Set2")

par(mfrow=c(1, 1))
display.brewer.all()
mycols4 <- c("#C3D938", "#772877", "#7C821E", "#D8B98B", "#7A4012")
view(mycols4)

##Group traits by category
###Let's group phenotypes by "category"

tassel_architecture<-c("tassel_length_BLUP;Brown_2011","spike_length_BLUP;Brown_2011","branch_zone_BLUP;Brown_2011","branch_number_transformed_BLUP;Brown_2011","tassprimbranchno_BLUP;Hung_2012_1","tasslength_BLUP;Hung_2012_1")
ear_architecture<-c("cob_length_BLUP;Brown_2011","cob_diameter_BLUP;Brown_2011","ear_row_number_transformed_BLUP;Brown_2011","cobdiam_BLUP;Hung_2012_1","coblength_BLUP;Hung_2012_1","earrowno_BLUP;Hung_2012_1","kernelnoperrow_BLUP;Hung_2012_1","earmass_BLUP;Hung_2012_1","cobmass_BLUP;Hung_2012_1","totalkernelweight_BLUP;Hung_2012_1","weight20kernels_BLUP;Hung_2012_1","totalkernelno_BLUP;Hung_2012_1","starch_kernel_BLUP;Cook_2012","protein_kernel_BLUP;Cook_2012","oil_kernel_BLUP;Cook_2012")
stalk_strength<-c("WI_rind_penetrometer_resistance_BLUE;Peiffer_2013","WI_rind_penetrometer_resistance_BLUP;Peiffer_2013","ALL_rind_penetrometer_resistance_BLUE;Peiffer_2013","ALL_rind_penetrometer_resistance_BLUP;Peiffer_2013","MO_rind_penetrometer_resistance_BLUE;Peiffer_2013","MO_rind_penetrometer_resistance_BLUP;Peiffer_2013","NY_rind_penetrometer_resistance_BLUE;Peiffer_2013","NY_rind_penetrometer_resistance_BLUP;Peiffer_2013")
disease<-c("southern_leaf_blight_BLUP;Bian_2014_Kump_2011","lesion_severity_BLUP;Olukolu_2014","sqrt_diseased_leaf_area1_BLUP;Poland_2011","sqrt_diseased_leaf_area2_BLUP;Poland_2011","sqrt_diseased_leaf_area3_BLUP;Poland_2011","diseased_leaf_area1_BLUP;Poland_2011","diseased_leaf_area2_BLUP;Poland_2011","diseased_leaf_area3_BLUP;Poland_2011","northern_leaf_blight_index_BLUP;Poland_2011")
plant_architecture<-c("height_ratio_BLUP;Olukolu_2014","stalk_width_ratio_BLUP;Olukolu_2014","leaf_length_BLUP;Tian_2011","leaf_width_BLUP;Tian_2011","upper_leaf_angle_BLUP;Tian_2011","last_leaf_with_epicuticular_wax;Foerster_2015","ALL_ear_height_BLUE;Peiffer_2013","ALL_ear_height_BLUP;Peiffer_2013","plantheight_BLUP;Hung_2012_1","earheight_BLUP;Hung_2012_1","leaflength_BLUP;Hung_2012_1","leafwidth_BLUP;Hung_2012_1","upperleafangle_BLUP;Hung_2012_1","nodenumberbelowear_BLUP;Hung_2012_1","nodenumberaboveear_BLUP;Hung_2012_1","numbraceroots_BLUP;Hung_2012_1","plant_height_BLUP;Peiffer_2014","ear_height_BLUP;Peiffer_2014","plant_height_minus_ear_height_BLUP;Peiffer_2014","ear_height_div_plant_height_BLUP;Peiffer_2014","plant_height_div_DTA_BLUP;Peiffer_2014","leaf_angle_boxcox_transformed_BLUP;Tian_2011")
flowering_time<-c("DTA_ratio_BLUP;Olukolu_2014","ALL_DTA_BLUE;Peiffer_2013","ALL_DTA_BLUP;Peiffer_2013","DTS_BLUP;Hung_2012_1","DTA_BLUP;Hung_2012_1","asi_BLUP;Hung_2012_1","DTA_BLUP;Buckler_2009","DTS_BLUP;Buckler_2009","DTA_BLUP;Wallace_2014","ASI_BLUP;Buckler_2009","GDD_DTS_BLUP;Peiffer_2014","GDD_DTA_BLUP;Peiffer_2014","GDD_ASI_BLUP;Peiffer_2014","DTS_BLUP;Peiffer_2014","DTA_BLUP;Peiffer_2014","ASI_BLUP;Peiffer_2014","GDD_DTA_long_BLUP;Hung_2012","GDD_DTA_short_BLUP;Hung_2012","GDD_DTA_photo_resp_BLUP;Hung_2012","GDD_DTS_long_BLUP;Hung_2012","GDD_DTS_short_BLUP;Hung_2012","GDD_DTS_photo_resp_BLUP;Hung_2012","GDD_DTA_NC06_BLUP;Hung_2012","GDD_DTS_NC06_BLUP;Hung_2012","GDD_DTA_MO06_BLUP;Hung_2012","GDD_DTS_MO06_BLUP;Hung_2012","GDD_DTA_NY06_BLUP;Hung_2012","GDD_DTS_NY06_BLUP;Hung_2012","GDD_DTA_IL06_BLUP;Hung_2012","GDD_DTS_IL06_BLUP;Hung_2012","GDD_DTA_FL06_BLUP;Hung_2012","GDD_DTS_FL06_BLUP;Hung_2012","GDD_DTA_PR06_BLUP;Hung_2012","GDD_DTS_PR06_BLUP;Hung_2012","GDD_DTA_NC07_BLUP;Hung_2012","GDD_DTS_NC07_BLUP;Hung_2012","GDD_DTA_MO07_BLUP;Hung_2012","GDD_DTS_MO07_BLUP;Hung_2012","GDD_DTA_NY07_BLUP;Hung_2012","GDD_DTS_NY07_BLUP;Hung_2012","GDD_DTA_IL07_BLUP;Hung_2012","GDD_DTS_IL07_BLUP;Hung_2012","GDD_DTA_FL07_BLUP;Hung_2012","GDD_DTS_FL07_BLUP;Hung_2012")
misc<-c("glsblup;Benson_2015","flecking_ls_mean;Olukolu_2016","flecking_poisson_transformation_BLUP;Olukolu_2016")
vitamin_E<-c("alphaT_transformed_BLUE;Diepenbrock_2017","deltaT_transformed_BLUE;Diepenbrock_2017","gammaT_transformed_BLUE;Diepenbrock_2017","TotalT_transformed_BLUE;Diepenbrock_2017","alphaT3_transformed_BLUE;Diepenbrock_2017","deltaT3_transformed_BLUE;Diepenbrock_2017","gammaT3_transformed_BLUE;Diepenbrock_2017","TotalT3_transformed_BLUE;Diepenbrock_2017","TotalT_plus_T3_transformed_BLUE;Diepenbrock_2017","plastochromanol8_transformed_BLUE;Diepenbrock_2017","alphaT_div_gammaT_transformed_BLUE;Diepenbrock_2017","alphaT3_div_gammaT3_transformed_BLUE;Diepenbrock_2017","gammaT_div_gammaTPlusalphaT_transformed_BLUE;Diepenbrock_2017","gammaT3_div_gammaT3PlusalphaT3_transformed_BLUE;Diepenbrock_2017","deltaT_div_gammaTPlusalphaT_transformed_BLUE;Diepenbrock_2017","deltaT_div_alphaT_transformed_BLUE;Diepenbrock_2017","deltaT_div_gammaT_transformed_BLUE;Diepenbrock_2017","deltaT3_div_gammaT3PlusalphaT3_transformed_BLUE;Diepenbrock_2017","deltaT3_div_alphaT3_transformed_BLUE;Diepenbrock_2017","deltaT3_div_gammaT3_transformed_BLUE;Diepenbrock_2017","TotalT_div_totalT3_transformed_BLUE;Diepenbrock_2017")
metabolites<-c("chlorophylla_BLUP;Wallace_2014","chlorophyllb_BLUP;Wallace_2014","malate_BLUP;Wallace_2014","fumarate_BLUP;Wallace_2014","fumarate2_BLUP;Wallace_2014","glutamate_BLUP;Wallace_2014","aminoacids_BLUP;Wallace_2014","protein_BLUP;Wallace_2014","nitrate_BLUP;Wallace_2014","starch_BLUP;Wallace_2014","sucrose_BLUP;Wallace_2014","glucose_BLUP;Wallace_2014","fructose_BLUP;Wallace_2014","principal_component_metabolies1;Wallace_2014","principal_component_metabolies2;Wallace_2014")

melted_Hperm0<-H_perm0 %>% select(c(contains("Va"),Trait)) %>% melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component")

melted_Hperm00<-mutate(melted_Hperm0, Trait_Category = case_when(Trait %in% tassel_architecture ~ "tassel_architecture",
                                                                 Trait %in% ear_architecture ~ "ear_architecture",
                                                                 Trait %in% disease ~ "disease",
                                                                 Trait %in% stalk_strength ~ "stalk_strength",
                                                                 Trait %in% plant_architecture ~ "plant_architecture",
                                                                 Trait %in% flowering_time ~ "flowering_time",
                                                                 Trait %in% misc ~ "misc",
                                                                 Trait %in% vitamin_E ~ "vitamin_E",
                                                                 Trait %in% metabolites ~ "metabolites")) %>% as.data.frame()

str(melted_Hperm00)

vaSPE <- melted_Hperm00[str_detect(melted_Hperm00$K_Component, "Va_SPE"), ]
view(vaSPE)
data1 <- iris[str_detect(iris$Species, "virg"), ]  # Extract matching rows with str_detect


view(melted_Hperm00)
for(i in c("ear_architecture","disease","penetrometer","plant_architecture","flowering_time","misc","vitamin_E","metabolites")){
  assign(str_c("bxplt_",i), melted_Hperm00 %>% filter(Trait_Category == i) %>% ggplot(aes(x=Trait)) +geom_boxplot(aes(y=percent_Va, color=K_Component))+coord_flip()+theme(axis.text.y = element_text(size = 6))+xlab("")+ylab("Va %")+scale_color_brewer(palette = "Set1")+ggtitle(str_c("Traits of ",i)))
}

for(i in ls(pattern = "bxplt")){ggsave(file=str_c(.,i,".pdf"), plot = get(i), device = "pdf")}


view(melted_Hperm00)



#loop for everything?
#load in files
H_perm1<-read_tsv("TPMbyL_1000_perm_1.h_estimates.txt")
H_perm2<-read_tsv("TPMbyL_1000_perm_2.h_estimates.txt")
H_perm3<-read_tsv("TPMbyL_1000_perm_3.h_estimates.txt")
H_perm4<-read_tsv("TPMbyL_1000_perm_4.h_estimates.txt")
H_perm5<-read_tsv("TPMbyL_1000_perm_5.h_estimates.txt")
H_perm6<-read_tsv("TPMbyL_1000_perm_6.h_estimates.txt")
H_perm7<-read_tsv("TPMbyL_1000_perm_7.h_estimates.txt")
H_perm8<-read_tsv("TPMbyL_1000_perm_8.h_estimates.txt")
H_perm9<-read_tsv("TPMbyL_1000_perm_9.h_estimates.txt")
H_perm10<-read_tsv("TPMbyL_1000_perm_10.h_estimates.txt")

#loop to format
format_Hperm<-function(df){
  names(df)[13]<-"Trait"
  names(df)<- names(df) %>% str_replace_all(.,"_1","_SD")
  names(df)[1]<-"Component" 
  names(df)[2]<-"Her_K1" 
  names(df)[3]<-"Her_K2" 
  names(df)[4]<-"Her_K3" 
  names(df)[5]<-"Her_Top" 
  names(df)[6]<-"Her_ALL" 
  names(df)[7]<-"Component" 
  names(df)[8]<-"Her_K1SD" 
  names(df)[9]<-"Her_K2SD" 
  names(df)[10]<-"Her_K3SD" 
  names(df)[11]<-"Her_TopSD" 
  names(df)[12]<-"Her_ALLSD" 
  df<-df %>% select(-contains("Component"))
  return(df)
}

H_perm1<-format_Hperm(H_perm1)
H_perm2<-format_Hperm(H_perm2)
H_perm3<-format_Hperm(H_perm3)
H_perm4<-format_Hperm(H_perm4)
H_perm5<-format_Hperm(H_perm5)
H_perm6<-format_Hperm(H_perm6)
H_perm7<-format_Hperm(H_perm7)
H_perm8<-format_Hperm(H_perm8)
H_perm9<-format_Hperm(H_perm9)
H_perm10<-format_Hperm(H_perm10)

#combining into master dataframe
df_names<-ls(pattern = "^H_perm")
H_permALL<-H_perm1
for(i in 2:length(df_names)){
  H_permALL<-bind_rows(H_permALL, get(df_names[i]))
}
view(H_permALL)

#calculations
H_permALL<-mutate(H_permALL, 
                  Va_SPE = Her_K1 / Her_ALL,
                  Va_nonSPE = Her_K2 / Her_ALL,
                  Va_Rest = Her_K3 / Her_ALL)

H_permALL %>% select(c(contains("Va"),Trait)) %>% melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component") %>%
  ggplot(aes(x=Trait)) +
  geom_boxplot(aes(y=percent_Va, color=K_Component))+
  coord_flip()+
  theme(axis.text.y = element_text(size = 7))+
  xlab("Trait")+ylab("Va %")+
  scale_color_brewer(palette = "Set2")

view(H_permALL)

melted_HpermALL<-H_permALL %>% select(c(contains("Va"),Trait)) %>% melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component")
view(melted_HpermALL)

melted_stats_HpermALL<-H_permALL%>% 
  select(c(contains("Va"),Trait)) %>% 
  melt(id.vars="Trait", value.name="percent_Va", variable.name="K_Component") %>% 
  group_by(Trait,K_Component)%>% 
  summarize(Mean = mean(percent_Va), Median = median(percent_Va), std_dev=sd(percent_Va)) 

view(melted_stats_HpermALL)

vaSPEs <- melted_stats_HpermALL[str_detect(melted_stats_HpermALL$K_Component, "Va_SPE"), ]
view(vaSPEs)

melted_stats_HpermALL<-mutate(melted_stats_HpermALL, Trait_Category = case_when(Trait %in% tassel_architecture ~ "tassel_architecture",
                                                                                Trait %in% ear_architecture ~ "ear_architecture",
                                                                                Trait %in% disease ~ "disease",
                                                                                Trait %in% stalk_strength ~ "stalk_strength",
                                                                                Trait %in% plant_architecture ~ "plant_architecture",
                                                                                Trait %in% flowering_time ~ "flowering_time",
                                                                                Trait %in% misc ~ "misc",
                                                                                Trait %in% vitamin_E ~ "vitamin_E",
                                                                                Trait %in% metabolites ~ "metabolites")) %>% as.data.frame()
view(melted_HpermALL)






H_permALL %>%
  ggplot(aes(x=reorder(Trait,Va_SPE)))+
  geom_point(aes(y=Va_Rest, color="3_Rest"), alpha = 0.3, size = 1)+
  geom_point(aes(y=Va_nonSPE, color="2_Background"), alpha = 0.3,size = 1)+
  geom_point(aes(y=Va_SPE, color="1_SPE"), alpha = 0.3,size = 1)+
  geom_point(data = melted_stats_HpermALL, aes(x=Trait, y=Mean, shape = K_Component)) + 
  geom_point(data = melted_stats_Hperm0, aes(x=Trait, y=Mean, shape = K_Component)) + 
  scale_shape_manual(values=c(16, 6, 4))+
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_blank())+ #, color = H_perm_ALL$trait_colors
  xlab("")+ylab("Va %")+scale_color_brewer(palette = "Set2")+ggtitle(str_c("Estimated Va of All Traits with with 10 Permutations, 1000 SPE Genes"))+
  theme(legend.title = element_blank())

ggplot(H_permALL, aes(y=reorder(Trait,Va_SPE))) + 
  stat_density_ridges(aes(x=Va_Rest,fill="3_Rest"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.01)+
  stat_density_ridges(aes(x=Va_nonSPE,fill="2_nonSPE"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.01)+
  stat_density_ridges(aes(x=Va_SPE,fill="1_SPE"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.01)+
  ylab("Trait")+ xlab("Va %") +
  scale_fill_brewer(palette = "Set2")+
  geom_point(data = melted_stats_HpermALL, aes(y=Trait, x=Mean, shape = K_Component)) + 
  scale_shape_manual(values=c(16, 6, 4))+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())+
  ggtitle(str_c("Estimated Va of All Traits with 10 Permutations, 1000 SPE Genes"))+
  theme(legend.title = element_blank())




ggplot(H_permALL, aes(y=reorder(Trait,Va_SPE))) + 
  stat_density_ridges(aes(x=Va_Rest,fill="3_Rest"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.03)+
  stat_density_ridges(aes(x=Va_nonSPE,fill="2_nonSPE"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.03)+
  stat_density_ridges(aes(x=Va_SPE,fill="1_SPE"),alpha=0.75,size=0,from=0,to=1,bandwidth = 0.03)+
  ylab("Trait")+ xlab("Va %") +
  scale_fill_brewer(palette = "Set2")+
  geom_point(data = melted_stats_Hperm0, aes(y=Trait, x=Mean, shape = K_Component)) + 
  scale_shape_manual(values=c(16, 6, 4))+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())+
  ggtitle(str_c("Estimated Va of All Traits with 10 Permutations, 1000 SPE Genes"))+
  theme(legend.title = element_blank())
































#v3data_set_3 <- read_xlsx("data_set_3.xlsx") 
#filter(V3V4_conversions, v3geneID %in% v3data_set_3$"v3geneID") #results in tibble with 38 747 rows, this is finding matches from v3geneid in master spreadsheet to genes in the gene_ID colum in data set 3
#v4data_set_3 <- v3data_set_3 %>% inner_join(V3V4_conversions,by="v3geneID")

#try 500000
#v4_v5 <- read.delim("B73v4_B73v5_header.txt")
#write_tsv(v4_v5, "b73v4_v5_conversions.tsv")

#v4_v5new <- read.delim("B73v4_to_B73v5new.txt")
#write_tsv(v4_v5new, "B73v4_to_B73v5new.tsv") 
#view(v4_v5new)

#colnames(v4_v5new)[1] <- "v4geneID"
#colnames(v4_v5new)[2] <- "v5geneID"

#nomany0v4v5 <- subset(v4_v5new, v5geneID!="") #removes 1:0

#nomany0v4v5 <- nomany0v4v5[!grepl(",", nomany0v4v5$v5geneID), ] #removes 1:many
#unique_SPE_genes <- read_tsv("unique_SPE_genes.tsv") %>% as.data.frame() #13871 genes
#view(unique_SPE_genes)

#SPE_v5new <- inner_join(unique_SPE_genes, nomany0v4v5, by = c("v4geneID")) %>% as.data.frame() #9,290 = no better

#view(SPE_v5new)
#str(SPE_v5new)


#4. Run for loop to pull out all rows for each unique SPE gene ID, then see if they have a lot of rows
#pub_results <- tibble(geneID = NA, in_pubs = NA)  #makes empty tibble to pipe results into

#for (p in 1:length(unique_SPE_genes)) { #setting length to the number of unique SPE genes
# temporary <- filter(merged_data_set_simple, geneID==unique_SPE_genes[p]) #filters out values that match in geneID and unique SPE genes into a temporary tibble that will be overwritten
# max_names <- names(which.max(table(temporary$in_pubs))) #use ifelse insteade statement #filters out most popular publication string from each temporary tibble? I think this is fundamentally wrong but I tried it anyways????
# pub_results <- add_row(pub_results, #adds row to empty tissue results tbble from earlier
#     geneID = unique_SPE_genes[p], #tissue will fill from unique tissue list
#    in_pubs = max_names) #in pubs will come from max names which is the most frequent publication string
# }
#view(pub_results)  #so this worked? I think?

#pub_summary <- table(pub_results$in_pubs) #creates summary table with string and then number of times it shows up
#view(pub_summary)
#now I probably should remove rows with 0 in SPE_YN so I only get genes that are spe
#frequency <- table(merged_data_set_simple$in_pubs)
#view(frequency)
#but I have to deal with tissues overlapping.... uh time to reuse the for loop
#T/F, looks for geneID anywhere in set1 vector, if yes, true = pub1, repeat for other pubs
#then create column for all pubs to make vendiagram use str_c (add strings together from in_pub1, in_pub2, in_pub3, then add separator)
#in_pubs = str_c(in_pub1, in_pub2, in_pub3, sep = " ") #for venndiagram, use pptx and manually draw, can pull out unique values to determine how many in each category. 
#How many genes have been called SPE across the datasets, overlap
#for (id in 1:5) {
# print(id)
# filter(merged_data_set_simple, geneID==unique_gene_list[id])
# sums <- select(temp,-c(geneID, tissue)) %>% 
#  colSums(., na.rm = TRUE) %>% as.data.frame()
# sums$. <- ifelse(sums$.>0,1,0) #turns the values from the sums df greater than 0 to 1 to make it a 1,0 binary
# sums <- colSums(sums) #somehow need to preserve gene ID as column name????
# results_dataframe <- results_dataframe %>% 
#  add_row(number_SPE_crosses = sums)
#}
#view(sums)
#str(sums)
#view(results_dataframe)
#5. Run for loop to get output of gene ID and how many crosses show up in it
#for final ex. For dataframe, make empty data frame up there, use add_row command in the actual forloop
#results_vector<-c() #-as.tibble(geneID = NA, .... = NA) to make empty dataframe, use addrow(df, geneID = ) only make 2 columns, gene ID and #ofspecrosses
#str(results_vector)
#for(i in 1:10){
# print(i)
#results_vector<-
#  c(results_vector, i) #use addrow insead of column command when working with df
#}
#view(results_vector)
#for(id in 1:5) {
# filter(merged_data_set, geneID==unique_gene_list[id]) %>% 
# }
























#merged_data_set <- data_set_101 %>% 
# inner_join(data_set_2_cast, v4data_set_3, by = "geneID", "tissue") #I know this is not right but am struggling, will only pull 






#view(trial_forloop)
#str(trial_forloop)


#SPEinXcrosses <- colSums(stacked_123[79:84])
#output <- add_row(geneID = )





# Merging sheets for ear, flag leaf, and tassel from supplemental dataset s1 -----------------------------
# excel_sheets(path)
#sheet = excel_sheets("Supplemental_dataset_S1.xlsx")
#data_frame = lapply(setNames(sheet, sheet), 
#function(x) read_excel("Supplemental_dataset_S1.xlsx", sheet=x))                    
#data_frame = bind_rows(data_frame, .id="Sheet")                     
#print (data_frame)
#view(data_frame)
#Results in 18718 rows signifying that everything is now in one sheet (appx 6000 rows from each page)
#head(data_frame)
#str(data_frame)


# (all genes in 3 tissues from Supplemental_dataset_S1 Unique genes among ear, flagleaf, and tassel in ("Supplemental_dataset_S1.xlsx") ----------------------------

#unique(data_frame[c("geneID")])
#Results in 10,965 total rows

# (genes expressed in all 3 tissues set 1)Same genes among ear, flagleaf, and tassel in ("Supplemental_dataset_S1.xlsx")  ---------------------------

#data_framegeneID <- select(data_frame,c(geneID, Sheet))
#head(data_framegeneID)
#str(data_framegeneID)
#duplicated(data_framegeneID)
#which(duplicated(data_framegeneID)) #I'm struggling to pull a list of names - I'm just able to pull the rows that have duplicate values, Sam, you have a better idea of how to pull a list of genes that are present in all 3 tissues?
#duplicates <- filter(data_framegeneID,duplicated(geneID)) #duplicated at least 2x, create 3 lists
#dup_tassel <- filter(data_framegeneID,duplicated(geneID)) %>% filter(Sheet=="tassel") %>% pull(geneID)
#str(dup_tassel)
#head(dup_tassel)#run for flagleaf

#filter(data_framegeneID,duplicated(geneID)) %>% str()
#run filter for flagleaf and tassel, use those 2 sets, go to og frame, where sheets = ear, && gene ID must be in vector for flagleaf and tasse)
#filter(data_framegeneID,Sheet=="ear"&geneID %in%dup_tassel) #run again with flagleaf
#filter(data_framegeneID,Sheet=="flagleaf"&geneID %in%dup_tassel)






# (all genes expressed among sets 1 and 2) Comparing data_frame2 to data_frame for unique gene titles (("Supplemental8_SPE_PAV_gene_list (COPY).xlsx") and ("Supplemental_dataset_S1.xlsx")) --------------

#data_frame2 <- read_xlsx("Supplemental8_SPE_PAV_gene_list (COPY).xlsx")
#head(data_frame2)
#str(data_frame2)
#data_framegeneID <- pull(data_frame,geneID)
#head(data_framegeneID)
#data_framegeneID2 <- pull(data_frame2,geneID)
#str(data_framegeneID2)

#data_frame3 <- c(data_framegeneID, data_framegeneID2)
#head(data_frame3)
#str(data_frame3)
#unique(data_frame3) #soooo, how to I pull the whole list? This is all the unique genes between all the tissues in both of the V4 spreadsheets
#innerjoin to join sets - 

# (same genes among sets 1 and 2) Comparing data_frame2 to data_frame for gene titles that are the same "Supplemental8_SPE_PAV_gene_list (COPY).xlsx") and ("Supplemental_dataset_S1.xlsx")) ---------

#same_1and2 <- generics::intersect(data_framegeneID, data_framegeneID2)
#str(same_1and2) #did this work????

#Options to chunk the data of SPE genes: 
#accumulation of all genes present in all datasets (looking for unique)
#genes present in every tissue across all datasets 
#genes present in all datasets (doesn't need to be any every tissue/cross)
#for crosses, turn into binary, use mutate and sum coll.
#turn into binary, stringr - package in tidyr
#to combine B and X, mutate

# I want to build a master spreadsheet combining supp 1 and supp 8 by organizing by geneID, sheet/tissue, crosses in binary (1 for )
# Step one, mutate x and NA in crosses in Supp 1 data_frame to binary x = 1 NA = 0
#head(data_frame)
#data_frame %>% 
#mutate(across(c(SPE_B_A554_E, SPE_X_A554_E, SPE_B_H84_E, SPE_X_H84_E, SPE_B_H99_E, SPE_X_H99_E, SPE_B_Mo17_E, SPE_X_Mo17_E, SPE_B_Oh43_E, SPE_X_Oh43_E, SPE_B_W64A_E, SPE_X_W64A_E),
#    ~factor(ifelse(.x == "x",1,0)))) 
#head(data_frame)#wooo this works, all x = 1 and NA = 0)
# Step two, rearrange supplementa8 into tissuT|geneID|female/cross||||
#data_frame2 <- read_xlsx("Supplemental8_SPE_PAV_gene_list (COPY).xlsx")
#head(data_frame2)
#mergedata_frame2 <- data_frame2 %>% 
#  group_by(geneID, Tissue)
#head(mergedata_frame2)
#str(mergedata_frame2)
#str(data_frame2)

#reshape(data_frame2,idvar = "geneID", timevar ="Female", direction ="wide")
#head(data_frame2)

#V3 to V4 conversions for dataset 3 - Did I do this remotely right?
#V3V4_conversions <- read_xlsx("V3 V4 conversions.xlsx")
#head(V3V4_conversions)
#str(V3V4_conversions)
#v3dataframe <- read_xlsx("1-s2.0-S0960982217316603-mmc3 (COPY).xlsx")
#head(v3dataframe) #lowercasev
#filter(V3V4_conversions, v3geneID %in% v3dataframe$"v3geneID")
#converted_data3 <- inner_join(x=v3dataframe,y=(V3V4_conversions),by="v3geneID")
#str(converted_data3)
#head(converted_data3)








