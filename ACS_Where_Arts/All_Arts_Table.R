# All Arts Table


# Summary: The American Community Survey (ACS) is a 1% survey of the U.S. population conducted annually by the U.S. Census Bureau as a complement to the decennial census. 
# The ACS adds depth and a broader range of demographic, housing and economic data that census cannot capture. 
# The 1% "microsamples" are available in 5-year, 5% packages, which provide more stable population estimates. The 2022 5-year ACS, for example, is actually 5, 1-year, 1% samples combined 2018-2022.

# Data are available online at census.gov, or at the more user friendly IPUMS website (https://usa.ipums.org/usa/). IPUMS stands for Integrated Public Use Microsample Series.
# Users can select the samples and the columns of data they want to analyze, and find all the codebooks, questionaires and other documentation there.

# Getting Data from IPUMS requires permission. They vet your request and then provide a downloadable zip file, usually within hours.
# Beware of large file sizes! Each 1% of the U.S. population is about 3.5 million cases. A 5-year dataset will contain close to 18 million rows. 


# setwd("C:/Dat_Sci/Datasets")

# If Unzipping
#library(utils)
#zip_file<-"ACS PUMS/usa_00011.csv"
#output_files<-"ACS PUMS"
#unzip(zipfile=zip_file, exdir=output_files)
#file.info(zip_file)

ACS_2022<-read.csv("ACS PUMS/usa_00011.csv")
str(ACS_2022)
# I Asked for a dozen or so columns, plus IPUMS gives you some extras, like DEGFIELDD (which is a detailed code of DEGFIELD (degree field)).
# Make sure you ask for Person and/or Household weights, depending on your level of analysis. 

# In this project I'm interested in what types of places certain occupations tend to agglomerate. Or, what types of occupations agglomerate (or are diffuse) in particular places. In the long run, I'll want to use this data to see what effect the pandemic had on occupational agglomeration. 

## The ACS is collected at the public use microsample area (PUMA) level, but they are coded MET2013, which refers to the metro area (defined in 2013) that a PUMA is located. Sometimes, however, PUMAs don't neatly correspond to Metro areas. From what I gather, some PUMAs contain subsamples that are up to 15% outside the Metro area boundary. 
## The ACS comes with a MET2013ERROR that tells us this for each PUMA. However, some Metros, like Portland, OR, contain mostly PU


# Because I'm interested in Metro Areas and Occupations, the first step will be to add the labels to these codes. 
# Rather than manually recoding these variables, however (There are 282 Metro codes and 500+ occupation codes), we'll recode by merging a list. 
# The trick is to copy/paste the code book to a excel file, save as a .csv, and then merge on the numeric code. 
# [IMG. CODEBOOK]

metro_labels<-read.csv("ACS PUMS/MET2013_Labels.csv")
str(metro_labels)
ACS_2022<-merge(ACS_2022, metro_labels, by = "MET2013", all.x=TRUE)

# Do the same for occupation labels.
occ_labels<-read.csv("ACS PUMS/Occ_Names.csv")
ACS_2022<-merge(ACS_2022, occ_labels, by="OCC", all.x=TRUE)

# Where are the artists (in the data)?


# OCC (Occupation) code for "Artists and Related Workers" = 2600
sum(ACS_2022$OCC==2600)
# There are 17883 cases of people who claimed Artist as their primary occupation in the 5 1% microsample surveys 2018-2022. 
sum(ACS_2022$PERWT[ACS_2022$OCC==2600])
# which theoretically represent (i.e., with person weights) 354,607 artists.

# But there are many arts occupations:
# 2600: Artists and Related Workers
# 2631: Commercial and Industrial Designers
# 2632: Fashion Designers
# 2633: Floral Designers (?)
# 2634: Graphic Designers
# 2635: Interior Designers 
# 2640: Other Designers
# 2700: Actors
# 2710: Producers and Directors
# 2740: Dancers and Choreographers
# 2751: Music Directors and Composers
# 2752: Musicians and Singers
# 2850: Writers and Authors
# 2910: Photographers

# Of course not all arts fields are the same. 
# Much ink has been spilled trying to differentiate art vs. commerce, for example. 
# Occupationally, some fields tend to be more industry oriented (e.g., Producers & Directors).

# Lets make a table that aggregates arts occupation populations for each Metro area. 

# First, subset our arts occupations 

Art_occ_dt<-ACS_2022[ACS_2022$OCC %in% c(2600,2631,2632,2634,2635,2640,2700,2710,
					 2740,2751,2752,2850,2910),]

# Next, aggregate by summing each occupation within each Metro, being sure to use the per/person weights provided in the data

library(dplyr)

Arts_agg<-Art_occ_dt %>%
	group_by(Metro) %>%
	summarize(
		Artists = sum(PERWT[OCC==2600], na.rm=TRUE),
		Graphic_Designers = sum(PERWT[OCC==2634], na.rm=TRUE),
		Fashion_Designers = sum(PERWT[OCC==2632], na.rm=TRUE),
		Other_Designers = sum(PERWT[OCC %in% c(2631,2633,2635,2640)], na.rm=TRUE),
		Musicians = sum(PERWT[OCC %in% c(2751,2752)], na.rm=TRUE),
		Writers = sum(PERWT[OCC==2850], na.rm=TRUE),
		Photographers = sum(PERWT[OCC==2910], na.rm=TRUE),
		Dancers_Chors = sum(PERWT[OCC==2740], na.rm=TRUE),
		Actors = sum(PERWT[OCC==2700], na.rm=TRUE),  
		Producers_Dirs = sum(PERWT[OCC==2710], na.rm=TRUE)  
	)

# To see whether or not a metro arts population is not simply a function of that city's size, I construct a *relative* measure that accounts for Metro size. 
# Our Location Quotient (LQ) will be the relative share of each art occupation in a city vs. compared to their portion of the national work force. 
# That is: (local occupation population/local total labor force)/(national occupation population/national total labor force)

# 1. National Total Labor Force
nat_tot_labor_force<-sum(ACS_2022$PERWT[ACS_2022$OCC>0])
# OCC>0 ensures that we're counting the labor force (i.e., has an occupational code), not the whole population. 

# 2. Metro labor force (a sum of workforce in each Metro)
metro_labor_force<-ACS_2022 %>%
	filter(OCC>0) %>%
	group_by(Metro) %>%
	summarize(Metro_labor_force = sum(PERWT, na.rm=TRUE))

# 3. Include Metro labor force in our aggregated table
Arts_agg<-merge(metro_labor_force, 
		Arts_agg, 
		by = "Metro", all.x=TRUE)

# 4. We can get the national artist' labor force by summing the local artists in each Metro.  

Arts_relative<-Arts_agg %>%
	mutate(Artists = (Artists/Metro_labor_force)/(sum(Artists)/nat_tot_labor_force),
	       Graphic_Designers = (Graphic_Designers/Metro_labor_force)/(sum(Graphic_Designers)/nat_tot_labor_force),
	       Fashion_Designers = (Fashion_Designers/Metro_labor_force)/(sum(Fashion_Designers)/nat_tot_labor_force),
	       Other_Designers = (Other_Designers/Metro_labor_force)/(sum(Other_Designers)/nat_tot_labor_force),
	       Musicians = (Musicians/Metro_labor_force)/(sum(Musicians)/nat_tot_labor_force),
	       Writers = (Writers/Metro_labor_force)/(sum(Writers)/nat_tot_labor_force),
	       Photographers = (Photographers/Metro_labor_force)/(sum(Photographers)/nat_tot_labor_force),
	       Dancers_Chors = (Dancers_Chors/Metro_labor_force)/(sum(Dancers_Chors)/nat_tot_labor_force),
	       Actors = (Actors/Metro_labor_force)/(sum(Actors)/nat_tot_labor_force),  
	       Producers_Dirs = (Producers_Dirs/Metro_labor_force)/(sum(Producers_Dirs)/nat_tot_labor_force)  
	)

Arts_all<-merge(Arts_agg,
		Arts_relative,
		by = "Metro", all.x=TRUE)

setwd("C:/Dat_Sci/Data Projects/ACS_Where_Arts")
write.csv(Arts_all, "Arts_all.csv")


# See you in Tableau
# library(knitr)
# knit("All_Arts_Table.Rmd")

# https://public.tableau.com/shared/73KNZQDPH?:display_count=n&:origin=viz_share_link


