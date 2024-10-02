# ACS PUMS 

setwd("C:/Dat_Sci/Datasets")

# Getting Data from IPUMS requires permission. They vet your request and then provide a downloadable zip file
# If Unzipping
#library(utils)
#zip_file<-"ACS PUMS/usa_00011.csv"
#output_files<-"ACS PUMS"
#unzip(zipfile=zip_file, exdir=output_files)
#file.info(zip_file)

ACS_2022<-read.csv("ACS PUMS/usa_00011.csv")
str(ACS_2022)
# I Asked for a dozen or so columns, plus ACS gives you some extras, like DEGFIELDD (which is a detailed code of DEGFIELD (degree field)).

# I'm interested in what types of places certain occupations tend to conglomerate. 
# The ACS is collected at the PUMA level, but they have a code MET2013, which refers to the metro area (defined in 2013) that a PUMA is located. Sometimes, however, PUMAs don't neatly correspond to Metro areas. 

# ACS data does not come with metro areas or occupation categories labeled.
# The trick is to copy/paste the code book to a excel file, save as a .csv, and then merge on the numeric code. 
# [CODEBOOK]

metro_labels<-read.csv("ACS PUMS/MET2013_Labels.csv")
str(metro_labels)
ACS_2022<-merge(ACS_2022, metro_labels, by = "MET2013", all.x=TRUE)

# Do the same for occupation labels.
occ_labels<-read.csv("ACS PUMS/Occ_Names.csv")
ACS_2022<-merge(ACS_2022, occ_labels, by="OCC", all.x=TRUE)

# Where are the artists (in the data)?

# DEGFIELD for Fine Arts = 60
sum(ACS_2022$DEGFIELD==60)
# DEGFIELDD for Visual Artists = 6003 # "Visual and Performing Arts"
sum(ACS_2022$DEGFIELDD==6003)

# OCC (Occupation) code for "Artists and Related Workers" = 2600
sum(ACS_2022$OCC==2600)
# There are 17883 cases of people who claimed Artist as their primary occupation in the 5 1% microsample surveys 2018-2022. 

# One possibility to reduce noise in the data would be to filter out only those artists that also have a degree in Fine Art, 
# but that wouldn't be fair to many of the most famous American artists like Basquiat. 
sum(ACS_2022$OCC==2600 & ACS_2022$DEGFIELD==60)
sum(ACS_2022$OCC==2600 & (ACS_2022$DEGFIELD==60 | ACS_2022$DEGFIELD2==60))
# Fine_art_degrees<-ACS_2022[ACS_2022$DEGFIELD==60 | ACS_2022$DEGFIELD2==60, ]

# Arts Occupation Codes
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
# Occupationally, some fields tend to be more industry oriented (e.g., Producers & Directors)

# Lets make a table that aggregates arts occupation populations for each Metro area. 

# First, subset our arts occupations 
library(dplyr)

Art_occ_dt<-ACS_2022[ACS_2022$OCC %in% c(2600,2631,2632,2634,2635,2640,2700,2710,
				       2740,2751,2752,2850,2910),]

# Next, aggregate by summing each occupation within each Metro, being sure to use the per/person weights provided in the data
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

# To see whether or not a metro arts population is not simply a function of that city's size, lets construct a *relative* measure that accounts for Metro size. 
# Our Location Quotient will be the relative share of each art occupation in a city vs. in the national work force. 
# (local occ population/local labor force)/(national occ population/national labor)

# Constants
# National Total Labor Force
# OCC>0 ensures that we're counting the labor force (i.e., has an occupational code), not the whole population. 
nat_tot_labor_force<-sum(ACS_2022$PERWT[ACS_2022$OCC>0])

# Metro labor force (a sum of workforce in each Metro)
metro_labor_force<-ACS_2022 %>%
	filter(OCC>0) %>%
	group_by(Metro) %>%
	summarize(Metro_labor_force = sum(PERWT, na.rm=TRUE))

# Include Metro labor force in our aggregated table
Arts_agg<-merge(metro_labor_force, 
		Arts_agg, 
		by = "Metro", all.x=TRUE)

# So, now we have 1. Metro artist labor force, 2. Metro labor force, 3. National labor force, and 4. We can get the national artist' labor force by summing the local artists in each Metro.  

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





##############
# TECH WORKERS
##############

tech<-subset(ACS_2022, OCC %in% c(1005,1006,1007,1010,1050,1105,1106,0110,1108,1065,1022,1032,1021,1031))

# Metro tech Pop
tech_temp<-tech %>%
	group_by(Metro) %>%
	summarize(Tech_pop = sum(PERWT))

occ_geo_table<-merge(occ_geo_table, 
		     tech_temp,
		     by = "Metro", all.x=TRUE)

# National tech pop
nat_tech_pop<-sum(tech$PERWT)
tech_prop_nat<-nat_tech_pop/nat_tot_labor_force

occ_geo_table<-mutate(occ_geo_table, Tech_rel = (Tech_pop/Metro_labor_force)/tech_prop_nat)


##############
# MUSICIANS
##############
# 2751: Music Directors and Composers
# 2752: Musicians and Singers

music<-subset(ACS_2022, OCC %in% c(2751,2752))

# Metro music Pop
music_temp<-music %>%
	group_by(Metro) %>%
	summarize(Music_pop = sum(PERWT))

occ_geo_table<-merge(occ_geo_table, 
		     music_temp,
		     by = "Metro", all.x=TRUE)

# National music pop
nat_mus_pop<-sum(music$PERWT)
mus_prop_nat<-nat_mus_pop/nat_tot_labor_force

occ_geo_table<-mutate(occ_geo_table, Music_rel = (Music_pop/Metro_labor_force)/mus_prop_nat)


######################### 
# Financial Analysts
#########################

financial<-subset(ACS_2022, OCC %in% c(0120,0845,0850,0960))

# Metro tech Pop
fin_temp<-financial %>%
	group_by(Metro) %>%
	summarize(Financial_pop = sum(PERWT))

occ_geo_table<-merge(occ_geo_table, 
		     fin_temp,
		     by = "Metro", all.x=TRUE)

# National fin pop
nat_fin_pop<-sum(financial$PERWT)
fin_prop_nat<-nat_fin_pop/nat_tot_labor_force

occ_geo_table<-mutate(occ_geo_table, Fin_rel = (Financial_pop/Metro_labor_force)/fin_prop_nat)

### Make .csv######
write.csv(occ_geo_table, "ACS PUMS/Art_by_Metro_Table.csv")

### Make data table for shinyApp

app_table<-ACS_2022[,c("OCC","Metro","PERWT")]



###########
#PLAY

####### Code Artists with Fine Arts Degrees ########

art_ed<-subset(artists, DEGFIELD == 60 | DEGFIELD2 == 60) # about 30% of original artists table 

metro_art_ed_pop<-art_ed %>%
	group_by(Metro) %>%
	summarize(art_ed_pop = sum(PERWT))

# National Artist Pop
national_art_ed_pop<-sum(art_ed$PERWT)

art_ed_prop_nat<-national_art_ed_pop/nat_tot_labor_force 

agg_art_ed_temp<-merge(metro_art_ed_pop,
		       metro_labor_force,
		       by = "Metro", all.x=TRUE)
agg_art_ed<-merge(agg_art_ed_temp,
		  metro_pop,
		  by = "Metro", all.x=TRUE)

metro_artists_all<-merge(agg_art_ed,
			 metro_artists,
			 by = "Metro", all.x=TRUE)

metro_artists_all<-mutate(metro_artists_all, relative_art_ed_pop = (art_ed_pop/Met_labor_force.x)/art_ed_prop_nat)



######################################################################
# Filter out High Metro Uncertainty; i.e. MET2013ERR = 5-6, i.e., >5%

art_filtered<-subset(ACS_2022, OCC == 2600 & MET2013ERR <=4)

# calculate relative metro populations

metro_artist_pop_f<-art_filtered %>%
	group_by(Metro) %>%
	summarize(artist_population = sum(PERWT))

# Metro labor force

metro_labor_force_f<-ACS_2022 %>%
	filter(OCC>0) %>%
	filter(MET2013ERR<=4) %>%
	group_by(Metro) %>%
	summarize(Met_labor_force = sum(PERWT, na.rm=TRUE))

metro_pop_f<-ACS_2022 %>%
	filter(MET2013ERR<=4) %>%
	group_by(Metro) %>%
	summarize(Metro_population_est = sum(PERWT, na.rm=TRUE))

national_artist_pop<-sum(artists$PERWT) # stays the same?

nat_tot_labor_force<-sum(ACS_2022$PERWT[ACS_2022$OCC>0]) # stays the same?

art_prop_nat<-national_artist_pop/nat_tot_labor_force # stays the same?

agg_artists_temp_f<-merge(metro_artist_pop_f,
			  metro_labor_force_f,
			  by = "Metro", all.x=TRUE)
agg_artists_f<-merge(agg_artists_temp_f,
		     metro_pop_f,
		     by = "Metro", all.x=TRUE)

metro_artists_f<-mutate(agg_artists_f, relative_art_pop = (artist_population/Met_labor_force)/art_prop_nat)

# But the filtered version returns very inconsistent data, e.g., 
pdx<-subset(ACS_2022, Metro=="Portland-Vancouver-Hillsboro, OR-WA")
table(pdx$MET2013$ERR) # is mostly 5s, so filtering out the 5s means truncating Portland from the sample by about 80%
###### EVENTUALLY, ALL White COLLAR


White_Collar_Codes<-c()

# Other OCCs of Interest
# CS, Data, UX
# 1108: Computer occs, all other
# 1065: Database managers & engineers
# 1022: Software quality testers
# 1032: Web and Digital Interface Designers
# 1021: Software Developers
# 1031: Web Developers

# Financial Analysts
# 0845: Financial and Investment Analysts
# 0960: Other Financial Specialists

# 8550: Woodworkers, all other (not model/pattern -makers)

# experiment with Portland data
pdx<-subset(ACS_2022, Metro=="Portland-Vancouver-Hillsboro, OR-WA")

