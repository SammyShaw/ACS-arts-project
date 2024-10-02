
## Occ App 3


## Where is your job?

setwd("C:/Dat_Sci/Datasets")

# ACS_2022<-read.csv("ACS PUMS/usa_00011.csv")

# Add Metro Labels
# metro_labels<-read.csv("ACS PUMS/MET2013_Labels.csv")
# str(metro_labels)
# ACS_2022<-merge(ACS_2022, metro_labels, by = "MET2013", all.x=TRUE)
# unique(ACS_2022$Metro)

# Make app table

# app_table<-ACS_2022[,c("OCC","Metro","PERWT")]

# Add Occupation labels
# occ_labels<-read.csv("ACS PUMS/Occ_Names.csv")
# app_table<-merge(app_table, occ_labels, by="OCC", all.x=TRUE)

# write.csv(app_table, "ACS PUMS/app_table.csv")

app_table<-read.csv("ACS PUMS/app_table.csv")

# install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)

occupation_choices<-app_table %>%
	filter(Occupation != "N/A (not applicable)") %>%
	select(OCC, Occupation) %>%
	distinct() %>%
	arrange(Occupation)

occupation_choices<-setNames(occupation_choices$OCC, occupation_choices$Occupation)

# User Interface
# 2.0 with Occupation drop-down menu  change "textInput" to "selectInput"
ui<-fluidPage(
	titlePanel("Occupation by Metro Area"),
	sidebarLayout(
		sidebarPanel(
			selectInput("occ_input", "Select Occupation", choices=occupation_choices),
			actionButton("submit","Submit")
		),
		mainPanel(
			fluidRow(
				column(6,
				       h4("Top 10 Metros by Occupation Population"),
				       tableOutput("top_50_size"),
				       plotOutput("plot_population")),
				column(6,
				       h4("Top 10 per capita"),
				       tableOutput("top_50_relative"),
				       plotOutput("plot_relative"))
			)
		)
	)
)

# Define server logic

server<-function(input,output, session){
	observeEvent(input$submit, {
		occ_code<-as.numeric(input$occ_input)
		print(paste("Selected occupation code:", occ_code))
		
		### Process and Return Data	
		Met_labor_force<-app_table %>%
			filter(OCC>0, Metro!="Not in identifiable area") %>%
			group_by(Metro) %>%
			summarize(Metro_labor_force = sum(PERWT, na.rm=TRUE)) %>%
			arrange(desc(Metro_labor_force))
		
		Met_occupation<-app_table %>%
			filter(OCC==occ_code, Metro!="Not in identifiable area") %>%
			group_by(Metro) %>%
			summarize(Metro_occupation_population = sum(PERWT,na.rm=TRUE))
			
		filtered_data<-merge(Met_labor_force, Met_occupation, by="Metro", all.x=TRUE)
		
		nat_occupation<-sum(app_table$PERWT[app_table$OCC==occ_code],na.rm=TRUE)	
		nat_labor_force<-sum(app_table$PERWT[app_table$OCC>0],na.rm=TRUE)
		
		filtered_data<-filtered_data %>%
			mutate(Relative_size = (Metro_occupation_population/Metro_labor_force)/
								(nat_occupation/nat_labor_force)) 
		
		filtered_data <- filtered_data %>%
			filter(!is.na(Metro_occupation_population) & !is.na(Relative_size))
		
		top_10_metros<-filtered_data %>%
			arrange(desc(Metro_occupation_population)) %>%
			top_n(10,Metro_occupation_population) %>%
			mutate(Rank = row_number()) %>%
			select(Rank, Metro, Metro_occupation_population, Relative_size) %>%
			rename("Occupation Population" = Metro_occupation_population,
			       "Relative Size" = Relative_size) %>%
			na.omit()
	
		top_relative<-filtered_data %>%
			arrange(desc(Relative_size)) %>%
			top_n(10,Relative_size) %>%
			mutate(Rank = row_number()) %>%
			select(Rank, Metro,Relative_size) %>%
			rename("Relative Size (All MSAs)" = Relative_size) %>%
			na.omit()
		
		output$top_50_size <- renderTable({
			top_10_metros
		})
		output$top_50_relative <- renderTable({
			top_relative
		})
		
		output$plot_population<-renderPlot({
			ggplot(top_10_metros, aes(x = reorder(Metro, `Occupation Population`), y = `Occupation Population`)) +
				geom_bar(stat = "identity", fill = "steelblue") +
				coord_flip() +
				labs(title = "Top 50 Metros by Occupation Population",
				     x = "Metro",
				     y = "Occupation Population") +
				theme_minimal()
		})
		
		output$plot_relative <- renderPlot({
			ggplot(top_relative, aes(x = reorder(Metro, `Relative Size (All MSAs)`), y = `Relative Size (All MSAs)`)) +
				geom_bar(stat = "identity", fill = "darkgreen") +
				coord_flip() +  # Flip to make the plot horizontal
				labs(title = "Top 50 Metros by Relative Metro Occupation",
				     x = "Metro",
				     y = "Relative Metro Occupation") +
				theme_minimal()
		})
	})
}


# Run the application


shinyApp(ui = ui, server = server)
