## OCC_app1.2

## Where is your job?

ACS_2022<-read.csv("ACS PUMS/usa_00011.csv")
app_table<-ACS_2022[,c("OCC","Metro","PERWT")]

install.packages("shiny")
library(shiny)
library(dplyr)

# User Interface
ui<-fluidPage(
	titlePanel("Occupation by Metro Area"),
	sidebarLayout(
		sidebarPanel(
			textInput("occ_input", "Enter Occupation Code", ""),
			actionButton("submit","Submit")
		),
		mainPanel(
			tableOutput("table")
		)
	)
)

# Define server logic
# Prototype with just numeric Occ codes

server<-function(input,output){
	observeEvent(input$submit, {
		occ_code<-as.numeric(input$occ_input)
		
		print(paste("Entered occupation code:", occ_code))
		
		if (is.na(occ_code)) {
			output$table <- renderTable({
				data.frame(Message = "Please enter a valid numeric occupation code.")
			})
			return()  # Stop execution if the input is invalid
		}
		
		### RETURN DATA		
		
		Metro_labor_force<-app_table %>%
			group_by(Metro) %>%
			summarize(metro_labor_force = sum(PERWT, na.rm=TRUE))
		
		Met_occupation<-app_table %>%
			filter(OCC==occ_code) %>%
			group_by(Metro)
		summarize(met_occupation = sum(PERWT,na.rm=TRUE)) %>%
			
			filtered_data<-merge(Metro_labor_force, Met_occupation, by="Metro", all.x=TRUE)
		
		nat_occupation<-sum(app_table$PERWT[app_table$OCC==occ_code],na.rm=TRUE)	
		nat_labor_force<-sum(app_table$PERWT[app_table$OCC>0],na.rm=TRUE)
		
		filtered_data<-filtered_data %>%
			mutate(
				Relative_metro_occupation = (met_occupation/metro_labor_force)/
					(nat_occupation/nat_labor_force)
			) %>%
			arrange(desc(Relative_metro_occupation))
		
		if (nrow(filtered_data) == 0) {
			output$table <- renderTable({
				data.frame(Message = "No data found for this occupation code.")
			})
		} else {
			# Display the filtered data in a table
			output$table <- renderTable({
				filtered_data
			})
		}
	})
}


# Run the application

shinyApp(ui = ui, server = server)