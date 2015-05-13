### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

row <- function(...) {
  tags$div(class="row", ...)
}

col <- function(width, ...) {
  tags$div(class=paste0("span", width), ...)
}

actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(bootstrapPage(
	tags$head(
		tags$link(rel='stylesheet', type='text/css', href='styles.css'),
		tags$script(type='text/javascript', src='scripties.js')
	),

	tags$div(
		class = "container",

		tags$p(tags$br()),
		row(
			col(12, 
				h2('LabulatoR'), 
		      	h4('Filter Room Chaos'),
		      	br()
			)
		),

		tabsetPanel(
			tabPanel("RH and Temp Plot",
				row(dygraphOutput("temp")),
				row(dygraphOutput("rh"))
			),
			
			tabPanel("Table",
				row(
					col(12,
						p(id='tableTitle',strong("Data Table"), a("hide", id='hide_ce', onclick='false'), downloadLink("downloadFull", class='export')),
						dataTableOutput('allDataTable')
					)
				)
			),
			tabPanel("Violations Summary",
				row(
					col(12,
						p(id='tableTitle',strong("Violations"), a("hide", id='hide_ce', onclick='false')),
						dataTableOutput('summaryDataTable')
					)
				)
			)
		),

		row(
			col(12, 
				p(id="colophon",class='text-center',"Though this be madness, yet there is method in't.")
			)
		)
	)
))

