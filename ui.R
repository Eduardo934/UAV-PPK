source("global.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(title = "UAV-PPK",
                    tags$li(class="dropdown",div(class= "btn-auth",
                                                 shinyauthr::logoutUI(id = "logout", label = "Exit")),
                            
                    )
                    ),
    dashboardSidebar(
        disable = TRUE
    ),
    dashboardBody(
        tags$head(
            tags$link(rel="shortcut icon", href="favicon-16x16.png")
        ),
        # add login panel UI function
        shinyauthr::loginUI(id = "login", title= div(img(src="SG Logo.png", align="center", style="width:60%; height:60%;"),
                                                     h2("Log In")),
                                cookie_expiry = cookie_expiry),
        
        uiOutput("panelPrincipal"),
        
        HTML('<footer style="background-color:#e3e3e3; height:50px; width:100%;margin-top:100px;">
                      <p>&copy; 2022 Soluciones Geográficas - all rights reserved</p>
                    </footer>')
    
    )
))
