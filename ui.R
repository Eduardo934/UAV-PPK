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
        # add login panel UI function
        shinyauthr::loginUI(id = "login", cookie_expiry = cookie_expiry),
        
        uiOutput("panelPrincipal"),
        
        
    )
))
