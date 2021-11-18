source("global.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(title = "UAV-PPK",
                    tags$li(class="dropdown",div(class= "btn-auth",
                                                 shinyauthr::logoutUI(id = "logout", label = "Salir")),
                            
                    )
                    ),
    dashboardSidebar(
        disable = TRUE
    ),
    dashboardBody(
        # add login panel UI function
        shinyauthr::loginUI(id = "login", cookie_expiry = cookie_expiry,
                            title = "Iniciar Sesión",
                            user_title = "Nombre de Usuario",
                            pass_title = "Contraseña",
                            login_title = "Log in",
                            error_message = "Nombre de usuario o contraseña incorrectas!"
                            ),
        
        uiOutput("panelPrincipal"),
        
        
    )
))
