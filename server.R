source("global.R")


#Server
shinyServer(function(input, output, session) {
    
    ## Valores reactivos para guardar los usuarios 
    usuarios<- reactiveValues()
    ##Lee los usuarios del archivo temporal
    user_base <- readRDS("user_base.rds")
    ##Guada los usuarios del archivo temporal en los valores reactivos
    usuarios$datos <- user_base
    
    ## Lee las credenciales y las sesiones
    credentials <- callModule(
        shinyauthr::login,
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        sodium_hashed = TRUE,
        log_out = reactive(logout_init())
    )

    ## Llama a módulo de logout desde la paquetería shinyauthr
    logout_init <- callModule(
        shinyauthr::logout,
        id = "logout",
        active = reactive(credentials()$user_auth)
    )
    
    
    user_data <- reactive({
        credentials()$info
    })
    
    ## Crea la interfaz dependiendo de las credenciales de usuario
    
    output$panelPrincipal<- renderUI(
        expr = if(!is.null(user_data()$permissions) && user_data()$permissions %in% "admin"){
            
            req(credentials()$user_auth)
            req(user_data()$permissions %in% "admin")
            
            fluidRow(
                box( 
                    status = "primary",
                    width = 11,
                    
                    
                    div(style="align-content: center;",
                        column(width = 12,
                               h2("Usuarios"),
                               dataTableOutput("user_table"),
                               br(),
                               br(),
                               fluidRow(
                                   column(width = 2, actionButton("eliminar", label = "Eliminar",  class = "btn-new")),
                                   column(width = 2, actionButton("nuevo", label = "Nuevo",  class = "btn-new"))
                               )
                        )
                    )
                )
            )
            
                
        } else if(!is.null(user_data()$permissions) && user_data()$permissions %in% "standard") {
            
            fluidRow(
                box(
                    status = "primary",
                    solidHeader = TRUE,
                    width = 2,
                    title = "Controls",
                    radioButtons("modo", label = "Select Model UAV",
                                 choices = list("PHANTOM 4RTK" = "rtk4", "KIT PPK PHANTOM 4 - REACH" = "reach"), 
                                 selected = "rtk4"),
                    uiOutput("controlEventsOrMrk"),
                    fileInput("posFile", label = "File (.pos)"),
                    actionButton("muestraArchivos", "Show"),
                    uiOutput("mostrarResultados"),
                    uiOutput("botonDescarga")
                
                ),
                tabBox(
                    title = "Interpolation",
                    width = 10,
                    height = "1100px",
                    id = "tabset1",
                    tabPanel("Data", 
                             div(
                                 h4("Files"),
                                 div(
                                     box( 
                                         status = "primary",
                                         width = 6,
                                         div(
                                             h4("File (.pos)"),
                                             dataTableOutput("tablaRinex")
                                         )
                                     ),
                                     
                                     box(
                                         status = "primary",
                                         width = 6,
                                         div(
                                             uiOutput("fileTitle"),
                                             dataTableOutput("tablaMrk")
                                         )
                                     )
                                     
                                 )
                                 
                                 )
                             ),
                    tabPanel("Results", 
                             div(
                                 div(
                                     box( 
                                         status = "primary",
                                         width = 12,
                                         div(
                                             leafletOutput("mapa")
                                         )
                                     ),
                                     
                                     box(
                                         status = "primary",
                                         width = 12,
                                         div(
                                             dataTableOutput("tablaResultados")
                                         )
                                     )
                                     
                                 )
                                 
                             )
                             )
                )
            )
        
        } else {
            NULL
        }
    )
    
    
    ### Crea la tabla para visualizar usuarios
    output$user_table <- renderDataTable({
        
        # use req to only render results when credentials()$user_auth is TRUE
        req(credentials()$user_auth)
        req(user_data()$permissions %in% "admin")
    
        data <- usuarios$datos
        
        datatable(data[,c("user", "permissions", "name")], options = list(
            pageLength = 10,
            scrollX = TRUE
        ))
    })
    
    ### Ventana emergente para introducir datos de neuvo usuario
    observeEvent(input$nuevo,{
        showModal(
            modalDialog(
                title = "Nuevo usuario",
                fluidRow(
                    column(width = 12,
                           textInput("usuario", label = h4("Usuario")),
                           passwordInput("password",label = h4("Constraseña")),
                           radioButtons("permisos", label = h4("Permiso"),
                                        choiceNames = c("Usuario","Administrador"),
                                        choiceValues = c("standard","admin")
                           ),
                           textInput("nombre",label = h4("Nombres"))
                    )
                ),
                easyClose = FALSE,
                footer = tagList(
                    actionButton("cancelar","Cancel"),
                    actionButton("guardar","Guardar")
                )
            )
        )
    })
    
    ## Cierra cualquier modal al momento de dar cancelar
    observeEvent(input$cancelar,{
        removeModal()
    })
    
    ## Modifica el archivo temporal donde están guardados los usuarios
    observeEvent(input$guardar,{
        req(credentials()$user_auth)
        req(user_data()$permissions %in% "admin")
        
        if(nchar(input$usuario)<1 || nchar(input$password)<1 ||
           nchar(input$nombre)<1){
            showNotification(
                h4("Debes completar todos los campos"), 
                action = NULL, duration = 5, type = "warning")
        } else {
            
            usuarios_base<-readRDS("user_base.rds")
            
            if(length(which(usuarios_base$user %in% input$usuario))>0){
                showNotification(
                    h4("El nombre de usuario ya existe"), 
                    action = NULL, duration = 5, type = "warning")   
            } else {
                nuevoUsuario<- c(input$usuario, password_store(input$password), input$permisos, input$nombre)
                
                nuevaTabla<- rbind(usuarios_base, nuevoUsuario)
                
                saveRDS(nuevaTabla, "user_base.rds")
                
                usuarios$datos <- readRDS("user_base.rds")
                
                removeModal()
                
                showNotification(
                    h4("Creación exitosa"), 
                    action = NULL, duration = 5, type = "message")
                
                
            }
            
        }
    })
    
    ####Valores reactivos que guardaran info de las selecciones en la tabla
    selecciones_tabla<- reactiveValues()
    #### Ventana emergente para advertir de que se van a eliminar usuarios
    observeEvent(input$eliminar,{
        
        if(length(input$user_table_rows_selected)>0){
            showModal(
                modalDialog(title = "Borrar",
                            fluidPage(column(12,h3("Cuidado: Estás a punto de borrar usuarios de la base de datos"),style="color:red;")),
                            easyClose = FALSE,
                            size = "m",
                            footer = tagList(
                                actionButton("cancelar","Cancel"),
                                actionButton("borrar_usuario","Eliminar")
                            ) 
                )
            )
        } else {
            showNotification(
                h4("Selecciona un renglón"), 
                action = NULL, duration = 5, type = "warning") 
        }
        
    })
    
    observeEvent(input$borrar_usuario,{
        req(credentials()$user_auth)
        req(user_data()$permissions %in% "admin")
        
        selecciones_tabla$renglon<-input$user_table_rows_selected
        
        usuarios_base<-readRDS("user_base.rds")
        
        nuevaTabla<-usuarios_base[- selecciones_tabla$renglon,]
        
        saveRDS(nuevaTabla, "user_base.rds")
        
        usuarios$datos <- readRDS("user_base.rds")
        
        removeModal()
        
        showNotification(
            h4("Usuario eliminado con éxito"), 
            action = NULL, duration = 5, type = "message")
        
           
    })
    
    
    ## Panel generado al escoger uno de los dos modos de interolación
    output$controlEventsOrMrk <- renderUI(
        expr = if(input$modo %in% "rtk4"){
            div(
                fileInput("mrkOrEventsFile", label = "Mrk File (.MRK)")
            )
        } else {
            div(
                fileInput("mrkOrEventsFile", label = "Events File (.pos)"),
                numericInput("defaultNumber", "Indicate time delay (seconds)", value = 0.3)
            )
        }
    )
    
    ##Título generado por el tipo de archivo
    output$fileTitle<- renderUI(expr = if(input$modo %in% "rtk4"){
        h4("File (.Mrk)")
    } else {
        h4("Events File (.pos)")
    })
    
    ##Valores reactivos que guardaran datos de los archivos y de los resultados
    datos<- reactiveValues()
    
    observeEvent(input$muestraArchivos,{
        req(input$posFile)
        req(input$mrkOrEventsFile)
        
        #### Se asegura que los archivos tengan la extensión correcta
        if(input$modo %in% "rtk4"){
            #### Se asegura que los archivos tengan la extensión correcta
            if(length(grep(".pos",input$posFile )) %in% 1 || length(grep(".MRK",input$mrkOrEventsFile )) %in% 0){
                showNotification(
                    h4("Make sure files are  .pos for Rinex and .mrk for MRK"), 
                    action = NULL, duration = 5, type = "warning")
                return()
            }
            
            ## Lee el archivo .MRK directamente
            datos$archivoMrkOrEvents<-read.table(file= input$mrkOrEventsFile$datapath)
            
        } else if(input$modo %in% "reach"){
            #### Se asegura que los archivos tengan la extensión correcta
            if(length(grep(".pos",input$posFile )) %in% 1 || length(grep(".pos",input$mrkOrEventsFile )) %in% 0){
                showNotification(
                    h4("Make sure both files are .pos"), 
                    action = NULL, duration = 5, type = "warning")
                return()
            }
            ## Elle el archivo event
            ####lee el archivo
            archivoEventsEncabezado<- read.delim(file= input$mrkOrEventsFile$datapath, sep="\t", header = FALSE)
            ###Encuentra el renglón donde empieza el encabezado
            for(i in 1:30){
                if(length(grep("latitude",  archivoEventsEncabezado[i,1],fixed = TRUE)) %in% 0){
                    i = i +1
                }else{
                    ren_encabezado = i ## Encuentra el renglón donde empiezan los datos
                    break;
                }
            }
            #########lee de nuevo el archivo pero ahora tomando en cuenta el encabezado para leer solo la tabla y lo guarda en un valor reactivo
            datos$archivoMrkOrEvents<-read.table(file= input$mrkOrEventsFile$datapath, skip = i, header = F)
            
            
        }
        

        
        ############################Lee el primer archivo pos
        ##Lee el archivo .pos pero se asegura de que el encabezado sea el correcto y de leer el punto de control
        archivoPosEncabezado<- read.delim(file= input$posFile$datapath, sep="\t", header = FALSE)
        
        ### Busca el encabezado en el archivo .pos
        #######Solo busca en los primeros 50 renglone ara evitar un loop infinito
        for(i in 1:30){
            if(length(grep("latitude",  archivoPosEncabezado[i,1],fixed = TRUE)) %in% 0){
                i = i +1
            }else{
                ren_encabezado = i ## Encuentra el renglón donde empiezan los datos
                break;
            }
        }
        #########lee de nuevo el archivo pero ahora tomando en cuenta el encabezado para leer solo la tabla y lo guarda en un valor reactivo
        datos$archivoPos<-read.table(file= input$posFile$datapath, skip = i-1, header = T)
        
        
        ### Busca el renglón donde se enceuntra el punto de referencia (ref pos)
        #######Solo busca en los primeros 50 renglone ara evitar un loop infinito
        for(i in 1:30){
            if(length(grep("ref pos",  archivoPosEncabezado[i,1],fixed = TRUE)) %in% 0){
                i = i +1
            }else{
                renglonReferencia = i ## Encuentra el renglón donde está el punto de referncia
                break;
            }
        }
        puntoReferencia<- as.character(archivoPosEncabezado[renglonReferencia,1]) %>%
            strsplit( ":") %>%
            unlist()
        
        coordenadasReferencia<-as.character(puntoReferencia[2]) %>%
            strsplit("[| ]+") %>%
            unlist() %>%
            as.numeric()
        
        ####Guarda las coordenadas del punto de referencia dentro de los valores reactivos
        datos$coordenadasReferencia<-coordenadasReferencia
        
        
        
        
        ### Solo se asegura de que este preproceso se realice antes de calcular la interpolación
        datos$preProceso <- TRUE
    })
    
    ## Renderiza la tabla rinex y la tabla Mrk con los datos cargados por los usuarios
    output$tablaRinex<- renderDataTable({
        req(datos$archivoPos)
        
        datatable(datos$archivoPos, options = list(
            pageLength = 15,
            scrollX = TRUE
        ))
          
    })
    
    output$tablaMrk<- renderDataTable({
        req(datos$archivoMrkOrEvents)
        
        datatable(datos$archivoMrkOrEvents, options = list(
            pageLength = 15,
            scrollX = TRUE
        ))
        
    })
    
    
    
    #############Renderiza mapa
    
    output$mapa <- renderLeaflet({
        
        mapa<-leaflet() %>% 
            addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap.Mapnik","Esri.WorldImagery"),
                options = layersControlOptions(collapsed = TRUE)
            )
        
        
        mapa%>%addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#3D535D",
            completedColor = "#7D4479",
            localization = "es"
            ) %>%
            addControl(actionButton("reset","", icon= icon("sync-alt")),position="bottomright")
    })
    
    ### Mapa proxy
    map_proxy<-leaflet::leafletProxy("mapa")
    
    
    
    
    ############ depliega boton para hace el cálculo
    output$mostrarResultados<- renderUI(expr = if(!is.null(datos$archivoMrkOrEvents) && !is.null(datos$archivoPos)){
        req(datos$preProceso %in% TRUE)
        div(
            h3("Interpolation"),
            actionButton("iniciar", "Start", class = "btn-success")
        )
    })
    
    output$botonDescarga<- renderUI(expr = if(!is.null(datos$resultados)){
        div(
            h3("Download results"),
            downloadButton("descargaInterpolacionCsv", label = "Download(.csv)"),
            br(),
            downloadButton("descargaInterpolacionTxt", label = "Download(.txt)"),
            br(),
            downloadButton("descargaPointsKML", label = "Download KML")
        )
    } else {
        NULL
    })
    
    
    output$namingPanel <- renderUI(expr = if(input$modo %in% "reach"){
        fluidPage(
            column(12,
                   h4("Name of the first image"),
                   textInput("carpetImagenes", label="", value="img")
            ))
    } else {
        fluidPage(
            column(12,
                   h4("Name of Directory"),
                   textInput("carpetImagenes", label="", value="img")
            ))
    })
    observeEvent(input$iniciar,{
        showModal(
            modalDialog(title = "Interpolation",
                        uiOutput("namingPanel"),
                        easyClose = FALSE,
                        size = "m",
                        footer = tagList(
                            actionButton("cancelar","Cancel"),
                            actionButton("interpolar","Start", class = "btn-success")
                        ) 
            )
        )
    })
    
    observeEvent(input$interpolar, {
        
        req(datos$archivoPos)
        req(datos$archivoMrkOrEvents)
        req(input$carpetImagenes)
        
        ## Cambia a la página de resultados
        updateTabsetPanel(session, "tabset1",
                          selected = "Results")
        
        removeModal()
        
        
        rinex <- as.data.frame(datos$archivoPos)
        
        MrkorEvents <- datos$archivoMrkOrEvents
        if(input$modo %in% "reach"){
            MrkorEvents$V2 <- MrkorEvents$V2 + input$defaultNumber
        }
        mrk <- as.data.frame(MrkorEvents)
        
        ### Asegurarse de que los nombres de las columnas sean los correctos para poder manejar las columnas
        
        
        #Se crean matrices con los nombres de nuestros dato para despu?s agragarlas como columnas al data frame final
        gpst <- matrix(nrow = nrow(mrk), ncol = 1)
        referencia_inf <- matrix(nrow = nrow(mrk), ncol = 1)
        referencia_sup <- matrix(nrow = nrow(mrk), ncol = 1)
        valor_inf <- matrix(nrow = nrow(mrk), ncol = 1)
        valor_sup <- matrix(nrow = nrow(mrk), ncol = 1)
        peso_inf <- matrix(nrow = nrow(mrk), ncol = 1)
        peso_sup <- matrix(nrow = nrow(mrk), ncol = 1)
        latitude <- matrix(nrow = nrow(mrk), ncol = 1)
        longitude <- matrix(nrow = nrow(mrk), ncol = 1)
        height <- matrix(nrow = nrow(mrk), ncol = 1)
        sdn <- matrix(nrow = nrow(mrk), ncol = 1)
        sde <- matrix(nrow = nrow(mrk), ncol = 1)
        sdu <- matrix(nrow = nrow(mrk), ncol = 1)
        
        #Se obtiene el valor m?nimo de nuestros datos, as? como el intervalo en el que varian
        
        
        for (i in 1:nrow(mrk)){
            #Se encuntran los valores superior e inferior 
            valor_inf_valor <- tail(rinex$GPST[rinex$GPST<mrk$V2[i]], n=1)
            valor_sup_valor <- rinex$GPST[rinex$GPST>mrk$V2[i]][1]
            
            #Con los valores se obtienen las referencias o indices
            
            referencia_inf_valor <- which(rinex$GPST==valor_inf_valor)
            referencia_sup_valor <- which(rinex$GPST==valor_sup_valor)
            
            #print(valor_inf_valor)
            #print(mrk$V2[i])
            #print(valor_sup_valor)
            
            
            #Con los valores superior e inferior se obtienen los pesos inferior y superior
            peso_inf_valor <- (valor_sup_valor-mrk$V2[i])/(valor_sup_valor - valor_inf_valor)
            peso_sup_valor <- (mrk$V2[i]-valor_inf_valor)/(valor_sup_valor - valor_inf_valor)
            
            #print(peso_inf_valor)
            #print(peso_sup_valor)
            
            #Con los pesos se obitiene los demas datos y se les asignan a sus respectivas matrices
            latitude_valor <- rinex$latitude.deg.[referencia_inf_valor]*peso_inf_valor+rinex$latitude.deg.[referencia_sup_valor]*peso_sup_valor
            longitude_valor <- rinex$longitude.deg.[referencia_inf_valor]*peso_inf_valor+rinex$longitude.deg.[referencia_sup_valor]*peso_sup_valor
            height_valor <- rinex$height.m.[referencia_inf_valor]*peso_inf_valor+rinex$height.m.[referencia_sup_valor]*peso_sup_valor
            sdn_valor <- rinex$sdn.m.[referencia_inf_valor]*peso_inf_valor+rinex$sdn.m.[referencia_sup_valor]*peso_sup_valor
            sde_valor <- rinex$sde.m.[referencia_inf_valor]*peso_inf_valor+rinex$sde.m.[referencia_sup_valor]*peso_sup_valor
            sdu_valor <- rinex$sdu.m.[referencia_inf_valor]*peso_inf_valor+rinex$sdu.m.[referencia_sup_valor]*peso_sup_valor
            
            
            #print(longitude_valor)
            #print(height_valor)
            gpst[i] <- mrk$V2[i]
            referencia_inf[i] <- referencia_inf_valor
            referencia_sup[i] <- referencia_sup_valor
            valor_inf[i] <- valor_inf_valor
            valor_sup[i] <- valor_sup_valor
            peso_inf[i] <- peso_inf_valor
            peso_sup[i] <- peso_sup_valor
            latitude[i] <- latitude_valor
            longitude[i] <- longitude_valor
            height[i] <- height_valor
            sdn[i] <- sdn_valor
            sde[i] <- sde_valor
            sdu[i] <- sdu_valor
            
        }
        
        resultados <- data.frame(gpst, referencia_inf, referencia_sup, valor_inf, valor_sup, peso_inf, peso_sup, latitude, longitude, height, sdn, sde, sdu)
         
        ###Nombra los puntos de acuerdo a la carpeta o al nombre de la primera imagen de acuerdo al
        #tipo de proceso
        if(input$modo %in% "rtk4"){
            resultados$referencias <- sprintf("%04d", seq(1:nrow(resultados))) %>%
                paste(input$carpetImagenes,"_", .,".JPG", sep="")
        } else {
            if(input$modo %in% "reach"){
                ## Busca números en el nombre de la imágen para iniciar el conteo
                fotoN <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$carpetImagenes))  
                
                if(is.na(fotoN)){
                    fotoN <- 1
                }
                
                resultados$referencias <- sprintf("%04d", rep(seq(1:999),20)[c(fotoN:(fotoN+nrow(resultados)-1))]) %>%
                    paste(gsub('[[:digit:]]+', '', input$carpetImagenes), .,".JPG", sep="")
            }
        }
        
        
        
        # Se guarda en los valores reactivos
        datos$resultados <- resultados
        
        #### Renderiza el mapa
        ###### crea el ícono del punto de referencia
        
        #####Crea el vector de colores
        PrecMedia <- (datos$resultados$sdn + datos$resultados$sde)/ 2
        coloresMapa <- ifelse(PrecMedia <= 0.015, "#00FF00", 
                              ifelse( 0.015 < PrecMedia|| PrecMedia<= 0.03, "#EEC718",
                                      ifelse(PrecMedia > 0.03, "#FF4000")
                                      )
                              )
        
        map_proxy %>% 
            clearShapes() %>%
            clearMarkers() %>%
            addCircleMarkers(lng = datos$resultados$longitude, lat = datos$resultados$latitude, 
                             color = coloresMapa,group="Flight points",
                             popup = paste("Reference: ", datos$resultados$referencias, "<br>",
                                           "Latitude: ",datos$resultados$latitude,"<br>",
                                           "Longitude: ",datos$resultados$longitude,"<br>",
                                           "sdn: ",datos$resultados$sdn, "<br>",
                                           "sde: ", datos$resultados$sde, "<br>",
                                           "sdu: ", datos$resultados$sdu, "<br>")
                             ) %>%
            fitBounds(min(datos$resultados$longitude), min(datos$resultados$latitude), max(datos$resultados$longitude), max(datos$resultados$latitude)) %>%
            addAwesomeMarkers(lng = datos$coordenadasReferencia[2], lat = datos$coordenadasReferencia[1], 
                              group = "Reference point", icon = icon("home"),
                              popup = paste("Reference point", "<br>",
                                            "Latitude: ", datos$coordenadasReferencia[1], "<br>",
                                            "Longitude: ",datos$coordenadasReferencia[2],"<br>",
                                            "Ellipsoidal height: ",datos$coordenadasReferencia[3],"<br>")
            ) %>%
            removeLayersControl()%>%
            addLayersControl(
                baseGroups = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"),
                overlayGroups = c("Flight points", "punto referencia"),
                options = layersControlOptions(collapsed = TRUE)
            )
    })
    
    ###Refresca la pantalla del mapa
    observeEvent(input$reset,{
        req(datos$archivoPos)
        req(datos$archivoMrkOrEvents)
        req(input$carpetImagenes)
        
        #### Renderiza el mapa
        ###### crea el ícono del punto de referencia
        
        #####Crea el vector de colores
        PrecMedia <- (datos$resultados$sdn + datos$resultados$sde)/ 2
        coloresMapa <- ifelse(PrecMedia <= 0.015, "#00FF00", 
                              ifelse( 0.015 < PrecMedia|| PrecMedia<= 0.03, "#EEC718",
                                      ifelse(PrecMedia > 0.03, "#FF4000")
                              )
        )
        
        map_proxy %>% 
            clearShapes() %>%
            clearMarkers() %>%
            addCircleMarkers(lng = datos$resultados$longitude, lat = datos$resultados$latitude, 
                             color = coloresMapa,group="Flight points",
                             popup = paste("Reference: ", datos$resultados$referencias, "<br>",
                                           "Latitude: ",datos$resultados$latitude,"<br>",
                                           "Longitude: ",datos$resultados$longitude,"<br>",
                                           "sdn: ",datos$resultados$sdn, "<br>",
                                           "sde: ", datos$resultados$sde, "<br>",
                                           "sdu: ", datos$resultados$sdu, "<br>")
            ) %>%
            fitBounds(min(datos$resultados$longitude), min(datos$resultados$latitude), max(datos$resultados$longitude), max(datos$resultados$latitude)) %>%
            addAwesomeMarkers(lng = datos$coordenadasReferencia[2], lat = datos$coordenadasReferencia[1], 
                             group = "Reference point", icon = icon("home"),
                             popup = paste("Reference point", "<br>",
                                           "Latitude: ", datos$coordenadasReferencia[1], "<br>",
                                           "Longitude: ",datos$coordenadasReferencia[2],"<br>",
                                           "Ellipsoidal height: ",datos$coordenadasReferencia[3],"<br>")
            ) %>%
            removeLayersControl()%>%
            addLayersControl(
                baseGroups = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"),
                overlayGroups = c("Flight points", "Reference point"),
                options = layersControlOptions(collapsed = TRUE)
            )
    })
    
    output$tablaResultados<- renderDataTable({
        req(datos$resultados)
        datosResultadosTabla <- datos$resultados
        
        
        datatable(datosResultadosTabla[,c("referencias", "gpst", "latitude", "longitude", "height", "sdn", "sde", "sdu")], 
                  colnames=c("References", "gpst", "latitude", "longitude", "height", "sdn", "sde", "sdu"),
                  options = list(
            pageLength = 10,
            scrollX = TRUE
        ))
    })
    
    output$descargaInterpolacionCsv<-  downloadHandler(
        filename = function() {
            gsub(".pos","", input$posFile$name) %>%
            paste(.,"_geoet", ".csv", sep="") 
        },
        content = function(file) {
            datosR<- datos$resultados[,c("referencias", "latitude", "longitude", "height", "sdn", "sde", "sdu")]
            names(datosR) <- c("References", "latitude", "longitude", "height", "sdn", "sde", "sdu")
            write.csv(datosR, file,  row.names = FALSE, quote = FALSE)
        }
    )
    
    output$descargaInterpolacionTxt<- downloadHandler(
        filename = function() {
            gsub(".pos","", input$posFile$name) %>%
                paste(.,"_geoet", ".txt", sep="") 
        },
        content = function(file) {
            datosR<- datos$resultados[,c("referencias", "latitude", "longitude", "height", "sdn", "sde", "sdu")]
            names(datosR) <- c("References", "latitude", "longitude", "height", "sdn", "sde", "sdu")
            
            write.table(datosR, file, sep = ",", row.names = FALSE, quote = FALSE)
        }
    )
    
    
    
    output$descargaPointsKML<- downloadHandler(
        filename = function() {
            gsub(".pos","", input$posFile$name) %>%
                paste(.,"_geoet", ".kml", sep="") 
        },
        content = function(file) {
            datosR<- datos$resultados[,c("referencias", "latitude", "longitude", "height", "sdn", "sde", "sdu")]
            names(datosR) <- c("References", "latitude", "longitude", "height", "sdn", "sde", "sdu")
            
            sfPoints <- st_as_sf(datosR, coords=c("longitude", "latitude"))
            
            st_write(sfPoints, file, driver = "kml", delete_dsn = TRUE)
        }
    )

})
