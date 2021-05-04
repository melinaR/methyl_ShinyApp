#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

load(file = "../../data/small_data.Rdata")
Index_seq = small_data$index
Y = small_data$Y
N = nrow(Y)
K = ncol(Y)
sourceAll(path="../../code/R_functions/")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
#### SET DATA ####
    
     res_fmn <- reactiveValues(Y = matrix(0,N,K), time = NA, RMSE = NA, R2 = NA)
     res_fmp <- reactiveValues(Y = matrix(0,N,K), time = NA, RMSE = NA, R2 = NA)
     res_fmpb <- reactiveValues(Y = matrix(0,N,K), time = NA, RMSE = NA, R2 = NA)
     res_fgasp <- reactiveValues(Y = matrix(0,N,K), time = NA, RMSE = NA, R2 = NA)
     global_param <- reactiveValues(rank = "2")
     set_data <- reactiveValues(obj_toy = set_obj(Y, K, 1, N, 100))
     shinyjs::disable("run_fmp")
     shinyjs::disable("run_fmpb")
     
     
#### put data to NA when change param ####
     observeEvent(input$run_plot, {
         set_data$obj_toy = set_obj(Y, K, as.numeric(input$kstar), N, as.numeric(input$nstar))
         res_fmn$Y = matrix(0,N,K); res_fmn$time = NA; res_fmn$RMSE = NA; res_fmn$R2 = NA
         res_fmp$Y = matrix(0,N,K); res_fmp$time = NA; res_fmp$RMSE = NA; res_fmp$R2 = NA
         res_fmpb$Y = matrix(0,N,K); res_fmpb$time = NA; res_fmpb$RMSE = NA; res_fmpb$R2 = NA
         res_fgasp$Y = matrix(0,N,K); res_fgasp$time = NA; res_fgasp$RMSE = NA; res_fgasp$R2 = NA
         shinyjs::disable("run_fmpb")
         shinyjs::disable("run_fmp")
     })

     observeEvent(input$run_rank, {
         res_fmn$Y = matrix(0,N,K); res_fmn$time = NA; res_fmn$RMSE = NA; res_fmn$R2 = NA
         res_fmp$Y = matrix(0,N,K); res_fmp$time = NA; res_fmp$RMSE = NA; res_fmp$R2 = NA
         res_fmpb$Y = matrix(0,N,K); res_fmpb$time = NA; res_fmpb$RMSE = NA; res_fmpb$R2 = NA
         #res_fgasp$Y = matrix(0,N,K); res_fgasp$time = NA; res_fgasp$RMSE = NA; res_fgasp$R2 = NA
         global_param$rank = input$rank
     })


     observeEvent(input$make_matlab, {
         withProgress(message = 'Construction data fmp et fmpb en cours',value = 0, {obj_toy = set_data$obj_toy
         make_matlab(obj_toy)})
         shinyjs::enable("run_fmp")
     })

     output$param_all = renderText({
         obj_toy = set_data$obj_toy
         paste(c(paste("nstar =",input$nstar),
           paste("kstar =", paste(set_data$obj_toy$index_kstar,collapse = " et "))
         ),collapse = "\n")
     })

     #### outputUI ####
     output$secondSelection = renderUI({
         obj_toy = set_data$obj_toy
         selectInput("kplot", label = "Echantillon observé",
                     choices = obj_toy$index_kstar, selected = obj_toy$index_kstar[1])
     })

     #### Plot ####
    output$visu <- renderPlotly({
        obj_toy = set_data$obj_toy
        Y = obj_toy$Y
        s = 1:nrow(Y)
        fig <- plot_ly(x = s, y = Y[,1], type = 'scatter', mode = 'lines', name = "Echantillon 1")
        for ( i in 2:K)
        {
            fig <- fig  %>% add_trace(y =Y[,i], name = paste("Echantillon",i), mode = 'lines')
        }
        index_remove = obj_toy$index_nstar
        #Yr = Y[,obj_toy$index_kstar[1]]
        #Yr[-index_remove] = NA
        #fig <- fig  %>% add_trace(y = Yr, name = 'Points tests', mode = 'markers')
        fig <- fig %>% layout(title = paste("Echantillons test : ",paste(obj_toy$index_kstar, collapse = " et ")))
        fig
    })

    output$visuMat <- renderPlot({
        obj_toy = set_data$obj_toy
        im_Y = matrix(FALSE,nrow(obj_toy$Y), ncol(obj_toy$Y))
        im_Y[obj_toy$index_nstar,obj_toy$index_kstar] = TRUE
        x = raster(ncol=K, nrow=20, xmn=0, xmx=K, ymn=0, ymx=20)
        values(x) = as.vector(t(im_Y[1:20,]))
        image(x, col=c('white', 'black'),  xaxt = "n",yaxt = "n",
              xlab = paste("Indice échantillons cibles : ",paste(obj_toy$index_kstar,collapse = " et ")),
              ylab = paste(input$nstar,"lignes en moins"),
              main = "Schéma matrice \n(20 première lignes)")
        abline(h = c(1:19))
        abline(v = 1:K)
    },height = 400,width = 200)




    #### FMN ####

    observeEvent(input$run_fmn, {
        obj_toy = set_data$obj_toy
        withProgress(message = 'FMN en cours',value = 0, {
            res_fmn$time = system.time({

                param = as.numeric(strsplit(input$par_fmn,",")[[1]])
                res_fmn$Y = fmn(obj_toy,as.numeric(global_param$rank),alpha = param[1:3], beta = param[4:6])
                res_fmn$Y[res_fmn$Y>1] = 1
                res_fmn$Y[res_fmn$Y<0] = 0
            })
        })
        res_fmn$RMSE = RMSE(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
             res_fmn$Y[obj_toy$index_nstar,obj_toy$index_kstar])
        res_fmn$R2 = R2(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                            res_fmn$Y[obj_toy$index_nstar,obj_toy$index_kstar])
    })

    observeEvent(input$run_fmn,{
        output$plotres_fmn = renderPlot({
            obj_toy = set_data$obj_toy
            kplot = as.numeric(input$kplot)
            plot(obj_toy$Y[obj_toy$index_nstar,kplot],
                 res_fmn$Y[obj_toy$index_nstar,kplot],
                 xlab = "Y_true",
                 ylab = 'Y_pred',
                 main = "FMN")
            abline(a = 0,b=1, col="red")
        })
    })



    #### FMP ####

    observeEvent(input$run_fmp, {
        obj_toy = set_data$obj_toy
        withProgress(message = 'FMP en cours',value = 0, {
                param_fmp = as.numeric(strsplit(input$par_fmp,",")[[1]])
                data = "toy_example.mat"
                num_feat = as.numeric(global_param$rank)
                save_epoch = 10
                maxepoch  = param_fmp[2]
                numbatches = 1
                lambda = param_fmp[1]
                epsilon = 10
                momentum = 0.8

                res_fmp$res = fmp(data,
                              num_feat,
                              save_epoch,
                              maxepoch,
                              numbatches,
                              lambda,
                              epsilon,
                              momentum)
                res_fmp$Y = with(res_fmp$res, w1.M1 %*% t(w1.P1))
                res_fmp$Y[res_fmp$Y>1] = 1
                res_fmp$Y[res_fmp$Y<0] = 0
                res_fmp$time = res_fmp$res$time
        })
        res_fmp$RMSE = RMSE(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                            res_fmp$Y[obj_toy$index_nstar,obj_toy$index_kstar])
        res_fmp$R2 = R2(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                        res_fmp$Y[obj_toy$index_nstar,obj_toy$index_kstar])
        shinyjs::enable("run_fmpb")
    })

    observeEvent(input$run_fmp, {
        output$plotres_fmp = renderPlot({
            obj_toy = set_data$obj_toy
            kplot = as.numeric(input$kplot)
            plot(obj_toy$Y[obj_toy$index_nstar,kplot],
                 res_fmp$Y[obj_toy$index_nstar,kplot],
                 xlab = "Y_true",
                 ylab = 'Y_pred',
                 main = "FMN")
            abline(a = 0,b=1, col="red")
        })
    })

    observeEvent(input$run_fmp, {
        output$out_fmp <- renderPlotly({
            obj_toy = set_data$obj_toy
            err = res_fmp$res$err.train
            s = 1:length(err)
            fig <- plot_ly(x = s, y = err, type = 'scatter', mode = 'lines', name = "RMSE_train")
            err = res_fmp$res$err.valid
            fig <- fig  %>% add_trace(y = err, name = "RMSE_test", mode = 'lines')
            fig <- fig %>% layout(title =
                                      paste("Evolution RMSE au cours des itérations"))
            fig
        })
    })




    #### FMPB ####

    observeEvent(input$run_fmpb, {
        obj_toy = set_data$obj_toy
        withProgress(message = 'FMPB en cours',value = 0, {
            param_fmpb = as.numeric(strsplit(input$par_fmpb,",")[[1]])
            data ="toy_example.mat"
            w1_M1_sample = res_fmp$res$w1.M1
            w1_P1_sample = res_fmp$res$w1.P1
            num_feat = as.numeric(global_param$rank)
            maxgibbs = param_fmpb[1]
            savegibbs = 10
            burnin = param_fmpb[2]
            beta = param_fmpb[3]
            b0_u = param_fmpb[4]
            b0_m = param_fmpb[4]


            res_fmpb$res = fmpb(data ,
                            w1_M1_sample,
                            w1_P1_sample,
                            num_feat,
                            maxgibbs ,
                            savegibbs,
                            burnin,
                            beta,
                            b0_u,
                            b0_m )
            res_fmpb$Y = with(res_fmpb$res, w1.M1.sample %*% t(w1.P1.sample))
            res_fmpb$Y[res_fmpb$Y>1] = 1
            res_fmpb$Y[res_fmpb$Y<0] = 0
            res_fmpb$time = res_fmpb$res$time
        })
        res_fmpb$RMSE = RMSE(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                            res_fmpb$Y[obj_toy$index_nstar,obj_toy$index_kstar])
        res_fmpb$R2 = R2(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                        res_fmpb$Y[obj_toy$index_nstar,obj_toy$index_kstar])
    })

    observeEvent(input$run_fmpb, {
        output$plotres_fmpb = renderPlot({
        obj_toy = set_data$obj_toy
       kplot = as.numeric(input$kplot)
        plot(obj_toy$Y[obj_toy$index_nstar,kplot],
             res_fmpb$Y[obj_toy$index_nstar,kplot],
             xlab = "Y_true",
             ylab = 'Y_pred',
             main = "FMPB")
        abline(a = 0,b=1, col="red")
    })
    })

    observeEvent(input$run_fmpb, {
        output$out_fmpb <- renderPlotly({
        err = res_fmpb$res$err.test
        s = 1:length(err)
        fig <- plot_ly(x = s, y = err, type = 'scatter', mode = 'lines', name = "RMSE_test")
        fig <- fig %>% layout(title =
                                  paste("Evolution RMSE gibbs sampling"))
        fig
    })
        })

    #### FGASP ####

    observeEvent(input$run_fgasp, {
        obj_toy = set_data$obj_toy
        withProgress(message = 'FGASP en cours',value = 0, {
            res_fgasp$time = system.time({
                obj_fgasp = MEF_fgasp(obj_toy,Index_seq)
                res_fgasp$Y = pred_fastgasp(obj_fgasp,obj_toy)
            })
        })
        res_fgasp$RMSE = RMSE(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                              res_fgasp$Y[obj_toy$index_nstar,obj_toy$index_kstar])
        res_fgasp$R2 = R2(obj_toy$Y[obj_toy$index_nstar,obj_toy$index_kstar],
                          res_fgasp$Y[obj_toy$index_nstar,obj_toy$index_kstar])
    })

    observeEvent(input$run_fgasp, {
        output$plotres_fgasp = renderPlot({
        obj_toy = set_data$obj_toy
        kplot = as.numeric(input$kplot)
        plot(obj_toy$Y[obj_toy$index_nstar,kplot],
             res_fgasp$Y[obj_toy$index_nstar,kplot],
             xlab = "Y_true",
             ylab = 'Y_pred',
             main = "FGASP")
        abline(a = 0,b=1, col="red")
    })
    })


    #### All####


    output$out_all = renderTable({
        tbl = data.frame(temps = rep(NA,4),
                         RMSE = rep(NA,4),
                         R2 = rep(NA,4),
                         Rang = global_param$rank)
        tbl[1,1:3] = c(res_fmn$time[3], res_fmn$RMSE,res_fmn$R2)
        tbl[2,1:3] = c(res_fmp$time, res_fmp$RMSE,res_fmp$R2)
        tbl[3,1:3] = c(res_fmpb$time, res_fmpb$RMSE,res_fmpb$R2)
        tbl[4,1:3] = c(res_fgasp$time[3], res_fgasp$RMSE,res_fgasp$R2)
        rownames(tbl) = c("fmn","fmp","fmpb","fgasp")
        tbl

    },rownames = TRUE,digits = 4)


})
