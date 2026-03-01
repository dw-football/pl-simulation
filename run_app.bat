@echo off
set RSTUDIO_PANDOC=C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools
taskkill /f /im Rscript.exe /t 2>nul
timeout /t 2 /nobreak
start "" "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" -e "setwd('G:/My Drive/Computing/R/pl-simulation'); shiny::runApp('app.R', port=3838, launch.browser=FALSE)"
timeout /t 5 /nobreak
start chrome http://127.0.0.1:3838
