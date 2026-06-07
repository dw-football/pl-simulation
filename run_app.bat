@echo off
if exist "C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools\pandoc.exe" (
    set RSTUDIO_PANDOC=C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools
) else (
    set RSTUDIO_PANDOC=%LOCALAPPDATA%\Programs\RStudio\resources\app\bin\quarto\bin\tools
)
taskkill /f /im Rscript.exe /t 2>nul
timeout /t 2 /nobreak
start "" Rscript.exe -e ".libPaths(c('C:/Users/dwarren/AppData/Local/R/win-library/4.5', .libPaths())); setwd('G:/Soccer/pl-simulation'); shiny::runApp('app.R', port=3838, launch.browser=FALSE)"
timeout /t 5 /nobreak
start chrome http://127.0.0.1:3838