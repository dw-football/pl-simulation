@echo off
REM --- pl-simulation launcher (self-healing, off-Drive) ---
REM Sit down at ANY machine and just run: pulls latest code, syncs the
REM R package library to renv.lock, then launches the Shiny app.

set PROJ=C:\Users\dwarren\src\pl-simulation

REM Pandoc (for any Quarto render) -- probe both RStudio install locations.
if exist "C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools\pandoc.exe" (
    set RSTUDIO_PANDOC=C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools
) else (
    set RSTUDIO_PANDOC=%LOCALAPPDATA%\Programs\RStudio\resources\app\bin\quarto\bin\tools
)

cd /d "%PROJ%"

REM Layer 1: pull latest (best-effort -- run local code even if offline).
git pull --ff-only 2>nul

REM Stop any running instance.
taskkill /f /im Rscript.exe /t 2>nul
timeout /t 2 /nobreak >nul

REM NOTE: a machine-level R_PROFILE_USER (-> G:/Computing/R/.Rprofile) hijacks the
REM profile slot and setwd()s elsewhere, so renv's auto-activator never runs. We
REM therefore activate renv EXPLICITLY: setwd into the project, source activate.R
REM (pins .libPaths to the renv.lock library), then self-heal via renv::restore().
start "" Rscript.exe -e "setwd('%PROJ:\=/%'); source('renv/activate.R'); renv::restore(prompt = FALSE); shiny::runApp('app.R', port=3838, launch.browser=FALSE)"
timeout /t 5 /nobreak >nul
start chrome http://127.0.0.1:3838
