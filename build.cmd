@echo off
cls

IF EXIST "paket.lock" (
  paket restore
) ELSE (
  paket install
)

if errorlevel 1 (
  exit /b %errorlevel%
)
dotnet restore build.proj
dotnet fake build %*
