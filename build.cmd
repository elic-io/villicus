echo installing needed dotnet version
dotnet-install.ps1 -JSonFile global.json

echo Restoring dotnet tools...
dotnet tool restore

dotnet fake build -t %*
