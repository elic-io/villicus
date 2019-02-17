FROM microsoft/dotnet:2.2-aspnetcore-runtime-alpine
COPY /deploy /
WORKDIR /Villicus.Api
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Villicus.Api.dll" ]