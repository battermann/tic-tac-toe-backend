FROM fsharp/fsharp
COPY . .
RUN mono ./.paket/paket.bootstrapper.exe
RUN mono ./.paket/paket.exe restore
RUN mono ./.paket/paket.exe generate-include-scripts
CMD fsharpi --exec src/App.fsx $PORT