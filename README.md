# Tic-Tac-Toe Backend

## Create Docker image

```bash
docker build -t tictactoe .
```

## Run Docker

```bash
docker run -d -p 5000:5000 -e PORT=5000 tictactoe
```