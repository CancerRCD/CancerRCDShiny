library(rsconnect)

# Caminho do app
rsconnect::deployApp(appDir = getwd(), appName = "cancerrcdshiny")

# Se precisar fazer mudanças no código e reenviar o app, basta rodar:
rsconnect::deployApp()

# Verificar instalações
rsconnect::applications()


