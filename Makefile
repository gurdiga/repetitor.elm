build: elm.json tutor-pages

tutor-pages: inregistrare autentificare

inregistrare:
	elm make --optimize src/Tutor/Pages/Registration.elm --output=docs/$@/main.js

autentificare:
	elm make --optimize src/Tutor/Pages/Authentication.elm --output=docs/$@/main.js

elm.json:
	elm init

install:
	wget https://github.com/elm/compiler/releases/download/0.19.0/installer-for-mac.pkg
	open installer-for-mac.pkg
	rm installer-for-mac.pkg

SERVER_PORT=8003

open:
	open http://localhost:$(SERVER_PORT)

start:
	elm reactor --port $(SERVER_PORT)


deploy:	build
	git commit -am 'Deploy'
	git push
