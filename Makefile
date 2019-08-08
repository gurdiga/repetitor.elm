default: compile

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
	git commit --allow-empty -am 'Deploy'
	git push

e: edit
edit:
	mine .

TUTOR_PAGES = $(wildcard src/Tutor/Pages/*Page.elm)

compile: elm.json compile-tutor-pages
compile-tutor-pages:
	@for module_path in $(TUTOR_PAGES); do \
		echo "$$module_path"; \
		elm make $$module_path --output=/dev/null || exit 1; \
	done

build: elm.json build-tutor-pages

build-tutor-pages:
	@for module_path in $(TUTOR_PAGES); do \
		elm_module_name=`head -n1 $$module_path | grep -Po '(?<=module )[A-Za-z0-9.]+'`; \
		path_name=`grep -Po '(?<=-- path name: )[a-z]+' $$module_path` || \
			{ echo "\nModule $$module_path does not have a path name.\n"; exit 1; }; \
		echo "$$module_path -> $$path_name"; \
		sed "s/ELM_MODULE_NAME/$$elm_module_name/" docs/page-template.html > docs/$$path_name/index.html; \
		elm make --optimize $$module_path --output=docs/$$path_name/main.js || exit 1; \
	done

elm.json:
	elm init
