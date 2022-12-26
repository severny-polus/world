all: elm.js
	cp elm.js elm.min.js

debug: elm.js_unoptimized

release: elm.min.js

elm.js: $(wildcard src/*)
	elm make src/Main.elm --optimize --output=$@

elm.min.js: elm.js
	uglifyjs $^ --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output $@

elm.js_unoptimized: $(wildcard src/*)
	elm make src/Main.elm --output=elm.min.js

geodata = $(wildcard geodata/*.min.geo.json)

minify: $(geodata)

$(geodata): %.min.geo.json: %.geo.json

%.min.geo.json:
	jq -c . < $^ > $@

clean:
	rm elm.js elm.min.js

