all: elm.js flags.js
	cp elm.js elm.min.js

debug: elm.js_unoptimized flags.js

release: elm.min.js flags.js

elm.js: $(wildcard src/*)
	elm make src/Main.elm --optimize --output=$@

elm.min.js: elm.js
	uglifyjs $^ --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output $@

elm.js_unoptimized: $(wildcard src/*)
	elm make src/Main.elm --output=elm.min.js

geodata = landWithoutAntarctica landAntarctica

flags.js: $(foreach name,$(geodata),geodata/$(name).geo.json)
	echo 'const flags = {' > $@
	for name in $(geodata); do \
  		echo "  $$name: \`" >> $@ && \
		jq -c . < geodata/$$name.geo.json >> $@ && \
		echo "  \`," >> $@; \
  	done
	echo '  end: ""' >> $@
	echo '}' >> $@

clean:
	rm elm.js elm.min.js flags.js

