
.PHONY: test
test:
	dune exec --force tests/list_like_test.exe

.PHONY: bench
bench:
	dune exec --force tests/bench_list.exe


.PHONY: doc
doc: odoc.css
	rm -rf _doc
	dune build @doc
	cp -r _build/default/_doc _doc
	[ -f odoc.css ] && cp -f odoc.css _doc/_html/odoc.css

.PHONY: cover
cover: clean
	dune exec --force --instrument-with bisect_ppx tests/list_like_test.exe
	dune exec --force --instrument-with bisect_ppx tests/dequeue_test.exe
	dune exec --force --instrument-with bisect_ppx tests/steque_test.exe
	dune exec --force --instrument-with bisect_ppx tests/deck_test.exe
	bisect-ppx-report html
	bisect-ppx-report summary

.PHONY: clean
clean:
	dune clean
	rm -rf _doc
	rm -rf _coverage
	rm -f bisect*.coverage

.PHONY: width80
width80:
	find . -name '*.ml' | grep -v _build | xargs grep --color -E -e '^.{80,}| $$' \
		|| echo 'OK'

.PHONY: check
check:
	grep 'Obj.magic' src/*.ml || echo 'no magic'
	grep 'rec' src/*_internal.ml || echo 'no recursion'

.PHONY: knuth_plass
knuth_plass:
	dune exec examples/knuth_plass.exe

.PHONY: ngrams
ngrams:
	dune exec examples/ngrams.exe

.PHONY: string_builder
string_builder:
	dune exec examples/string_builder.exe

