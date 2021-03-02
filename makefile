
.PHONY: test
test:
	dune exec --force tests/list_like_test.exe


.PHONY: doc
doc:
	rm -rf _doc
	dune build @doc
	cp -r _build/default/_doc _doc

.PHONY: cover
cover: clean
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

.PHONY: magic
magic:
	grep 'Obj.magic' src/*.ml || echo 'OK'

.PHONY: nonrec
nonrec:
	grep 'rec' src/*_internal.ml || echo 'OK'

