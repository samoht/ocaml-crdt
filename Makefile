all: ocp-build.root
	ocp-build build

ocp-build.root:
	ocp-build root
	ocp-build configure

_obuild/crdt-test/crdt-test.asm: all
	@

test: _obuild/crdt-test/crdt-test.asm
	OCAMLRUNPARAM=b _obuild/crdt-test/crdt-test.asm
