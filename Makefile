OCAMLMAKEFILE = OCamlMakefile

PACKS=str extlib xml-light
LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a caml-aiml.cma caml-aiml.cmxa

SOURCES= aiml_base.ml aiml_predicate.ml aiml_pattern.ml aiml_templ.ml aiml_topic.ml aiml_main.ml aiml_nodemapper.ml aiml_brain.ml aiml_test.ml
RESULT  = caml-aiml

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES_ML)

all : ncl bcl
include $(OCAMLMAKEFILE)
