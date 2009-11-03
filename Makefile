PACKAGE=rabbit_status
DEPS=rabbitmq-mochiweb rabbitmq-server rabbitmq-erlang-client

TEST_APPS=mochiweb rabbit_mochiweb rabbit_status 
START_RABBIT_IN_TESTS=true


GENERATED_SOURCES=template-ghost
include ../include.mk


$(SOURCE_DIR)/template-ghost.erl: $(SOURCE_DIR)/template.et  $(EBIN_DIR)/erltl.beam
	echo "-module(templateghost)." > src/template-ghost.erl
	@mkdir -p $(EBIN_DIR)
	$(ERL) -I -pa ebin -noshell  -eval 'erltl:compile("src/template.et"), halt().'
	mv $(SOURCE_DIR)/template.beam $(EBIN_DIR)

