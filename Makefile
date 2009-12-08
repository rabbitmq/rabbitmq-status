PACKAGE=rabbit_status
DEPS=rabbitmq-mochiweb rabbitmq-server rabbitmq-erlang-client

TEST_APPS=mochiweb rabbit_mochiweb rabbit_status 
TEST_ARGS=-rabbit_mochiweb port 55672
START_RABBIT_IN_TESTS=true


GENERATED_SOURCES=template-ghost
include ../include.mk


$(SOURCE_DIR)/template-ghost.erl: $(SOURCE_DIR)/template.et  $(EBIN_DIR)/erltl.beam
	echo "-module(templateghost)." > $(SOURCE_DIR)/template-ghost.erl
	$(ERL) -I -pa ebin -noshell  -eval 'erltl:compile("$(SOURCE_DIR)/template.et"), halt().'
	mv $(SOURCE_DIR)/template.beam $(EBIN_DIR)

