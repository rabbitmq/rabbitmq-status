DEPS:=rabbitmq-mochiweb rabbitmq-server rabbitmq-erlang-client

TEST_APPS=crypto inets mochiweb rabbit_mochiweb rabbit_status
TEST_ARGS=-rabbit_mochiweb port 55672
START_RABBIT_IN_TESTS=true

ERLTL_DIR:=$(PACKAGE_DIR)/erltl
ERLTL_EBIN_DIR:=$(ERLTL_DIR)/ebin
ERLTL_SRC_DIR:=$(ERLTL_DIR)/src

EBIN_DIR:=$(PACKAGE_DIR)/ebin
SOURCE_DIR:=$(PACKAGE_DIR)/src
EBIN_BEAMS:=$$(patsubst $$($(PACKAGE_DIR)_SOURCE_DIR)/%.erl,$$($(PACKAGE_DIR)_EBIN_DIR)/%.beam,$$($(PACKAGE_DIR)_SOURCE_ERLS)) $$($(PACKAGE_DIR)_EBIN_DIR)/template.beam

$(ERLTL_EBIN_DIR)/%.beam: $(ERLTL_SRC_DIR)/%.erl
	@mkdir -p $(ERLTL_EBIN_DIR)
	$(ERLC) -o $(ERLTL_EBIN_DIR) -Wall +debug_info $<

$(EBIN_DIR)/template.beam: $(SOURCE_DIR)/template.et $(ERLTL_EBIN_DIR)/erltl.beam
	$(ERL) -I -pa $(ERLTL_EBIN_DIR) -noshell -eval 'erltl:compile("$<", [{outdir, "$(@D)"}, report_errors, report_warnings, nowarn_unused_vars]), halt().'

clean::
	rm -f $(ERLTL_EBIN_DIR)/*.beam

include ../include.mk

