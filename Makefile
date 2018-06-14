REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

COMPOSE_HTTP_TIMEOUT := 300
export COMPOSE_HTTP_TIMEOUT

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := wapi
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service_erlang
BASE_IMAGE_TAG := 16e2b3ef17e5fdefac8554ced9c2c74e5c6e9e11

BUILD_IMAGE_TAG := 562313697353c29d4b34fb081a8b70e8c2207134

CALL_ANYWHERE := \
	submodules \
	all compile xref lint dialyze test cover \
	start devrel release clean distclean \
	swag_server.generate swag_client.generate \
	generate regenerate swag_server.regenerate swag_client.regenerate

CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER) all

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

generate: swag_server.generate #swag_client.generate

regenerate: swag_server.regenerate #swag_client.regenerate

compile: submodules generate
	$(REBAR) compile

xref:
	$(REBAR) xref

lint: generate
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: submodules distclean generate
	$(REBAR) as prod release

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean: swag_server.distclean swag_client.distclean
	$(REBAR) clean
	rm -rf _build

cover:
	$(REBAR) cover

# CALL_W_CONTAINER
test: submodules generate
	$(REBAR) do eunit, ct

# Swagger stuff
SWAGGER_CODEGEN = $(call which, swagger-codegen)
SWAGGER_SCHEME_BASE_PATH := schemes/swag

WAPI_SRC = src

$(SWAGGER_SCHEME): $(SWAGGER_SCHEME_BASE_PATH)/.git

# Swagger server

# wallet

SWAGGER_SCHEME_WALLET_PATH := $(SWAGGER_SCHEME_BASE_PATH)/api/wallet
SWAGGER_SCHEME_WALLET := $(SWAGGER_SCHEME_WALLET_PATH)/swagger.yaml

SWAG_SERVER_WALLET_PREFIX := swag_wallet_server
# SWAG_SERVER_APP_PATH := apps/$(SWAG_SERVER_PREFIX)
SWAG_SERVER_WALLET_APP_PATH := $(SWAG_SERVER_WALLET_PREFIX)
SWAG_SERVER_WALLET_APP_TARGET := $(SWAG_SERVER_WALLET_APP_PATH)/rebar.config

SWAG_SERVER_WALLET_SRC := $(WAPI_SRC)
SWAG_SERVER_WALLET_SRC_TARGET := $(SWAG_SERVER_WALLET_SRC)/$(SWAG_SERVER_WALLET_PREFIX).erl

$(SWAG_SERVER_WALLET_SRC_TARGET): $(SWAG_SERVER_WALLET_APP_TARGET)
	cp $(SWAG_SERVER_WALLET_APP_PATH)/src/*.erl $(SWAG_SERVER_WALLET_SRC)/
	rm -rf $(SWAG_SERVER_WALLET_APP_PATH)

$(SWAG_SERVER_WALLET_APP_TARGET): $(SWAGGER_SCHEME_WALLET)
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_WALLET) \
		-l erlang-server \
		-o $(SWAG_SERVER_WALLET_APP_PATH) \
		--additional-properties \
			packageName=$(SWAG_SERVER_WALLET_PREFIX)

# payres

SWAGGER_SCHEME_PAYRES_PATH := $(SWAGGER_SCHEME_BASE_PATH)/api/payres
SWAGGER_SCHEME_PAYRES := $(SWAGGER_SCHEME_PAYRES_PATH)/swagger.yaml


SWAG_SERVER_PAYRES_PREFIX := swag_payres_server
# SWAG_SERVER_APP_PATH := apps/$(SWAG_SERVER_PREFIX)
SWAG_SERVER_PAYRES_APP_PATH := $(SWAG_SERVER_PAYRES_PREFIX)
SWAG_SERVER_PAYRES_APP_TARGET := $(SWAG_SERVER_PAYRES_APP_PATH)/rebar.config

SWAG_SERVER_PAYRES_SRC := $(WAPI_SRC)
SWAG_SERVER_PAYRES_SRC_TARGET := $(SWAG_SERVER_PAYRES_SRC)/$(SWAG_SERVER_PAYRES_PREFIX).erl

$(SWAG_SERVER_PAYRES_SRC_TARGET): $(SWAG_SERVER_PAYRES_APP_TARGET)
	cp $(SWAG_SERVER_PAYRES_APP_PATH)/src/*.erl $(SWAG_SERVER_PAYRES_SRC)/
	rm -rf $(SWAG_SERVER_PAYRES_APP_PATH)

$(SWAG_SERVER_PAYRES_APP_TARGET): $(SWAGGER_SCHEME_PAYRES)
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_PAYRES) \
		-l erlang-server \
		-o $(SWAG_SERVER_PAYRES_APP_PATH) \
		--additional-properties \
			packageName=$(SWAG_SERVER_PAYRES_PREFIX)

# privdoc

SWAGGER_SCHEME_PRIVDOC_PATH := $(SWAGGER_SCHEME_BASE_PATH)/api/privdoc
SWAGGER_SCHEME_PRIVDOC := $(SWAGGER_SCHEME_PRIVDOC_PATH)/swagger.yaml


SWAG_SERVER_PRIVDOC_PREFIX := swag_privdoc_server
# SWAG_SERVER_APP_PATH := apps/$(SWAG_SERVER_PREFIX)
SWAG_SERVER_PRIVDOC_APP_PATH := $(SWAG_SERVER_PRIVDOC_PREFIX)
SWAG_SERVER_PRIVDOC_APP_TARGET := $(SWAG_SERVER_PRIVDOC_APP_PATH)/rebar.config

SWAG_SERVER_PRIVDOC_SRC := $(WAPI_SRC)
SWAG_SERVER_PRIVDOC_SRC_TARGET := $(SWAG_SERVER_PRIVDOC_SRC)/$(SWAG_SERVER_PRIVDOC_PREFIX).erl

$(SWAG_SERVER_PRIVDOC_SRC_TARGET): $(SWAG_SERVER_PRIVDOC_APP_TARGET)
	cp $(SWAG_SERVER_PRIVDOC_APP_PATH)/src/*.erl $(SWAG_SERVER_PRIVDOC_SRC)/
	rm -rf $(SWAG_SERVER_PRIVDOC_APP_PATH)

$(SWAG_SERVER_PRIVDOC_APP_TARGET): $(SWAGGER_SCHEME_PRIVDOC)
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_PRIVDOC) \
		-l erlang-server \
		-o $(SWAG_SERVER_PRIVDOC_APP_PATH) \
		--additional-properties \
			packageName=$(SWAG_SERVER_PRIVDOC_PREFIX)

swag_server.generate: $(SWAG_SERVER_WALLET_SRC_TARGET) $(SWAG_SERVER_PAYRES_SRC_TARGET) $(SWAG_SERVER_PRIVDOC_SRC_TARGET)

swag_server.distclean:
	rm -rf $(SWAG_SERVER_WALLET_SRC)/$(SWAG_SERVER_WALLET_PREFIX)*.erl
	rm -rf $(SWAG_SERVER_PAYRES_SRC)/$(SWAG_SERVER_PAYRES_PREFIX)*.erl
	rm -rf $(SWAG_SERVER_PRIVDOC_SRC)/$(SWAG_SERVER_PRIVDOC_PREFIX)*.erl

swag_server.regenerate: swag_server.distclean swag_server.generate

# Not in use yet

# Swagger client
SWAG_CLIENT_PREFIX := swag_wallet_client
# SWAG_CLIENT_APP_PATH := apps/$(SWAG_CLIENT_PREFIX)
SWAG_CLIENT_APP_PATH := $(SWAG_CLIENT_PREFIX)
SWAG_CLIENT_APP_TARGET := $(SWAG_CLIENT_APP_PATH)/rebar.config


SWAG_CLIENT_SRC := $(WAPI_SRC)
SWAG_CLIENT_SRC_TARGET := $(SWAG_CLIENT_PREFIX).erl

swag_client.generate: $(SWAG_CLIENT_SRC_TARGET)

swag_client.distclean:
	rm -rf $(SWAG_CLIENT_SRC)/$(SWAG_CLIENT_PREFIX)*.erl

swag_client.regenerate: swag_client.distclean swag_client.generate

$(SWAG_CLIENT_SRC_TARGET): $(SWAG_CLIENT_APP_TARGET)
	cp $(SWAG_CLIENT_APP_PATH)/src/*.erl $(SWAG_CLIENT_SRC)/
	rm -rf $(SWAG_CLIENT_APP_PATH)

$(SWAG_CLIENT_APP_TARGET): $(SWAGGER_SCHEME)
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME) \
		-l erlang-client \
		-o $(SWAG_CLIENT_APP_PATH) \
		--additional-properties \
			packageName=$(SWAG_CLIENT_PREFIX)
