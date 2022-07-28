# +-----------------------------------------------------------+
# * Docker tests for the autobuild script
# +-----------------------------------------------------------+

# List of available OS.
DOCKER_OS = $(patsubst %.Dockerfile.in, %, \
		$(notdir $(wildcard docker/templates/*.Dockerfile.in)))

# Arguments to pass to docker build .
DOCKER_BUILD_ARGS = -q

# Advice make not to delete these "intermediate" files.
.PRECIOUS: docker/%.Dockerfile docker/.%.build

.PHONY: all test check docker/build

all: docker/test
test: docker/test
check: docker/test

# Create the Dockerfile
docker/%.Dockerfile: docker/templates/%.Dockerfile.in \
	docker/templates/Dockerfile.common.in
	@echo Creating Dockerfile for target $*
	cat $^ > $@

# Build the Dockerfile
docker/.%.build: docker/%.Dockerfile ../autobuild docker/lib
	@echo Building target $*
	podman image build $(DOCKER_BUILD_ARGS) -t epdfinfo/$* -f $< ../ || exit 0
	touch $@

# Run the Dockerfile
docker/%: docker/.%.build
	@echo Running tests on target $*
	podman container run --rm --name epdfinfo-$* epdfinfo/$*

# Run all Dockerfiles
docker/test: docker/build $(patsubst %, docker/%, $(DOCKER_OS))

# Build all Dockerfiles
docker/build: $(patsubst %, docker/.%.build, $(DOCKER_OS))

docker/.%.clean:
	@echo Cleaning target $*
	podman container rm epdfinfo-$* || exit 0

docker/clean: $(patsubst %, docker/.%.clean, $(DOCKER_OS))

clean: docker/clean
	rm -f -- docker/.[^.]*.build
	rm -f -- docker/*.Dockerfile

print:
	@echo List of Operating systems where server compilation is tested [via Docker]:
	@for os in $(DOCKER_OS); do echo - $$os; done | sort

print-failing: FAILING_OS := $(patsubst %.Dockerfile.in.FAILING, %, \
	$(notdir $(wildcard docker/templates/*.Dockerfile.in.FAILING)))

print-failing:
	@echo List of Operating systems where server compilation is not tested:
	@for os in $(FAILING_OS); do echo - $$os; done | sort
	@echo PLEASE help support your favorite OS! More details at: https://pdftools.wiki/A401543C
