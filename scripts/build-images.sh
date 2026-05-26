#!/usr/bin/env bash
set -euo pipefail

# Build all cardano-toa Docker images and tag them with the last commit hash.
# Usage:
#   scripts/build-images.sh            # uses last commit short hash
#   scripts/build-images.sh v1.2.3     # or specify an explicit tag
#
# Env:
#   IMAGE_PREFIX   Docker registry/user prefix (default: mariusgeorgescu)
#   PUSH           If "true", build base + service images multi-arch and push to
#                  the registry. Otherwise build for the host arch only and load
#                  into the local docker daemon.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

TAG="${1:-}"
if [[ -z "${TAG}" ]]; then
  TAG="$(git -C "${ROOT_DIR}" rev-parse --short HEAD)"
fi

IMAGE_PREFIX="${IMAGE_PREFIX:-mariusgeorgescu}"
PUSH="${PUSH:-false}"

BUILDER_VERSION="9.6.6"
RUNTIME_TAG="3.20"
BUILDER_IMAGE="${IMAGE_PREFIX}/toa-builder:${BUILDER_VERSION}"
RUNTIME_IMAGE="${IMAGE_PREFIX}/toa-runtime-base:${RUNTIME_TAG}"

echo "Building images with tag: ${TAG}"
echo "Image prefix: ${IMAGE_PREFIX}"

if [[ "${PUSH}" == "true" ]]; then
  BUILDX_FLAGS=(--platform "linux/amd64,linux/arm64" --push)
  echo "Push mode: building multi-arch (linux/amd64,linux/arm64) and pushing to registry"
else
  ARCH="$(uname -m)"
  case "${ARCH}" in
    x86_64|amd64) NATIVE_PLATFORM="linux/amd64" ;;
    arm64|aarch64) NATIVE_PLATFORM="linux/arm64" ;;
    *) echo "Unsupported host arch: ${ARCH}" >&2; exit 1 ;;
  esac
  BUILDX_FLAGS=(--platform "${NATIVE_PLATFORM}" --load)
  echo "Local mode: building for ${NATIVE_PLATFORM} and loading locally"
fi

cd "${ROOT_DIR}"

# Build shared builder base (toolchain + cached deps; stable across source changes)
echo "Building shared builder base: ${BUILDER_IMAGE}"
docker buildx build \
  "${BUILDX_FLAGS[@]}" \
  -f Dockerfile.base \
  -t "${BUILDER_IMAGE}" \
  .

# Build shared runtime base (crypto libs; stable unless versions change)
echo "Building shared runtime base: ${RUNTIME_IMAGE}"
docker buildx build \
  "${BUILDX_FLAGS[@]}" \
  -f Dockerfile.runtime-base \
  -t "${RUNTIME_IMAGE}" \
  .

# Interaction API
docker buildx build \
  "${BUILDX_FLAGS[@]}" \
  -f Dockerfile.interaction-api \
  -t "${IMAGE_PREFIX}/toa-interaction-api:${TAG}" \
  .

echo ""
if [[ "${PUSH}" == "true" ]]; then
  echo "Pushed images:"
else
  echo "Built images:"
fi
echo "  ${BUILDER_IMAGE}"
echo "  ${RUNTIME_IMAGE}"
echo "  ${IMAGE_PREFIX}/toa-interaction-api:${TAG}"
