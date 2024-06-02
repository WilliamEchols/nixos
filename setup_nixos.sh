#!/bin/bash

set -e

REPO_URL="https://github.com/williamechols/nixos"
LOCAL_DIR="$HOME/Desktop/nixos"
HOST_NAME="default"

echo "Starting NixOS configuration setup..."

# Install git if not installed
# nix-env -iA nixos.git

if [ ! -d "$LOCAL_DIR" ]; then
    echo "Cloning the NixOS flake repository..."
    git clone $REPO_URL $LOCAL_DIR
else
    echo "Repository directory already exists. Pulling latest changes..."
    cd $LOCAL_DIR
    git pull
fi

cd $LOCALIZE_DIR

echo "Applying NixOS configuration for host $HOST_NAME..."
sudo nixos-rebuild switch --flake .#$HOST_NAME

echo "NixOS configuration has been successfully updated"

