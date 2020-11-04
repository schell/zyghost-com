#!/bin/sh -eu

echo "Rust Setup"

if hash rustup 2>/dev/null; then
    echo "Have rustup, skipping installation..."
else
    echo "Installing rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
fi

. $HOME/.cargo/env
rustup default stable

# AWS CLI
if hash aws 2>/dev/null; then
    echo "Have aws cli, skipping installation"
else
    echo "Installing aws cli"
    curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
    unzip awscliv2.zip
    sudo ./aws/install
    echo "  installed aws into:"
    which aws
fi

# TERRAFORM
INSTALL_TERRAFORM="1"
VER="0.13.5"
if hash terraform 2>/dev/null; then
    echo "Terraform is already installed at:"
    WHERE=`which terraform`
    echo "  $WHERE"
    INSTALLED_VER=`terraform -version`
    if [ "${INSTALLED_VER}" = "Terraform v${VER}" ]; then
        echo "  ...and the version is the one we want."
        export INSTALL_TERRAFORM="0"
    else
        echo "  ...but it's version ${INSTALLED_VER}"
    fi
fi

if [ "${INSTALL_TERRAFORM}" = "1" ]; then
    PKG="terraform_${VER}_linux_amd64"
    DIR="$HOME/.local/bin"
    PREV=`pwd`
    echo "Installing terraform..."
    mkdir -p install-terraform
    cd install-terraform
    echo "  downloading terraform..."
    wget https://releases.hashicorp.com/terraform/$VER/$PKG.zip
    echo "  unzipping..."
    unzip $PKG
    echo "  moving it into place..."
    mv terraform $DIR/
    echo "  cleaning up..."
    rm $PKG.zip
    cd $PREV
    tpath=`which terraform`
    echo "  installed terraform into: ${tpath}"
fi
