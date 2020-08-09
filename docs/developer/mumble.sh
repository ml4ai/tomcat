#!/usr/bin/env bash

# Script to install and launch Mumble.

set -eu pipefail

if [[ ! "$OSTYPE" == "darwin"* ]]; then
    echo "This script currently only works on macOS. Exiting now."
    exit 1
fi

# Check to see if Mumble is already installed.
if [[ ! -d /Applications/Mumble.app ]]; then
    # Check to see if Mumble-1.3.2.dmg exists in the current directory.
    if [[ ! -f Mumble-1.3.2.dmg ]]; then
        echo "Downloading Mumble-1.3.2.dmg..."
        curl -LO https://dl.mumble.info/stable/Mumble-1.3.2.dmg
    fi

    # If the .dmg file exists in the current directory, then mount it using
    # hdiutil.
    if [[ -f Mumble-1.3.2.dmg ]]; then
        echo "Mounting the Mumble disk image..."
        hdiutil attach Mumble-1.3.2.dmg
    fi

    # Check to see if the disk image is mounted.
    if [[ -d "/Volumes/Mumble 1.3.2" ]]; then
        echo "Copying Mumble.app to /Applications..."
        # Copy Mumble.app to the /Applications
        cp -R "/Volumes/Mumble 1.3.2/Mumble.app" /Applications

        # Unmount the disk image.
        echo "Unmounting the Mumble disk image..."
        hdiutil unmount "/Volumes/Mumble 1.3.2"
    fi

    # Remove the disk image file once the installation is complete
    echo "Finished installing Mumble. Removing Mumble-1.3.2.dmg..."
    /bin/rm Mumble-1.3.2.dmg

    echo "Mumble installation complete!"
fi

# Launch the Mumble app
echo "Launching Mumble."
open -a Mumble.app
exit 0
