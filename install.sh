#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' 

clone_and_enter_repo() {
    local repo_url="https://github.com/mathvis/mvscript.git"  
    local temp_dir="$HOME/.mvscript-tmp"
    
    if [ -d "$temp_dir" ]; then
        print_status "Using existing temp directory: $temp_dir"
        cd "$temp_dir" || exit 1
        git pull origin main
    else
        print_status "Cloning project repository..."
        git clone "$repo_url" "$temp_dir"
        cd "$temp_dir" || exit 1
    fi

}



print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

detect_os() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if command_exists apt-get; then
            echo "ubuntu"
        elif command_exists dnf; then
            echo "fedora"
        elif command_exists pacman; then
            echo "arch"
        else
            echo "linux"
        fi
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
        echo "windows"
    else
        echo "unknown"
    fi
}

install_system_deps() {
    local os=$(detect_os)
    print_status "Detected OS: $os"
    
    case $os in
        "ubuntu")
            print_status "Installing system dependencies for Ubuntu/Debian..."
            sudo apt-get update
            sudo apt-get install -y build-essential libffi-dev libgmp-dev zlib1g-dev
            ;;
        "fedora")
            print_status "Installing system dependencies for CentOS/RHEL..."
            sudo dnf group install -y "Development Tools"
            sudo dnf install -y gmp-devel zlib-devel
            ;;
        "arch")
            print_status "Installing system dependencies for Arch Linux..."
            sudo pacman -S --noconfirm base-devel gmp zlib
            ;;
        "macos")
            print_status "Installing system dependencies for macOS..."
            if ! command_exists brew; then
                print_status "Installing Homebrew..."
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            fi
            brew install curl
            if ! xcode-select -p >/dev/null 2>&1; then
                print_status "Installing Xcode command line tools..."
                xcode-select --install
            fi
            ;;
        "windows")
            print_warning "Windows detected. Please ensure you have the following installed:"
            print_warning "- Git for Windows or WSL"
            print_warning "- Visual Studio Build Tools or Visual Studio with C++ support"
            print_warning "Consider using WSL for better Haskell development experience"
            ;;
        *)
            print_warning "Unknown OS. Please install build tools manually:"
            print_warning "- curl"
            print_warning "- C compiler (gcc/clang)"
            print_warning "- Build tools (make, etc.)"
            print_warning "- Development libraries (gmp, zlib, ffi)"
            ;;
    esac
}

install_haskell_tools() {
    print_status "Installing GHC and Cabal..."
    
    case $os in
        "ubuntu")
            print_status "Installing Haskell Tools for Ubuntu/Debian..."
            sudo apt-get install -y ghc cabal-install
            ;;
        "dnf")
            print_status "Installing Haskell Tools for Fedora/RedHat..."
            sudo dnf install -y ghc cabal-install
            ;;
        "arch")
            print_status "Installing Haskell Tools for Arch Linux..."
            sudo pacman -S --noconfirm ghc cabal-install
            ;;
        "macos")
            print_status "Installing Haskell Tools for macOS..."
            brew install ghc cabal-install
            ;;
        "windows")
            print_status "Installing Haskell Tools for Windows..."
            winget install GHC.GHC
            winget install --id=haskell.cabal  -e
        *)
            print_warning "Unknown OS. Please install Haskell tools manually:"
            print_warning "- ghc"
            print_warning "- cabal"
            ;;
    esac
    print_status "Updating Cabal package database..."
    cabal update
}

setup_project() {
    print_status "Setting up project dependencies..."
    
    if [ ! -f "mvscript.cabal" ]; then
        print_error "mvscript.cabal not found. Please run this script from the project root directory."
        exit 1
    fi
    
    print_status "Installing Haskell dependencies..."
    cabal build --dependencies-only
    
    print_status "Building the project..."
    cabal build
    
    print_success "Project setup complete!"
}

install_executable() {
    print_status "Installing executable to ~/.local/bin..."
    
    mkdir -p ~/.local/bin
    mkdir -p ~/.mvscc
    cp ./config.toml ~/.mvscc/config.toml
    
    cabal install --install-method=copy --overwrite-policy=always --installdir="$HOME/.local/bin"
    
    if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
        print_warning "~/.local/bin is not in your PATH"
        print_status "Add the following line to your shell configuration file:"
        print_status "  export PATH=\"\$HOME/.local/bin:\$PATH\""
        echo
        print_status "For bash: echo 'export PATH=\"\$HOME/.local/bin:\$PATH\"' >> ~/.bashrc"
        print_status "For zsh:  echo 'export PATH=\"\$HOME/.local/bin:\$PATH\"' >> ~/.zshrc"
        print_status "Then restart your shell or run: source ~/.bashrc (or ~/.zshrc)"
    else
        print_success "~/.local/bin is already in your PATH"
    fi
    
    if [ -f "$HOME/.local/bin/mvscc" ]; then
        print_success "Executable 'mvscc' installed successfully to ~/.local/bin"
    else
        print_error "Failed to install executable"
        return 1
    fi
}

verify_installation() {
    print_status "Verifying installation..."
    
    local all_good=true
    
    if command_exists ghc; then
        local ghc_version=$(ghc --version)
        print_success "GHC: $ghc_version"
    else
        print_error "GHC not found"
        all_good=false
    fi
    
    if command_exists cabal; then
        local cabal_version=$(cabal --version | head -n1)
        print_success "Cabal: $cabal_version"
    else
        print_error "Cabal not found"
        all_good=false
    fi
    
    if cabal build >/dev/null 2>&1; then
        print_success "Project builds successfully"
    else
        print_error "Project build failed"
        all_good=false
    fi
    
    if [ -f "$HOME/.local/bin/mvscc" ]; then
        print_success "Executable 'mvscc' is installed in ~/.local/bin"
        
        if command_exists mvscc; then
            print_success "mvscc is available in PATH"
        else
            print_warning "mvscc is not in PATH (add ~/.local/bin to PATH)"
        fi
    else
        print_warning "Executable not installed to ~/.local/bin"
    fi
    
    if [ "$all_good" = true ]; then
        print_success "All components installed and working correctly!"
        echo
        print_status "You can now compile using mvscc <file> [config]"
    else
        print_error "Some components failed to install. Please check the errors above."
        exit 1
    fi
}

show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo "Install all dependencies for the mvscript Haskell project"
    echo
    echo "Options:"
    echo "  --help, -h          Show this help message"
    echo "  --skip-system       Skip system dependency installation"
    echo "  --skip-haskell      Skip Haskell toolchain installation"
    echo "  --skip-install      Skip installing executable to ~/.local/bin"
    echo "  --verify-only       Only verify existing installation"
    echo
}

main() {
    local skip_system=false
    local skip_haskell=false
    local skip_install=false
    local verify_only=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_usage
                exit 0
                ;;
            --skip-system)
                skip_system=true
                shift
                ;;
            --skip-haskell)
                skip_haskell=true
                shift
                ;;
            --skip-python)
                skip_python=true
                shift
                ;;
            --skip-install)
                skip_install=true
                shift
                ;;
            --verify-only)
                verify_only=true
                shift
                ;;
            *)
                print_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    print_status "Starting mvscript dependency installation..."
    echo
    
    if [ "$verify_only" = true ]; then
        verify_installation
        exit 0
    fi

        
    if [ "$skip_system" = false ]; then
        install_system_deps
        echo
    fi
    
    if [ "$skip_haskell" = false ]; then
        install_haskell_tools
        echo
    fi
    
    clone_and_enter_repo

    setup_project
    echo
    
    if [ "$skip_install" = false ]; then
        install_executable
        echo
    fi
    
    verify_installation

    print_status "Cleaning up temporary files..."
    rm -rf "$HOME/.mvscript-tmp"
}

main "$@"
