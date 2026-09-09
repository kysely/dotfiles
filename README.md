# dotfiles

Personal dotfiles and bootstrap scripts.

## Install

```sh
./install
```

This installs packages, installs LazyVim into `~/.config/nvim`, stows dotfiles into `$HOME`, wires `~/.zshrc` to load `~/.zshrc.dotfiles`, and sets zsh as the default shell.

On macOS, install Homebrew manually first from https://brew.sh/.

## Pi packages

`./install` (or `./install-packages`) installs Pi and then installs
[`pi-mcp-adapter`](https://pi.dev/packages/pi-mcp-adapter) globally for the current
user. Add more `pi install <source>` commands to `install_pi_packages` in
`install-packages` to include other Pi packages.

Pi manages the package list in `~/.pi/agent/settings.json`, which stays untracked;
existing settings are preserved. No `sudo` or project-local `-l` flag is used.

To install just the adapter on an existing machine:

```sh
pi install npm:pi-mcp-adapter
```

Restart Pi after installation, then use `/mcp setup` to configure MCP servers.
