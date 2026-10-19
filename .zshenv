# Homebrew exports FPATH via `brew shellenv`. Long-running terminals/tmux can
# retain a Cellar path after `brew upgrade zsh` removes that version. Repair it
# before .zprofile/.zshrc load completions, using Homebrew's stable symlink path.
() {
  local i functions_dir
  for (( i = 1; i <= ${#fpath}; i++ )); do
    [[ "${fpath[i]}" == */Cellar/zsh/*/share/zsh/functions ]] || continue
    functions_dir="${fpath[i]%%/Cellar/zsh/*}/share/zsh/functions"
    if [[ -d "$functions_dir" ]]; then
      fpath[i]="$functions_dir"
    fi
  done
}
