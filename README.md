# flit.el

Flit is a faster and less annoying TRAMP. flit.el connects over a custom binary RPC protocol (flitrpc) to the flit server on the remote host.

## Quick Start

Install flit:

```elisp
(use-package flit
  :vc (:url "https://github.com/muirdm/flit.el" :rev :newest)
  :ensure t)
```

If you use EternalTerminal (recommended):

```elisp
(setq flit-connection-methods '((t . :et)))
```

Otherwise for SSH:

```elisp
(setq flit-connection-methods '((t . :ssh)))
```

Then:

```elisp
C-x C-f /flit!somehost/
```

## Requirements

To compile the flit binary, you must have Go installed on your local host.

## Warnings

There are bugs, including potential data loss if something really goes wrong. That being said, I daily drive it and haven't had issues.

The flit server allows arbitrary command execution and filesystem operations over RPC with no authentication. I recommend only using the :et or :ssh connection methods, piggy backing on your existing authentication setup. The flit server is run in "oneshot" mode talking over stdio to Emacs. I am not a security expert, but this doesn't seem worse than TRAMP. Please open an issue if you have security advice.

There will almost certainly be breaking changes to the public interface.

## Architecture

flit.el connects to a flit server process on the remote host. flit.el establishes a bidirectional RPC connection using flitrpc, a custom binary protocol with msgpack-encoded metadata and raw binary payloads. Filesystem operations and process/PTY creation all happen over this connection.

To minimize round trips, flit.el aggressively caches file and directory information. The server watches for changes and pushes updates to flit.el.

## Connection Methods

Flit maintains a single persistent connection to the remote host. ET is highly recommended to minimize interruptions.

```elisp
;; /flit!somehost/ will connect to somehost
(setq flit-connection-methods '((t . :et)))
```

```elisp
;; /flit!dev/ will connect to somehost
(setq flit-connection-methods '(("dev" . (:et "somehost"))))
```

```elisp
;; /flit!dev/ will connect to somehost using "my-et somehost --command"
(setq flit-connection-methods '(("dev" . (lambda (host) `("my-et" ,host "--command")))))
```

For ET, Flit uses a single persistent PTY connection. Auth prompts are presented to the user. Flit will offer to automatically deploy the "flit" binary to the remote host if not already present. Flit uses a local "flit pty-bridge" sidecar to handle the PTY communication since Emacs has PTY performance issues.

Similarly :ssh can be used instead of :et. SSH mode will first try a non-PTY stdio SSH connection. On failure, :ssh will fall back to a PTY connection via the "flit pty-bridge" sidecar. This PTY fallback supports "flit" binary deployment and auth prompts.

Lower level methods :stdio and :tcp are also available, which do not support auth prompts or auto deploy. Finally, a function can be provided to yield an arbitrary Emacs process ready for the RPC connection.

## User Interface

To connect, use a file path `/flit!host/` or `/flit!host~`. The "flit" can be omitted (e.g. `/!host/`). Alternatively, use `M-x flit-connect` to manually connect to a host.

If you have cache issues, run `M-x flit-clear-cache`. Run `M-x flit-clear-all-state` to reset all state, including connections and cache.

See the current connection status via `M-x flit-status`. Manually disconnect a connection using `M-x flit-disconnect`.

## Tunnels

Flit can open forward or reverse TCP tunnels using `flit-tunnel-listen` or `flit-reverse-tunnel-listen`, respectively. These are useful in certain situations to proxy traffic transparently between the local and remote host.

For example, I use `flit-reverse-tunnel-listen` to allow Claude running on the remote host to connect to claude-code-ide.el listening in my local Emacs.

## Features and Compatibility

In general, compatibility is about the same as TRAMP. Packages that use TRAMP specific functions (e.g. `tramp-disect-file-name`) will not work since Flit paths are not TRAMP paths, by design.

Vterm and eat both basically work, but for eat you will need to change TERM or compile the eat terminfo entry manually on the remote host (pending https://codeberg.org/akib/emacs-eat/pulls/252/files). There are likely remaining terminal issues, so be vigilant.

LSP seems to work fine (I mainly use lsp-mode, but also tested eglot).

Desktop mode is supported specially for flit buffers. On startup, flit buffers are in a deferred state. Pressing "g" will attempt to connect to the remote host.

There are some Marginalia customizations to make certain things work better.

## Philosophy

- Support ET for reliable, persistent connections
- Don't be annoying with reconnection attempts
- Aggressively cache data and proactively push data from the server to avoid round trips
- Only attempt a connection for a direct user action on a flit buffer

## Why?

### What is wrong with TRAMP?

TRAMP works okay if you have passwordless auth, low latency, and a stable internet connection. Otherwise it is pretty unusable. And AFAIK you can't do LSP easily over EternalTerminal, which is disruptive when you have to reconnect.

### Why not just develop in Emacs on the remote host?

- Keystroke latency is annoying. Mosh helps, granted.
- Flaky internet makes remote Emacs very annoying.
- I don't want multiple Emacs when working on multiple hosts at once (including localhost).

## Problems with flit

The caching inevitably leads to stale data when something goes wrong. Use `M-x flit-clear-cache` when a file or directory is not up to date in Emacs.

Flit tries to prefetch and be async where possible, but some things will always hang for a bit doing serial filesystem access. Use `flit-run-after-connect` to eagerly prefetch data and avoid future serial fetches when you know certain files and/or directories will end up being needed.

File change detection and auto revert work to a certain degree, but there will be a long tail of bugs in this area.

The pty communication is versatile, but fragile. The flitrpc connection can get stuck with any unexpected output sneaking in on stdout.

## Debugging

Check the `*flit-log*` buffer for detailed goings on. Log messages from the flit server also show up here. Set `flit-log-level` to `'debug` or `'trace` to see more logs, optionally paired with `flit-log-filter` to zero in on a particular subject.

Logging from the "flit pty-bridge" sidecar shows up in the `*flit-stderr-HOST*` buffer.
