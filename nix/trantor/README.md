# trantor

NixOS configuration for the `trantor` workstation.

## Suspend debugging

If suspend misbehaves (won't suspend, hangs on resume, ghostty windows wedge after wake), the config includes hooks that capture state around every sleep cycle.

### What's installed

- `boot.kernelParams = [ "no_console_suspend" ]` — keeps the kernel console alive across suspend so any kernel-side hang leaves a trail.
- `systemd.services.suspend-debug` — runs `before = sleep.target`. Logs:
  - `systemd-inhibit --list` (who is holding sleep/idle locks)
  - all `ghostty` and `<defunct>` processes
  - `loginctl list-sessions`
- `systemd.services.resume-debug` — runs `after = suspend.target / hibernate.target`. Logs ghostty processes post-wake.

### Checking the logs

After a bad suspend, on the next boot:

```
journalctl -b -1 -u suspend-debug -u resume-debug
journalctl -b -1 -u systemd-suspend.service
journalctl -b -1 -k --grep 'suspend\|resume\|PM:'
```

If you catch it live (system woke but is misbehaving):

```
journalctl --since "10 min ago"
systemd-inhibit --list
```

### Confirming ghostty as the cause

While ghostty is open, `systemd-inhibit --list` will show any inhibitor it (or a child shell) is holding. If nothing shows up there but suspend still fails when ghostty is running, the issue is more likely the upstream EGL / GTK hang on exit rather than an inhibitor lock — see ghostty discussions [#4337](https://github.com/ghostty-org/ghostty/discussions/4337) and [#9190](https://github.com/ghostty-org/ghostty/discussions/9190).
