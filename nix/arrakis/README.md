# arrakis

NixOS 25.05 server. `/etc/nixos/configuration.nix` is a symlink to
`~/Projects/dotfiles/nix/arrakis/configuration.nix` **in arrakis's own clone**
of this repo, so changes only take effect once they're in that working copy.

## Music storage for aurora (SMB share)

Goal: move `aurora.local:~/Music` (Intel MacBook, low on disk) onto arrakis's
`/mnt/music` and use it from Logic and Transcribe.

### History

- 2026-10-17: added `services.samba` + an Avahi `_smb._tcp` service to
  `configuration.nix` on arrakis, ran `nixos-rebuild switch` and
  `smbpasswd -a craig`. smbd confirmed listening on 445.
- 2026-10-17: found arrakis's link stuck at **100 Mbps**. The NIC (Realtek
  RTL8125, 2.5 GbE) is fine and a brand-new cable didn't help; the culprit is
  the **TRENDnet TE100** switch it's plugged into (10/100 only). trantor
  (gigabit, not on that switch) measured only 10.9 MB/s to arrakis.
  A gigabit switch has been ordered.
- The work happened in a Claude Code session in `~/Projects/demucs-rs` on
  trantor (`cd ~/Projects/demucs-rs && claude --resume` to find it).

### Share config (in configuration.nix)

- Share `music` -> `/mnt/music` (ext4, 229 GB), writable, `valid users = craig`.
- `server min protocol = SMB3`; `hosts allow` limited to 192.168.88.0/24,
  the LAN's IPv6 ULA `fdf9:1a93:5ec1:425b::/64`, link-local and localhost.
- `vfs objects = catia fruit streams_xattr` + `fruit:*` options so Finder
  metadata, resource forks and `.logicx` packages survive the round trip.
- `openFirewall = true` (445/139 TCP, 137/138 UDP).
- Samba keeps its own password DB: `sudo smbpasswd -a craig` to (re)set it.

### When the new switch arrives

1. Swap the TE100 for the gigabit switch, then check the link:
   `cat /sys/class/net/enp1s0/speed` should print `1000` (not `100`).
   `ethtool enp1s0` (via `nix shell nixpkgs#ethtool`) shows what the switch
   port advertises under "Link partner advertised link modes".
2. Re-test throughput from trantor:
   `dd if=/dev/zero bs=1M count=200 | ssh arrakis.local 'cat > /dev/null'`
   (100 Mbps ≈ 11 MB/s; gigabit should be ~100 MB/s minus ssh overhead).
3. On aurora: Finder -> Cmd-K -> `smb://arrakis.local/music`, save the
   password in the keychain, and add the mounted share to
   System Settings -> General -> Login Items to remount at login.
4. Copy a large file over SMB from aurora to see real-world speed. aurora is on
   **Wi-Fi only** (5 GHz 802.11ac, ~405 Mbps link, en0 Ethernet unplugged), so
   expect roughly 20-35 MB/s regardless of the switch.
5. Move data with rsync, dry run first, and verify projects open in Logic
   before deleting anything on aurora.

### aurora ~/Music (as of 2026-10-17, 17.8 GB free on aurora)

| Folder | Size |
|---|---|
| `Logic` | 11 GB |
| `Music` (Apple Music library) | 946 MB |
| `Transcribe` | 337 MB |
| `GarageBand` | 80 MB |
| everything else | < 20 MB |

### Plan / guidance

- **Transcribe**: fine to run straight from the share (it loads the whole
  file into memory).
- **Logic**: use the share as an archive. Keep active projects local on
  aurora; move finished projects, old ideas and bounces to arrakis and copy a
  project back when working on it again. Recording or mixing large sessions
  directly over Wi-Fi SMB risks "Disk is too slow" errors and dropouts. That
  becomes realistic only with aurora on Ethernet. Apple doesn't officially
  support Logic projects on network volumes.

### Loose ends

- The Samba config is committed and pushed (`33daabb smb for arrakis`).
  arrakis itself can't reach GitHub over ssh (`Permission denied (publickey)`),
  so other clones (e.g. trantor's) need a `git pull` to see it.
- aurora's SSH key couldn't be used for non-interactive ssh to arrakis
  (probably a passphrase-protected key without an agent in that session).
