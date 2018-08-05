let
  fetchNixpkgs = import ./fetchNixpkgs.nix;
in

# version - 18.03

fetchNixpkgs {
  rev          = "949bddfae38a613a0e8b0931e48ea5d843c1cf71";
  sha256       = "1xlpl4hnw1hybd9q36av7xjjdp5igam498w56hnwvfi603aih13r";
  outputSha256 = "14lbj6qdgga548k7x610an34c91204dmhcz0c5lc9viry184x0l7";
}
