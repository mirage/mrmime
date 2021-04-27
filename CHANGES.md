### v0.4.0 2021-04-27 Paris (France)

- Return the zone of the date (#41, @dinosaure)
- Add `Content_type.to_string` (#43, @dinosaure)
- Be resilient on date (accept nano-seconds) (#44, @dinosaure)
- Add a simple example to craft an email with an attachment (#47, @dinosaure)
- Drop the support of OCaml 4.08.0 (#47, @dinosaure)
- `mrmime.prettym` is not a part of the distribution
  **breaking changes**
  We decided to split `mrmime` and `prettym` mostly because this
  part (`prettym`) is used by some others packages and they should
  not follow the release cycle of `mrmime` when they are not about
  email stuffs. The package is available here:

  https://github.com/dinosaure/prettym

### v0.3.2 2020-11-26 Paris (France)

- Add `Header.message_id` (#39, @dinosaure, @hannesm)
- Update README.md (#40, @dinosaure)
- Update the way to generate quoted-printable contents
  According `pecu.0.5` (#38, @dinosaure)

### v0.3.1 2020-09-24 Paris (France)

- Fix unstructured values (@dinosaure, #34)
- Fix `Mrmime.Hd` decoder about the separation between the header
  and the body (@dinosaure, #35)
- ocamlformat.0.15.0 pass (@dinosaure, #36)
- Use emile.1.0 (@dinosaure)

### v0.3.0 2020-05-11 Paris (France)

- Fix OPAM file, add `dune` constraint (@kit-ty-kate)
- Delete dead-code (@seliopou, #29)
- Move to `angstrom.0.14.0` (@dinosaure, #30)
- Use `bigarray-overlap.0.2.0` (@dinosaure, #31)

### v0.2.0 2020-03-15 Paris (France)

- New lightweight version of `mrmime`
- Use `unstrctrd`
- Add some accessors/converters

### v0.1.0 2019-07-26 Сарајево (Боснa и Херцеговина)

- First release
