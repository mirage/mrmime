### v0.6.1 2024-04-04 Paris (France)

- Upgrade tests to be compatible with OCaml 5.2 (@kit-ty-kate, #99)

### v0.6.0 2023-03-01 Paris (France)

- Be able to set field parsers when we parse an entire email (@dinosaure, #89)
- Be able to wrap a multipart email into a part (@dinosaure, #90)
- Don't use RNG to produce the example to be compatible with OCaml 5 (@dinosaure, #93)
- Upgrade to cmdliner.1.1.0 (@hannesm, #94)
- Use a smaller email to test isomorphism (@dinosaure, 931c6ad)
- Lint dependencies (@dinosaure, #95)
- Delete `rresult` dependency (@dinosaure, @hannesm, #96)
- Delete `fmt` dependency (@dinosaure, #97)

### v0.5.0 2021-10-18 Paris (France)

- Replace deprecated functions of `fmt` library (@dinosaure, #86)
- Fix OPAM constraints (@kit-ty-kate, @dinosaure, #85)
- Clarify the semantic about the underlying quoted-printable encoder (@dinosaure, #84)
- Be able to pass the seed to generate the boundary (@dinosaure, #83)
- Let the user to decide if it wants to encode or not the given stream
  according to the `Content-Transfer-Encoding` (@dinosaure, #82)
- Add `Content_type.Type.of_string` (@dinosaure, #81)
- Add `Header.add_unless_exists` (@dinosaure, #81)
- Add a regression test about #77 (@lyrm, @dinosaure, #78)
- Fix bug on the Base64 decoder (@clecat, @lyrm, @dinosaure, #78)
- Don't try to split quoted-printable and add useless spaces to respect isomorphism (@dinosaure, @lyrm, #77)
- Fix how we escape `\b` in content-type parameters (@dinosaure, #73)
- Add `Header.{length,to_list,of_list,to_list_with_location}` (@dinosaure, #72)
- Enable `fws` token in quoted-string values (as a parameter of the content-type) (@lyrm, @dinosaure, #71)
- Handle correctly an empty part with `angstrom` (@dinosaure, #68)
- Generate a corpus of a million emails where we ensure that
  `mrmime` assumes a kind of _isomorphism_, see https://github.com/mirage/hamlet
  for more details (@lyrm, @dinosaure, #67)
- Fix some comparison functions (on IANA values and field-name) (@lyrm, @dinosaure, #66)
- Simplify the `Mail.t` type (@lyrm, @dinosaure, #63)
- Fix pattern-matching failure when decoder is closed (@dinosaure, #62)
- Keep the order of fields into a header when we replace one (@dinosaure, #61)
- Ensure stable memory with lwt when we parse contents (@dinosaure, #60)
- Fix how we parse quoted-printable contents (@dinosaure, #59)
- Add a space in front of any field's values (@dinosaure, #56)
- Ensure to emit only lines (@dinosaure, #55)
- Fix how we parse Content-Transfer-Encoding (@dinosaure, #54)
- Better pretty-print multiple domains on mailbox (@dinosaure, #53)
- Unlock the arbitrary limit on the internal buffer to decode the header (@dinosaure, #51)
- Delete unused phrases field value (@dinosaure, #50)
- Fix fuzzer (@dinosaure, #49)

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
