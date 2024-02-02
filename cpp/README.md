# RCS C++

### Rubik’s Cube Solver (Old Pochmann)

---

Work in progress!

---

## Style and Linting

You can check the style of the code by running `make checkstyle`.
Internally, this runs googles `cpplint` https://github.com/google/styleguide/tree/gh-pages/cpplint.

install `cpplint` using `pip3`

```shell
pip3 install cpplint
```

---

### Tests

Install `gtest` using


#### `nix`

```shell
nix-shell -p gtest
```

#### `apt`

```shell
apt install libgtest-dev
```

---

## Building

To build the project, run `make`.
