# RCS Python3

### Rubik’s Cube Solver (Old Pochmann)

---

---

## Virtual Environment

```shell
python3 -m venv venv
. venv/bin/activate
```

---

## Requirements

```sh
pip3 install -r requirements.txt
```

---

## Style and Linting

```sh
python3 -m black .
```

```sh
python3 -m flake8 .
```

---

## Run / Example

```sh
python3 main.py
```

```python
import main as rcs_main

test_cube, _ = rcs_main.create_random_scrambled_cube()

print(f"\nScrambled Cube:\n\n{str(test_cube)}\n")
print(f"Solution: {rcs_main.solve_old_pochmann(test_cube)[0]}\n")
print(f"\nSolved Cube:\n\n{str(test_cube)}\n")
```
