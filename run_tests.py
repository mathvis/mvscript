from os import listdir, system
import subprocess


def parse_diff_output(output: str) -> str:
    try:
        if output == "":
            return output
        lines: list[str] = output.splitlines()
        i = lines.index("---")
        return f"Expected:\n\t{"\n".join(list(map(lambda l: l.lstrip('< '), lines[1 + 2:])))}\nGot:\n\t{"\n".join(list(map(lambda l: l.lstrip('> '), lines[1:i])))}"
    except ValueError:
        return output


def run_test(name: str) -> bool:

    subprocess.run(
        ["cabal", "run", "mvscript", f"test/{name}/{name}.in", f"test/{name}/config.toml"],
        stdout=open(f"test/{name}/{name}.myout", "w"),
        stderr=open(f"test/{name}/{name}.myout", "a"),
    )

    result = parse_diff_output(
        subprocess.getoutput(
            f"diff ./test/{name}/{name}.myout ./test/{name}/{name}.out"
        )
    )
    if result == "":
        print("TEST PASSED!")
        return True
    print(f"TEST {name} FAILED!\n{result}")
    return False


def run_tests():
    system("cabal build > /dev/null")
    files = listdir("./test")
    count: int = 0
    for file in files:
        if run_test(file):
            count += 1
    if count != len(files):
        print(f"{count}/{len(files)} TESTS PASSED!")
        exit(1)
    print(f"{count}/{len(files)} TESTS PASSED!")
    exit(0)


def main():
    run_tests()


if __name__ == "__main__":
    main()
