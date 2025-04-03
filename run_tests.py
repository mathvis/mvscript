from colorama import init as colorama_init
from colorama import Fore
from os import listdir, system
import subprocess

colorama_init()


def parse_diff_output(output: str) -> str:
    if output == "":
        return output
    lines: list[str] = output.splitlines()
    print(lines)
    i = lines.index("---")
    return f"Expected:\n\t{"\n".join(list(map(lambda l: l.lstrip('< '), lines[1:i])))}\nGot:\n\t{"\n".join(list(map(lambda l: l.lstrip('> '), lines[i + 1 :])))}"


def run_test(filename: str) -> bool:
    name: str = filename.rstrip(".in")

    system(f'cabal run mvscript "test/{filename}" | tail -n +2 > test/{name}.myout')
    print(listdir("./test"))

    result = parse_diff_output(
        subprocess.getoutput(f"diff ./test/test01.myout ./test/test01.out")
    )
    if result == "":
        print(f"{Fore.GREEN}TEST PASSED!")
        return True
    print(f"\033[1m{Fore.RED}TEST {name} FAILED!\033[0m\n{result}")
    return False


def run_tests():
    system("cabal build > /dev/null")
    files = list(filter(lambda x: x.endswith(".in"), listdir("./test")))
    count: int = 0
    for file in files:
        if run_test(file):
            count += 1
    if count != len(files):
        print(f"\033[1m{Fore.RED}{count}/{len(files)} TESTS PASSED!\033[0m")
        exit(1)
    print(f"\033[1m{Fore.GREEN}{count}/{len(files)} TESTS PASSED!\033[0m")
    exit(0)


def main():
    run_tests()


if __name__ == "__main__":
    main()
